source(here::here("R", "config.R"))
source(here::here("R", "recipes.R"))

report_object <- function(properties, description = NULL) {
  list(
    type = "object", description = description, properties = properties,
    required = names(properties), additionalProperties = FALSE
  )
}

insight_schema <- function() {
  text <- list(type = "string", minLength = 1L)
  date <- list(type = "string", format = "date")
  period <- report_object(list(start = date, end = date))
  count <- list(type = "integer", minimum = 0L)
  share <- list(type = "number", minimum = 0, maximum = 1)
  report_object(list(
    title = text,
    finding = text,
    suggested_action = text,
    data_as_of = date,
    reporting_period = period,
    evidence_window = period,
    outcome_horizon = list(type = "string", enum = c("snapshot", "30_days")),
    metric_definition = text,
    evidence = list(
      type = "array", minItems = 1L,
      items = report_object(list(
        cohort_month = date, leads = count, won = count,
        conversion = share, share_of_group_observed = share
      ))
    ),
    caveat = text,
    reproducible_code = text
  ), "One monthly cohort insight, with its time contract and reproducible evidence.")
}

insight_submission_type <- function() {
  ellmer::type_from_schema(canonical_report_json(insight_schema()))
}

# Only the JSON-schema vocabulary used above is needed for this worked example.
validate_report_value <- function(value, schema, path = "insight") {
  fail <- function(reason) stop(path, ": ", reason, call. = FALSE)
  switch(schema$type,
    object = {
      if (!is.list(value) || is.null(names(value)) ||
          anyDuplicated(names(value)) ||
          !setequal(names(value), names(schema$properties))) {
        fail("expected exactly the declared object fields")
      }
      for (name in names(schema$properties)) {
        validate_report_value(value[[name]], schema$properties[[name]], paste0(path, ".", name))
      }
    },
    array = {
      if (!is.list(value) || !is.null(names(value)) ||
          length(value) < schema$minItems) {
        fail("expected an array with sufficient entries")
      }
      for (i in seq_along(value)) {
        validate_report_value(value[[i]], schema$items, paste0(path, "[", i, "]"))
      }
    },
    string = {
      if (!is.character(value) || length(value) != 1L || is.na(value) ||
          !nzchar(trimws(value))) fail("expected one non-empty string")
      if (!is.null(schema$enum) && !value %in% schema$enum) fail("unsupported value")
      if (identical(schema$format, "date")) {
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", value)) fail("expected an ISO date")
        parsed <- as.Date(value, format = "%Y-%m-%d")
        if (is.na(parsed) || format(parsed, "%Y-%m-%d") != value) fail("invalid date")
      }
    },
    integer = ,
    number = {
      if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
        fail("expected one finite number")
      }
      if (schema$type == "integer" && value != floor(value)) fail("expected an integer")
      if (!is.null(schema$minimum) && value < schema$minimum) fail("below minimum")
      if (!is.null(schema$maximum) && value > schema$maximum) fail("above maximum")
    },
    fail("unsupported schema type")
  )
  invisible(value)
}

canonical_report_json <- function(value) {
  jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
}

report_hash <- function(value) {
  digest::digest(canonical_report_json(value), algo = "sha256", serialize = FALSE)
}

report_snapshot_id <- function(snapshot) {
  require_snapshot(snapshot)
  if ("won_eventually" %in% names(snapshot)) {
    stop("Report inputs must not contain hindsight.", call. = FALSE)
  }
  if (!"lead_id" %in% names(snapshot) || anyNA(snapshot$lead_id) ||
      anyDuplicated(snapshot$lead_id)) {
    stop("Report inputs need unique, non-missing lead IDs.", call. = FALSE)
  }
  report_hash(snapshot[order(snapshot$lead_id), sort(names(snapshot))])
}

worked_time_contract <- function(snapshot) {
  cutoff <- require_snapshot(snapshot)
  current_month <- as.Date(format(cutoff, "%Y-%m-01"))
  months <- sort(seq(current_month, by = "-1 month", length.out = 5L)[-1L])
  list(
    data_as_of = as.character(cutoff),
    reporting_period = list(start = as.character(cutoff - 27), end = as.character(cutoff)),
    evidence_window = list(start = as.character(months[1]), end = as.character(current_month - 1)),
    months = months
  )
}

# The snippet the email tells the reader to run.
#
# It has to work for somebody who has the data and none of this repo. The
# earlier version called cohort_conversion() and snapshot_cutoff(), which live
# in R/recipes.R -- so "reproduce the evidence" meant "clone our repository
# first", which is not reproducibility, it is an advertisement.
#
# So the snippet now loads its own libraries, reads the snapshot from a pin the
# reader points at, and does the arithmetic in plain dplyr. Spelling the metric
# out longhand is a side benefit: the definition stops being something you have
# to take on trust from a function name.
#
# Split into a preamble and a pipeline because the pipeline is executed in
# tests/testthat/test-report-contract.R and checked against the evidence it
# claims to produce. The preamble cannot be executed -- it points at a board
# that does not exist -- and an untested snippet in an email headed "reproduce
# the evidence" is exactly the kind of claim this repo is not supposed to make.
worked_evidence_preamble <- function() {
  paste(
    "# Reproduce the table above from the same frozen snapshot.",
    "# Point these two lines at your own board and pin, then run the rest as is.",
    "library(dplyr)",
    "library(pins)",
    "",
    "board <- board_folder(\"path/to/your/board\")",
    "snapshot <- pin_read(board, \"funnel-snapshot\")",
    "",
    "",
    sep = "\n"
  )
}

worked_evidence_pipeline <- function(months, outcome_horizon) {
  window <- paste0(
    "snapshot |>\n",
    "  filter(\n",
    "    cohort_month >= as.Date(\"", months[1], "\"),\n",
    "    cohort_month <= as.Date(\"", months[4], "\")\n",
    "  ) |>\n",
    "  group_by(cohort_month) |>\n"
  )

  switch(outcome_horizon,
    snapshot = paste0(
      window,
      "  summarise(\n",
      "    leads = n(),\n",
      "    won = sum(won_as_of),\n",
      "    conversion = mean(won_as_of),\n",
      "    share_of_group_observed = 1,\n",
      "    .groups = \"drop\"\n",
      "  )"
    ),
    `30_days` = paste0(
      window,
      "  summarise(\n",
      "    cohort_leads = n(),\n",
      "    leads = sum(observation_age_days >= 30),\n",
      "    won = sum(\n",
      "      observation_age_days >= 30 & won_as_of &\n",
      "        !is.na(days_to_won) & days_to_won <= 30\n",
      "    ),\n",
      "    .groups = \"drop\"\n",
      "  ) |>\n",
      "  mutate(\n",
      "    conversion = won / leads,\n",
      "    share_of_group_observed = leads / cohort_leads\n",
      "  ) |>\n",
      "  select(cohort_month, leads, won, conversion, share_of_group_observed)"
    ),
    stop("Unsupported outcome horizon.", call. = FALSE)
  )
}

worked_evidence_code <- function(snapshot, outcome_horizon) {
  contract <- worked_time_contract(snapshot)
  paste0(
    worked_evidence_preamble(),
    worked_evidence_pipeline(contract$months, outcome_horizon)
  )
}

worked_evidence <- function(snapshot, outcome_horizon) {
  report_snapshot_id(snapshot)
  contract <- worked_time_contract(snapshot)
  days <- switch(outcome_horizon, snapshot = NULL, `30_days` = 30,
    stop("Unsupported outcome horizon.", call. = FALSE))
  result <- cohort_conversion(snapshot, within_days = days, as_of = as.Date(contract$data_as_of)) |>
    dplyr::filter(cohort_month %in% contract$months)
  if (!identical(result$cohort_month, contract$months)) {
    stop("The worked example requires four complete entry cohorts.", call. = FALSE)
  }
  if (outcome_horizon == "snapshot") result$share_of_group_observed <- 1
  if (any(result$share_of_group_observed != 1)) {
    stop("The whole cohort must be observed for this comparison.", call. = FALSE)
  }
  lapply(seq_len(nrow(result)), function(i) list(
    cohort_month = as.character(result$cohort_month[i]),
    leads = result$leads[i], won = result$won[i],
    conversion = result$conversion[i],
    share_of_group_observed = result$share_of_group_observed[i]
  ))
}

validate_insight <- function(insight, snapshot) {
  validate_report_value(insight, insight_schema())
  contract <- worked_time_contract(snapshot)
  for (field in c("data_as_of", "reporting_period", "evidence_window")) {
    actual <- insight[[field]]
    if (is.list(actual)) actual <- actual[names(contract[[field]])]
    if (!identical(actual, contract[[field]])) {
      stop("Inconsistent ", field, ".", call. = FALSE)
    }
  }
  expected <- worked_evidence(snapshot, insight$outcome_horizon)
  actual <- lapply(insight$evidence, function(row) row[names(expected[[1]])])
  if (!isTRUE(all.equal(actual, expected, tolerance = 1e-12))) {
    stop("Reported evidence does not reproduce from the snapshot.", call. = FALSE)
  }
  # Do not evaluate submitted code. This fixture path accepts the known recipe
  # expression only; a general REPL execution boundary belongs to the live branch.
  if (!identical(insight$reproducible_code, worked_evidence_code(snapshot, insight$outcome_horizon))) {
    stop("Reproducible code must match the worked recipe expression.", call. = FALSE)
  }
  invisible(insight)
}

submit_insight_tool <- function(snapshot, on_submit) {
  report_snapshot_id(snapshot)
  if (!is.function(on_submit)) stop("on_submit must be a function.", call. = FALSE)
  ellmer::tool(
    fun = function(insight) {
      validate_insight(insight, snapshot)
      on_submit(insight)
      "Insight submitted for review, not publication."
    },
    description = "Submit one monthly cohort insight with reproducible snapshot evidence.",
    arguments = list(insight = insight_submission_type()),
    name = "submit_insight"
  )
}

new_example_report <- function(insight, snapshot, report_id, purpose) {
  validate_insight(insight, snapshot)
  result <- list(
    schema_version = 1L, report_id = report_id, purpose = purpose,
    company = INBOX_COMPANY, snapshot_id = report_snapshot_id(snapshot),
    provenance = list(
      kind = "authored_fixture", author = "Copilot",
      note = "Authored worked example, not a captured live model run."
    ),
    archive_id = NULL, applied_rule_ids = list(), insight = insight
  )
  validate_example_report(result, snapshot)
  result
}

validate_example_report <- function(report, snapshot) {
  fields <- c("schema_version", "report_id", "purpose", "company", "snapshot_id",
    "provenance", "archive_id", "applied_rule_ids", "insight")
  if (!is.list(report) || anyDuplicated(names(report)) ||
      !setequal(names(report), fields)) {
    stop("Invalid report envelope fields.", call. = FALSE)
  }
  text <- list(type = "string")
  validate_report_value(report$report_id, text, "report_id")
  validate_report_value(report$purpose, list(type = "string",
    enum = c("teaching_example", "corrected_preview")), "purpose")
  validate_report_value(report$provenance, report_object(list(
    kind = list(type = "string", enum = "authored_fixture"), author = text, note = text
  )), "provenance")
  validate_report_value(report$schema_version, list(
    type = "integer", minimum = 1L, maximum = 1L
  ), "schema_version")
  if (!identical(report$company, INBOX_COMPANY) ||
      !identical(report$snapshot_id, report_snapshot_id(snapshot))) {
    stop("Report version, company, or snapshot identity does not match.", call. = FALSE)
  }
  if (!is.null(report$archive_id)) validate_report_value(report$archive_id, text, "archive_id")
  validate_report_value(report$applied_rule_ids, list(
    type = "array", minItems = 0L, items = text
  ), "applied_rule_ids")
  validate_insight(report$insight, snapshot)
  expected_purpose <- if (report$insight$outcome_horizon == "snapshot") {
    "teaching_example"
  } else {
    "corrected_preview"
  }
  if (report$purpose != expected_purpose) {
    stop("Report purpose does not match its outcome horizon.", call. = FALSE)
  }
  invisible(report)
}

write_example_report <- function(report, snapshot, path) {
  validate_example_report(report, snapshot)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(report, path, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE)
  invisible(path)
}

read_example_report <- function(path, snapshot) {
  report <- jsonlite::read_json(path, simplifyVector = FALSE)
  validate_example_report(report, snapshot)
  report
}
