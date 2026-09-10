source(here::here("R", "report_contract.R"))

# The live branch: reports written by a model rather than authored.
#
# The authored path in report_contract.R validates an insight by recomputing its
# evidence with a reproducer this repository already owns. That is what makes
# the fixtures trustworthy, and it is exactly what a live agent cannot satisfy:
# a genuinely new finding has no pre-registered reproducer.
#
# So the live branch strikes the same bargain as the reference implementation.
# The agent computes its evidence in the sandboxed REPL and saves it there as a
# data frame; the runner reads that file back, checks its shape, and takes the
# code the agent reports as its account of how the table was produced. Nothing
# is re-executed. The checks here are the cheap, token-free kind -- a well-formed
# table, a schema, code that at least parses -- and the report says plainly that
# a model wrote it and nobody has reviewed it yet. Whether the question was worth
# asking, and whether the code really produces the table, is what the reviewer
# is for.

LIVE_REPORT_PURPOSE <- "weekly_email_live"

# An email table has to be readable. Past this the agent should aggregate.
LIVE_EVIDENCE_MAX_ROWS <- 40L

# Value columns beside the labels. Four columns fit the email's width; a fifth
# runs out of the sheet on the right.
LIVE_EVIDENCE_MAX_COLUMNS <- 3L

# How a model's numbers read in the email and the apps. The model names its
# columns, and the name and the values decide the format: money in dollars,
# percentages and shares to two decimals, whole numbers whole, anything else to
# two decimals. Applied per column, so a column is formatted one way throughout.
format_evidence_values <- function(column, values) {
  values <- as.numeric(values)
  words <- strsplit(tolower(gsub("[^A-Za-z0-9]+", " ", column)), " ", fixed = TRUE)[[1]]
  has <- function(...) any(c(...) %in% words)
  whole <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  if (has("value", "amount", "revenue", "arr", "usd", "dollar", "dollars", "price", "cost", "spend", "budget")) {
    return(paste0("$", whole(values)))
  }
  if (has("pct", "percent", "percentage")) return(sprintf("%.2f%%", values))
  if (has("share", "rate", "conversion", "proportion")) {
    return(sprintf("%.2f%%", if (all(abs(values) <= 1)) 100 * values else values))
  }
  if (all(values == round(values))) return(whole(values))
  format(round(values, 2), nsmall = 2, big.mark = ",", scientific = FALSE, trim = TRUE)
}

# The evidence, column by column, as strings ready to show.
format_evidence_table <- function(evidence) {
  columns <- unlist(evidence$value_columns)
  lapply(seq_along(columns), function(j) {
    format_evidence_values(columns[[j]], vapply(evidence$rows, function(row) as.numeric(row$values[[j]]), numeric(1)))
  })
}

# Evidence is a small labelled table: one label column and any number of numeric
# columns. General enough for a finding nobody anticipated, typed enough to
# render without guessing at formatting.
live_insight_schema <- function() {
  text <- list(type = "string", minLength = 1L)
  report_object(list(
    title = text,
    finding = text,
    suggested_action = text,
    metric_definition = text,
    caveat = text,
    evidence = report_object(list(
      label_column = text,
      value_columns = list(type = "array", minItems = 1L, items = text),
      rows = list(
        type = "array", minItems = 1L,
        items = report_object(list(
          label = text,
          values = list(type = "array", minItems = 1L, items = list(type = "number"))
        ))
      )
    )),
    reproducible_code = text
  ), "One insight, with a labelled evidence table and the code that produced it.")
}

# --- Evidence the agent saved from the REPL ----------------------------------

# The agent hands in a file name, never a path. Anything with a directory in it
# could point the runner at a file the sandbox was never allowed to write.
live_evidence_path <- function(scratch, file) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file) ||
      !identical(basename(file), file) || file %in% c(".", "..")) {
    stop(
      "evidence_file must be a plain file name in the working directory, not a path.",
      call. = FALSE
    )
  }
  path <- file.path(scratch, file)
  if (!file.exists(path)) {
    stop(
      "No file named ", file, " in the working directory. ",
      "Save the table with saveRDS() first.",
      call. = FALSE
    )
  }
  path
}

# One data frame: the first column labels the rows, every other column is a
# number. The errors are written for the model, which is who reads them.
live_evidence_from_frame <- function(frame) {
  if (!is.data.frame(frame)) {
    stop(
      "The evidence file must contain a data frame, not ", class(frame)[1], ".",
      call. = FALSE
    )
  }
  if (nrow(frame) < 1L) stop("The evidence table is empty.", call. = FALSE)
  if (nrow(frame) > LIVE_EVIDENCE_MAX_ROWS) {
    stop(
      "The evidence table has ", nrow(frame), " rows; at most ",
      LIVE_EVIDENCE_MAX_ROWS, " fit in an email. Aggregate further.",
      call. = FALSE
    )
  }
  if (ncol(frame) < 2L) {
    stop(
      "The evidence table needs a label column and at least one numeric column.",
      call. = FALSE
    )
  }
  if (ncol(frame) - 1L > LIVE_EVIDENCE_MAX_COLUMNS) {
    stop(
      "The evidence table has ", ncol(frame) - 1L, " value columns; at most ",
      LIVE_EVIDENCE_MAX_COLUMNS, " fit beside the labels in an email. Keep the",
      " columns that carry the finding and drop the rest.",
      call. = FALSE
    )
  }

  labels <- as.character(frame[[1]])
  if (anyNA(labels) || !all(nzchar(trimws(labels)))) {
    stop("Every row needs a label in the first column.", call. = FALSE)
  }
  values <- frame[-1]
  for (name in names(values)) {
    column <- values[[name]]
    if (!is.numeric(column) || !all(is.finite(column))) {
      stop("Column ", name, " must be numeric with no missing values.", call. = FALSE)
    }
  }

  list(
    label_column = names(frame)[1],
    value_columns = as.list(names(values)),
    rows = lapply(seq_len(nrow(frame)), function(i) list(
      label = labels[[i]],
      values = unname(lapply(values, function(column) as.numeric(column[[i]])))
    ))
  )
}

read_live_evidence <- function(scratch, file) {
  path <- live_evidence_path(scratch, file)
  frame <- tryCatch(readRDS(path), error = function(e) {
    stop(file, " could not be read as an RDS file. Save it with saveRDS().", call. = FALSE)
  })
  live_evidence_from_frame(frame)
}

# --- Validation ---------------------------------------------------------------

validate_live_insight <- function(insight) {
  validate_report_value(insight, live_insight_schema())

  # The row cap is checked here as well as when the RDS is read, so an insight
  # that arrives already serialised is held to the same size.
  if (length(insight$evidence$rows) > LIVE_EVIDENCE_MAX_ROWS) {
    stop(
      "Evidence has ", length(insight$evidence$rows), " rows; the limit is ",
      LIVE_EVIDENCE_MAX_ROWS, ". Aggregate before submitting.",
      call. = FALSE
    )
  }
  if (length(insight$evidence$value_columns) > LIVE_EVIDENCE_MAX_COLUMNS) {
    stop(
      "Evidence has ", length(insight$evidence$value_columns), " value columns; the limit is ",
      LIVE_EVIDENCE_MAX_COLUMNS, ". Drop the columns that do not carry the finding.",
      call. = FALSE
    )
  }

  width <- length(insight$evidence$value_columns)
  for (i in seq_along(insight$evidence$rows)) {
    if (length(insight$evidence$rows[[i]]$values) != width) {
      stop(
        "Evidence row ", i, " has ", length(insight$evidence$rows[[i]]$values),
        " values but there are ", width, " value columns.",
        call. = FALSE
      )
    }
  }

  # Parsing is not running. It catches code R would refuse outright, so the
  # email never shows a snippet that cannot be what produced the table, and it
  # establishes nothing beyond that.
  parsed <- tryCatch(
    parse(text = insight$reproducible_code, keep.source = FALSE),
    error = function(e) {
      stop("reproducible_code is not valid R: ", conditionMessage(e), call. = FALSE)
    }
  )
  if (!length(parsed)) stop("reproducible_code contains no code.", call. = FALSE)
  invisible(insight)
}

# --- The live report envelope -----------------------------------------------

validate_live_report <- function(report, snapshot) {
  fields <- c("schema_version", "report_id", "purpose", "company", "snapshot_id",
    "provenance", "headline_metrics", "insights")
  if (!is.list(report) || anyDuplicated(names(report)) ||
      !setequal(names(report), fields)) {
    stop("Invalid live report envelope fields.", call. = FALSE)
  }
  if (!identical(as.integer(report$schema_version), 1L)) {
    stop("Unsupported live report schema_version.", call. = FALSE)
  }
  validate_report_value(report$report_id, list(
    type = "string", minLength = 1L
  ), "report_id")
  validate_report_value(report$purpose, list(
    type = "string", enum = LIVE_REPORT_PURPOSE
  ), "purpose")

  # Provenance says plainly that a model wrote this. An authored fixture and a
  # captured model run must never be mistaken for one another, in either
  # direction.
  validate_report_value(report$provenance, report_object(list(
    kind = list(type = "string", enum = "live_model_run"),
    provider = list(type = "string", minLength = 1L),
    model = list(type = "string", minLength = 1L),
    generated_at = list(type = "string", minLength = 1L),
    note = list(type = "string", minLength = 1L),
    # What the run cost, in the units every provider reports. Cached input
    # counts as input; it was sent and billed.
    tokens = report_object(list(
      input = list(type = "number"),
      output = list(type = "number")
    ))
  )), "provenance")

  if (!identical(report$company, INBOX_COMPANY) ||
      !identical(report$snapshot_id, report_snapshot_id(snapshot))) {
    stop("Report company or snapshot identity does not match.", call. = FALSE)
  }

  # The headline strip is arithmetic, so it is held to the authored standard:
  # it must reproduce from the snapshot exactly, model or no model.
  validate_report_value(
    report$headline_metrics, headline_metrics_schema(), "headline_metrics"
  )
  expected <- headline_metrics(snapshot)
  actual <- lapply(report$headline_metrics, function(row) row[names(expected[[1]])])
  if (!isTRUE(all.equal(actual, expected, tolerance = 1e-12))) {
    stop("Headline metrics do not reproduce from the snapshot.", call. = FALSE)
  }

  if (!is.list(report$insights) || !length(report$insights)) {
    stop("A live report needs at least one insight.", call. = FALSE)
  }
  for (insight in report$insights) validate_live_insight(insight)
  invisible(report)
}

# `usage` is chat$get_tokens(): one row per assistant turn with input, output
# and, for providers that report it, cached_input columns. Summed into the two
# numbers the footer shows.
live_token_usage <- function(usage) {
  if (is.null(usage) || !nrow(usage)) return(list(input = 0, output = 0))
  cached <- if ("cached_input" %in% names(usage)) usage$cached_input else 0
  list(
    input = sum(usage$input, na.rm = TRUE) + sum(cached, na.rm = TRUE),
    output = sum(usage$output, na.rm = TRUE)
  )
}

new_live_report <- function(insights, snapshot, provider, model,
                            tokens = list(input = 0, output = 0),
                            report_id = "weekly-report-live") {
  result <- list(
    schema_version = 1L,
    report_id = report_id,
    purpose = LIVE_REPORT_PURPOSE,
    company = INBOX_COMPANY,
    snapshot_id = report_snapshot_id(snapshot),
    provenance = list(
      kind = "live_model_run",
      provider = provider,
      model = model,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      note = paste(
        "Written by a model from the frozen snapshot. Each evidence table is",
        "the data frame the model saved from its own sandboxed R session, shown",
        "as submitted; the code beside it is the model's account and was not",
        "re-run here. Submitted for review, not reviewed."
      ),
      tokens = tokens
    ),
    headline_metrics = headline_metrics(snapshot),
    insights = insights
  )
  validate_live_report(result, snapshot)
  result
}

write_live_report <- function(report, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(report, path, auto_unbox = TRUE, null = "null",
    digits = NA, pretty = TRUE)
  invisible(path)
}
