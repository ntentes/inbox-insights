source(here::here("R", "report_contract.R"))

GUIDANCE_PIN <- "demo-guidance"

inbox_board <- function(path = inbox_board_path()) {
  pins::board_folder(path, versioned = TRUE)
}

approval_time <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

validate_approval_time <- function(value) {
  validate_report_value(value, list(type = "string"), "approval timestamp")
  parsed <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (is.na(parsed) || format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") != value) {
    stop("Approval timestamps must be real UTC times with second precision.", call. = FALSE)
  }
  invisible(value)
}

incomplete_cohort_rule <- function() {
  list(
    rule_id = "incomplete-cohort-30-days-v1",
    text = paste(
      "Compare monthly entry-to-won conversion over the same 30-day horizon:",
      "include only leads observed for at least 30 days and count only wins",
      "within 30 days of entry. Show the denominator and observed share.",
      "If a whole monthly cohort is not yet observed for 30 days, label it",
      "incomplete and omit its full-cohort comparison; do not substitute",
      "only its oldest leads. Hindsight is not report evidence."
    ),
    outcome_horizon = "30_days"
  )
}

feedback_schema <- function() {
  text <- list(type = "string")
  report_object(list(
    feedback_id = text, source_report_id = text, source_report_hash = text,
    text = text, author = text, created_at = text
  ))
}

approved_rule_schema <- function() {
  text <- list(type = "string")
  report_object(list(
    rule_id = text, text = text, outcome_horizon = text,
    source_feedback_id = text, approved_by = text, approved_at = text
  ))
}

validate_guidance <- function(state, snapshot) {
  fields <- c("schema_version", "snapshot_id", "source_report", "feedback", "rules")
  if (!is.list(state) || anyDuplicated(names(state)) || !setequal(names(state), fields)) {
    stop("Invalid guidance state fields.", call. = FALSE)
  }
  validate_report_value(state$schema_version,
    list(type = "integer", minimum = 1L, maximum = 1L), "guidance version")
  if (!identical(state$snapshot_id, report_snapshot_id(snapshot))) {
    stop("Guidance belongs to a different snapshot.", call. = FALSE)
  }
  validate_example_report(state$source_report, snapshot)
  validate_report_value(state$feedback, list(
    type = "array", minItems = 0L, items = feedback_schema()
  ), "feedback")
  validate_report_value(state$rules, list(
    type = "array", minItems = 0L, items = approved_rule_schema()
  ), "rules")
  feedback_ids <- vapply(state$feedback, `[[`, character(1), "feedback_id")
  if (anyDuplicated(feedback_ids) || length(state$rules) > 1L) {
    stop("Duplicate feedback or multiple worked rules in guidance.", call. = FALSE)
  }
  for (entry in state$feedback) {
    validate_approval_time(entry$created_at)
    expected_id <- paste0("feedback-", report_hash(entry[setdiff(names(entry), "feedback_id")]))
    if (!identical(entry$feedback_id, expected_id) ||
        !identical(entry$source_report_id, state$source_report$report_id) ||
        !identical(entry$source_report_hash, report_hash(state$source_report))) {
      stop("Feedback does not match its recorded source report.", call. = FALSE)
    }
  }
  for (rule in state$rules) {
    validate_approval_time(rule$approved_at)
    expected <- incomplete_cohort_rule()
    if (!identical(rule[names(expected)], expected)) {
      stop("This slice supports only the canonical incomplete-cohort rule.", call. = FALSE)
    }
    source_index <- match(rule$source_feedback_id, feedback_ids)
    if (is.na(source_index)) stop("Rule source feedback is missing.", call. = FALSE)
    if (rule$approved_at < state$feedback[[source_index]]$created_at) {
      stop("Approval cannot precede its source feedback.", call. = FALSE)
    }
  }
  invisible(state)
}

write_guidance <- function(board, state, snapshot) {
  validate_guidance(state, snapshot)
  pins::pin_write(board, state, name = GUIDANCE_PIN, type = "rds")
  invisible(state)
}

read_guidance <- function(board, snapshot) {
  if (!pins::pin_exists(board, GUIDANCE_PIN)) {
    stop("No guidance state: initialize it explicitly before use.", call. = FALSE)
  }
  state <- pins::pin_read(board, GUIDANCE_PIN)
  validate_guidance(state, snapshot)
  state
}

initialize_guidance <- function(board, source_report, snapshot) {
  validate_example_report(source_report, snapshot)
  if (pins::pin_exists(board, GUIDANCE_PIN)) {
    state <- read_guidance(board, snapshot)
    if (!identical(report_hash(state$source_report), report_hash(source_report))) {
      stop("Existing guidance has a different source report; it will not be overwritten.", call. = FALSE)
    }
    return(invisible(state))
  }
  state <- list(
    schema_version = 1L, snapshot_id = report_snapshot_id(snapshot),
    source_report = source_report, feedback = list(), rules = list()
  )
  write_guidance(board, state, snapshot)
}

record_feedback <- function(board, snapshot, text, author, created_at = approval_time()) {
  validate_report_value(text, list(type = "string"), "feedback text")
  validate_report_value(author, list(type = "string"), "feedback author")
  validate_approval_time(created_at)
  state <- read_guidance(board, snapshot)
  entry <- list(
    source_report_id = state$source_report$report_id,
    source_report_hash = report_hash(state$source_report),
    text = text, author = author, created_at = created_at
  )
  entry <- c(list(feedback_id = paste0("feedback-", report_hash(entry))), entry)
  ids <- vapply(state$feedback, `[[`, character(1), "feedback_id")
  if (entry$feedback_id %in% ids) stop("This feedback has already been recorded.", call. = FALSE)
  state$feedback <- append(state$feedback, list(entry))
  write_guidance(board, state, snapshot)
  entry
}

approve_incomplete_cohort_rule <- function(board, snapshot, feedback_id, approver,
                                         expected_state_id, approved_at = approval_time()) {
  validate_report_value(approver, list(type = "string"), "approver")
  validate_approval_time(approved_at)
  state <- read_guidance(board, snapshot)
  if (!identical(report_hash(state), expected_state_id)) {
    stop("Guidance changed since it was displayed; review it again before approval.", call. = FALSE)
  }
  if (length(state$rules)) stop("The worked rule is already approved.", call. = FALSE)
  ids <- vapply(state$feedback, `[[`, character(1), "feedback_id")
  if (!is.character(feedback_id) || length(feedback_id) != 1L || is.na(feedback_id) ||
      !feedback_id %in% ids) {
    stop("Select one existing source feedback entry.", call. = FALSE)
  }
  rule <- c(incomplete_cohort_rule(), list(
    source_feedback_id = feedback_id, approved_by = approver, approved_at = approved_at
  ))
  state$rules <- list(rule)
  write_guidance(board, state, snapshot)
  rule
}
