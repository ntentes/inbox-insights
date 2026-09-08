source(here::here("R", "feedback.R"))

archive_definitions <- function() {
  list(
    denominator = "Leads entering a complete calendar month, not only leads reaching a later stage.",
    grouping = "Use entry-assigned dimensions for full-funnel denominators; use the house recipes.",
    time_contract = "Weekly delivery, the 28-day reporting period, monthly entry cohorts, and outcome horizon are distinct.",
    evidence = "Only information known in the frozen snapshot is available. Hindsight is excluded.",
    implementation = "House recipes are in R/recipes.R. Their guards do not restrict arbitrary REPL code."
  )
}

context_archive <- function(board, snapshot) {
  state <- read_guidance(board, snapshot)
  contract <- worked_time_contract(snapshot)
  payload <- list(
    schema_version = 1L, company = INBOX_COMPANY,
    snapshot_id = report_snapshot_id(snapshot),
    data_as_of = contract$data_as_of,
    reporting_period = contract$reporting_period,
    evidence_window = contract$evidence_window,
    definitions = archive_definitions(),
    rules = state$rules
  )
  # Pending feedback and source-report prose are intentionally not prompt inputs.
  c(list(archive_id = report_hash(payload)), payload)
}

require_current_archive <- function(archive, board, snapshot) {
  expected <- context_archive(board, snapshot)
  if (!identical(canonical_report_json(archive), canonical_report_json(expected))) {
    stop("Archive does not match the current approved context.", call. = FALSE)
  }
  invisible(archive)
}

write_context_archive <- function(archive, board, snapshot, path) {
  require_current_archive(archive, board, snapshot)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(archive, path, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE)
  invisible(path)
}

read_context_archive <- function(path, board, snapshot) {
  archive <- jsonlite::read_json(path, simplifyVector = FALSE)
  require_current_archive(archive, board, snapshot)
  archive
}
