source(here::here("R", "live_report.R"))

# What the weekly report leaves behind: one pin, one row per headline per run.
# The next run reads it so the model knows what has already been said. The
# chat and feedback apps read it so they open on the report the reader has in
# front of them. Full reports stay in artifacts/live/; this is the flat view a
# prompt and a Shiny table both want.

HEADLINE_HISTORY_PIN <- "weekly-headlines"

headline_history_columns <- function() {
  data.frame(
    generated_at = character(), data_as_of = character(), snapshot_id = character(),
    provider = character(), model = character(), slot = integer(),
    title = character(), finding = character(), suggested_action = character(),
    metric_definition = character(), caveat = character(),
    evidence_json = character(), reproducible_code = character(),
    stringsAsFactors = FALSE
  )
}

validate_headline_history <- function(history) {
  if (!is.data.frame(history) ||
      !identical(names(history), names(headline_history_columns()))) {
    stop("The headline history pin does not have the expected columns.", call. = FALSE)
  }
  invisible(history)
}

headline_rows <- function(report, snapshot) {
  validate_live_report(report, snapshot)
  rows <- lapply(seq_along(report$insights), function(i) {
    insight <- report$insights[[i]]
    data.frame(
      generated_at = report$provenance$generated_at,
      data_as_of = as.character(require_snapshot(snapshot)),
      snapshot_id = report$snapshot_id,
      provider = report$provenance$provider,
      model = report$provenance$model,
      slot = i,
      title = insight$title,
      finding = insight$finding,
      suggested_action = insight$suggested_action,
      metric_definition = insight$metric_definition,
      caveat = insight$caveat,
      evidence_json = as.character(jsonlite::toJSON(
        insight$evidence, auto_unbox = TRUE, digits = NA
      )),
      reproducible_code = insight$reproducible_code,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

read_headline_history <- function(board) {
  if (!pins::pin_exists(board, HEADLINE_HISTORY_PIN)) return(headline_history_columns())
  validate_headline_history(pins::pin_read(board, HEADLINE_HISTORY_PIN))
}

append_headline_history <- function(board, report, snapshot) {
  history <- read_headline_history(board)
  rows <- headline_rows(report, snapshot)
  if (rows$generated_at[[1]] %in% history$generated_at) {
    stop("This report is already in the headline history.", call. = FALSE)
  }
  updated <- rbind(history, rows)
  pins::pin_write(board, updated, name = HEADLINE_HISTORY_PIN, type = "rds")
  invisible(updated)
}

latest_headlines <- function(history) {
  if (!nrow(history)) return(history)
  rows <- history[history$generated_at == max(history$generated_at), ]
  rows[order(rows$slot), ]
}

# Formatted the way the email shows it, so the chat and the reader are looking
# at the same numbers.
evidence_table_text <- function(evidence_json) {
  evidence <- jsonlite::fromJSON(evidence_json, simplifyVector = FALSE)
  formatted <- format_evidence_table(evidence)
  header <- paste(c(evidence$label_column, unlist(evidence$value_columns)), collapse = " | ")
  rows <- vapply(seq_along(evidence$rows), function(i) {
    paste(c(evidence$rows[[i]]$label, vapply(formatted, `[[`, character(1), i)), collapse = " | ")
  }, character(1))
  paste(c(header, rows), collapse = "\n")
}

# Recent runs for the report runner's prompt. Titles, findings and actions
# only: enough to know what has been said, not enough to copy from.
headline_history_prompt <- function(history, reports = 4L) {
  if (!nrow(history)) {
    return("PREVIOUS REPORTS\nNone. This is the first report.")
  }
  runs <- sort(unique(history$generated_at), decreasing = TRUE)
  recent <- history[history$generated_at %in% utils::head(runs, reports), ]
  recent <- recent[order(-xtfrm(recent$generated_at), recent$slot), ]
  lines <- sprintf(
    "- [%s] %s: %s Suggested action: %s",
    substr(recent$generated_at, 1, 10), recent$title, recent$finding, recent$suggested_action
  )
  paste(c(
    "PREVIOUS REPORTS",
    paste0(
      "Headlines from the last ", length(unique(recent$generated_at)),
      " report(s), newest first. Do not report any of these again as a new"
    ),
    "finding. Build on one only if you add something: a deeper cut, a follow-up",
    "question, or a check the earlier report did not make. Then say so and refer",
    "back to the earlier headline.",
    lines
  ), collapse = "\n")
}

# The whole of one report as text, for a chat that starts already knowing it.
headline_report_prompt <- function(rows) {
  if (!nrow(rows)) return("LOADED REPORT\nNo live report has run yet.")
  indent <- function(text, by = "    ") paste0(by, strsplit(text, "\n", fixed = TRUE)[[1]])
  blocks <- vapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    paste(c(
      sprintf("HEADLINE %d: %s", r$slot, r$title),
      paste0("  Finding: ", r$finding),
      paste0("  Suggested action: ", r$suggested_action),
      paste0("  Metric: ", r$metric_definition),
      paste0("  Caveat: ", r$caveat),
      "  Evidence, as the model saved it:",
      indent(evidence_table_text(r$evidence_json)),
      "  The model's code for that table:",
      indent(r$reproducible_code)
    ), collapse = "\n")
  }, character(1))
  paste(c(
    sprintf(
      "LOADED REPORT\nThe weekly report generated %s, data as of %s, written by %s and not yet reviewed. The reader has it in front of them.",
      rows$generated_at[[1]], rows$data_as_of[[1]], rows$model[[1]]
    ),
    blocks
  ), collapse = "\n\n")
}
