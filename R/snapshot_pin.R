source(here::here("prep", "build_cohort.R"))

# One snapshot on the board rather than a copy in every bundle. The report pins
# the snapshot it worked from; the apps and a reader following the email's code
# read that pin. pins skips the write when the data has not changed, so a
# weekly run against the same frozen data adds no versions.

SNAPSHOT_PIN <- "funnel-snapshot"

write_snapshot_pin <- function(board, snapshot) {
  require_snapshot(snapshot)
  pins::pin_write(board, snapshot, name = SNAPSHOT_PIN, type = "rds")
  invisible(snapshot)
}

read_snapshot_pin <- function(board) {
  if (!pins::pin_exists(board, SNAPSHOT_PIN)) {
    stop(
      "No ", SNAPSHOT_PIN, " pin on this board yet. Render weekly-report.Rmd once,",
      " or run deploy.R, to write it.",
      call. = FALSE
    )
  }
  snapshot <- pins::pin_read(board, SNAPSHOT_PIN)
  require_snapshot(snapshot)
  snapshot
}

# What the apps work from: the pinned snapshot once one exists, the local
# cohort table before then. A Connect bundle carries no cohort table, so there
# the pin is the only source and the message says what to do about it.
current_snapshot <- function(board) {
  if (pins::pin_exists(board, SNAPSHOT_PIN)) return(read_snapshot_pin(board))
  if (file.exists(inbox_funnel_cohort_path())) return(funnel_snapshot(read_funnel_cohort()))
  read_snapshot_pin(board)
}
