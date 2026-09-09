# Configuration and paths. Everything here has a working default so that a
# clean clone builds without a config.R and without model credentials.

inbox_root <- function() {
  # Callers may be a Shiny app in apps/<name>/, a script in prep/, or the
  # project root, so walk up until the project marker turns up.
  path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "config.example.R"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate the inbox-insights project root from ", getwd())
    }
    path <- parent
  }
}

inbox_path <- function(...) {
  file.path(inbox_root(), ...)
}

# "fixture" or "live". Fixture mode never calls a model provider.
inbox_mode <- function() {
  mode <- Sys.getenv("INBOX_MODE", "fixture")
  if (!mode %in% c("fixture", "live")) {
    stop("INBOX_MODE must be \"fixture\" or \"live\", not \"", mode, "\"")
  }
  mode
}

inbox_is_live <- function() {
  identical(inbox_mode(), "live")
}

inbox_provider <- function() {
  list(
    provider = Sys.getenv("INBOX_CHAT_PROVIDER", "anthropic"),
    model = Sys.getenv("INBOX_CHAT_MODEL", "")
  )
}

# The board is generated, not committed; prep/seed_demo_board.R rebuilds it.
inbox_board_path <- function() {
  inbox_path(Sys.getenv("INBOX_BOARD", "board"))
}

inbox_data_path <- function(...) inbox_path("data", ...)
inbox_fixture_path <- function(...) inbox_path("fixtures", ...)
inbox_artifact_path <- function(...) inbox_path("artifacts", ...)

# The fictitious company the generated data and the email header belong to.
# Deliberately invented so no reader mistakes it for a real customer.
INBOX_COMPANY <- "ChickenCloud"

# Every artifact in the repo is point-in-time as of this date. The generator
# produces outcomes past it, but the agent is never shown them.
INBOX_AS_OF <- as.Date("2026-06-30")
