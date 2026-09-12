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
    model = Sys.getenv("INBOX_CHAT_MODEL", "claude-sonnet-5")
  )
}

# Where the shared state lives. INBOX_BOARD is a folder path, or "connect" for
# the Posit Connect board of CONNECT_SERVER. On Connect the default is
# "connect": the server sets RSTUDIO_PRODUCT, CONNECT_SERVER and
# CONNECT_API_KEY for every piece of content, so the report, the apps and the
# seeding scripts all find the same board with nothing configured. Elsewhere
# the default is the local folder board, which is generated, not committed.
inbox_board_path <- function() {
  on_connect <- identical(Sys.getenv("RSTUDIO_PRODUCT"), "CONNECT")
  spec <- Sys.getenv("INBOX_BOARD", if (on_connect) "connect" else "board")
  if (identical(spec, "connect")) return("connect")
  if (startsWith(spec, "/") || startsWith(spec, "~")) path.expand(spec) else inbox_path(spec)
}

inbox_board <- function(path = inbox_board_path()) {
  if (identical(path, "connect")) {
    return(pins::board_connect(auth = "envvar", versioned = TRUE))
  }
  pins::board_folder(path, versioned = TRUE)
}

inbox_data_path <- function(...) inbox_path("data", ...)
inbox_fixture_path <- function(...) inbox_path("fixtures", ...)
inbox_artifact_path <- function(...) inbox_path("artifacts", ...)

# Where the apps are published, for the links in the email. CONNECT_SERVER is
# set by Connect for every piece of content and stays out of the repository;
# the paths are the apps' vanity URLs. With no server known there are no links,
# and the email says nothing about apps.
inbox_app_url <- function(app = c("chat", "feedback")) {
  server <- sub("/+$", "", Sys.getenv("CONNECT_SERVER"))
  if (!nzchar(server)) return(NULL)
  paste0(server, "/chickencloud_insight_", match.arg(app), "/")
}

# The fictitious company the generated data and the email header belong to.
# Deliberately invented so no reader mistakes it for a real customer.
INBOX_COMPANY <- "ChickenCloud"

# Every artifact in the repo is point-in-time as of this date. The generator
# produces outcomes past it, but the agent is never shown them.
INBOX_AS_OF <- as.Date("2026-06-30")
