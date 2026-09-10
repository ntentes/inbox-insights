# Publish the live runner and the two apps to Posit Connect.
#
#   Rscript deploy.R              # the report, the chat app and the feedback app
#   Rscript deploy.R report       # or: chat, feedback
#
# Needs, in the environment (~/.Renviron is the usual place):
#   CONNECT_SERVER      the Connect URL
#   CONNECT_API_KEY     an API key for the publishing account
#   ANTHROPIC_API_KEY   copied to the report's and the chat's environment on Connect
# and data/funnel_cohort.csv built (Rscript prep/generate_data.R, then
# Rscript prep/build_cohort.R).
#
# On Connect the three pieces share the Connect board of the publishing
# account (see inbox_board() in R/config.R); no board setting is deployed. The
# data travels as a pin, not in the app bundles: the report carries the cohort
# table and pins the snapshot it works from as funnel-snapshot on every run,
# the apps read that pin, and this script pins it once at the end of a deploy
# so the apps work before the first scheduled run.
#
# Connect hosts are Linux, so the macOS mcp-repl from the installer is no use
# there. This script fetches the Linux release into bin/ (gitignored) and bundles
# it with the report and the chat; both look there first. The release is pinned
# to one whose Linux sandbox runs inside a Connect container and whose protocol
# the current mcptools speaks; neither is true of every release. Delete
# bin/mcp-repl after changing the pin so the next run fetches the new one, and
# read the first run's log.
#
# Scheduling and recipients are set in the Connect UI after the first deploy.
# The email body is the rendered page with every style inline and the logo as a
# CID attachment (rsc_email_images), so it survives mail clients that strip
# <style> blocks and data: images.

MCP_REPL_RELEASE <- "v0.2.0"
MCP_REPL_TARGET <- "x86_64-unknown-linux-gnu"

# Everything the Rmd and the apps source, transitively, plus the files those
# read: the same list .gitignore allows into the repository. config.example.R is
# here because inbox_root() finds the project by it.
shared_files <- function() {
  c(
    "config.example.R",
    file.path("R", paste0(c(
      "charts", "config", "context_archive", "email_preview", "feedback",
      "headline_history", "live_report", "live_session", "recipes",
      "report_contract", "snapshot", "snapshot_pin", "theme", "token_budget"
    ), ".R")),
    "prep/generate_data.R",
    "prep/build_cohort.R",
    "prep/build_initial_report.R",
    "images/chickencloud-logo.png"
  )
}

deployables <- list(
  report = list(
    name = "chickencloud-weekly-insights",
    title = "ChickenCloud Weekly Insights",
    document = "weekly-report.Rmd",
    files = c("weekly-report.Rmd", "weekly-report.template.html", "data/funnel_cohort.csv", "bin/mcp-repl"),
    env = "ANTHROPIC_API_KEY"
  ),
  chat = list(
    name = "chickencloud-weekly-insights-chat",
    title = "ChickenCloud Weekly Insights: Chat",
    app = "apps/chat",
    files = "bin/mcp-repl",
    env = "ANTHROPIC_API_KEY"
  ),
  feedback = list(
    name = "chickencloud-weekly-insights-feedback",
    title = "ChickenCloud Weekly Insights: Feedback",
    app = "apps/feedback",
    files = character(),
    env = character()
  )
)

mcp_repl_url <- function(release = MCP_REPL_RELEASE, target = MCP_REPL_TARGET) {
  asset <- paste0("mcp-repl-", target, ".tar.gz")
  base <- "https://github.com/posit-dev/mcp-repl/releases"
  if (identical(release, "latest")) {
    file.path(base, "latest", "download", asset)
  } else {
    file.path(base, "download", release, asset)
  }
}

fetch_mcp_repl <- function(dest = "bin/mcp-repl") {
  if (file.exists(dest)) {
    message("Using existing ", dest, " (delete it to fetch a fresh release)")
    return(invisible(dest))
  }
  url <- mcp_repl_url()
  message("Downloading ", url)
  archive <- tempfile(fileext = ".tar.gz")
  extracted <- tempfile("mcp-repl-")
  on.exit(unlink(c(archive, extracted), recursive = TRUE), add = TRUE)
  utils::download.file(url, archive, mode = "wb", quiet = TRUE)
  utils::untar(archive, exdir = extracted)
  binary <- file.path(extracted, paste0("mcp-repl-", MCP_REPL_TARGET), "mcp-repl")
  if (!file.exists(binary)) {
    stop("The release archive did not contain ", basename(binary), " where expected.")
  }
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  file.copy(binary, dest, overwrite = TRUE)
  Sys.chmod(dest, "0755")
  invisible(dest)
}

require_env <- function(name) {
  value <- Sys.getenv(name)
  if (!nzchar(value)) stop(name, " is not set. Put it in ~/.Renviron.", call. = FALSE)
  value
}

# rsconnect keeps its own registry of servers and accounts. The server named
# in CONNECT_SERVER is registered once; the API key is registered on every run
# under a fixed alias, so the key in the environment is always the one that
# deploys. A rotated key replaces the old one, and other accounts registered
# for the same server are never picked up by accident.
DEPLOY_ACCOUNT <- "inbox-insights-deploy"

connect_account <- function(url, api_key) {
  strip <- function(x) sub("/+(__api__/*)?$", "", x)
  servers <- rsconnect::servers()
  server <- servers$name[strip(servers$url) == strip(url)]
  if (!length(server)) {
    server <- "connect"
    rsconnect::addServer(url = url, name = server, quiet = TRUE)
  }
  server <- server[[1]]

  rsconnect::connectApiUser(
    account = DEPLOY_ACCOUNT, server = server, apiKey = api_key, quiet = TRUE
  )
  list(server = server, account = DEPLOY_ACCOUNT)
}

# Provider overrides travel with the deploy only if set here.
env_vars <- function(target) {
  c(target$env, Filter(function(v) nzchar(Sys.getenv(v)), c("INBOX_CHAT_PROVIDER", "INBOX_CHAT_MODEL")))
}

# A Shiny app on Connect needs app.R at the root of its bundle, but locally the
# apps live in apps/<name>/ and source the project's R/ files. So the bundle is
# staged: app.R at the root, the shared files at their project-relative paths,
# which is the layout app.R's here::i_am() call expects.
stage_app <- function(target) {
  stage <- tempfile("inbox-deploy-")
  dir.create(stage)
  files <- c(shared_files(), target$files)
  file.copy(file.path(target$app, "app.R"), file.path(stage, "app.R"))
  for (file in files) {
    dir.create(file.path(stage, dirname(file)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file, file.path(stage, file))
  }
  list(dir = stage, files = c("app.R", files))
}

deploy_target <- function(target, who) {
  if (is.null(target$app)) {
    app_dir <- "."
    app_files <- c(shared_files(), target$files)
  } else {
    staged <- stage_app(target)
    on.exit(unlink(staged$dir, recursive = TRUE), add = TRUE)
    app_dir <- staged$dir
    app_files <- staged$files
  }
  rsconnect::deployApp(
    appDir = app_dir,
    appFiles = app_files,
    appPrimaryDoc = target$document,
    appName = target$name,
    appTitle = target$title,
    envVars = env_vars(target),
    server = who$server,
    account = who$account,
    forceUpdate = TRUE,
    launch.browser = FALSE,
    lint = FALSE
  )
}

deploy <- function(targets = names(deployables)) {
  unknown <- setdiff(targets, names(deployables))
  if (length(unknown)) {
    stop("Unknown target(s): ", paste(unknown, collapse = ", "),
      ". Choose from: ", paste(names(deployables), collapse = ", "), call. = FALSE)
  }
  if (!file.exists("weekly-report.Rmd")) stop("Run from the project root.", call. = FALSE)
  url <- require_env("CONNECT_SERVER")
  api_key <- require_env("CONNECT_API_KEY")
  if (any(vapply(deployables[targets], function(t) "ANTHROPIC_API_KEY" %in% t$env, logical(1)))) {
    # The targets that call a model copy ANTHROPIC_API_KEY and no other
    # credential, so they are Anthropic-only; another provider would deploy
    # without its key and fail on every scheduled run.
    provider <- Sys.getenv("INBOX_CHAT_PROVIDER", "anthropic")
    if (!identical(provider, "anthropic")) {
      stop("INBOX_CHAT_PROVIDER is \"", provider, "\" but this deployment is Anthropic-only.",
        call. = FALSE)
    }
    require_env("ANTHROPIC_API_KEY")
  }
  if (!file.exists("data/funnel_cohort.csv")) {
    stop(
      "data/funnel_cohort.csv is missing. Build it first:\n",
      "  Rscript prep/generate_data.R\n  Rscript prep/build_cohort.R",
      call. = FALSE
    )
  }
  if (any(vapply(deployables[targets], function(t) "bin/mcp-repl" %in% t$files, logical(1)))) {
    fetch_mcp_repl()
  }

  who <- connect_account(url, api_key)
  for (name in targets) {
    message("\n== Deploying ", name, " (", deployables[[name]]$title, ") ==")
    deploy_target(deployables[[name]], who)
  }
  pin_snapshot_to_connect()
}

# The same snapshot the report will pin on its first run, written now from the
# local cohort table so the apps have data to open on straight after a deploy.
pin_snapshot_to_connect <- function() {
  source(here::here("R", "snapshot_pin.R"))
  write_snapshot_pin(inbox_board("connect"), funnel_snapshot(read_funnel_cohort()))
  message("Pinned the snapshot as ", SNAPSHOT_PIN, " on the Connect board")
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  deploy(if (length(args)) args else names(deployables))
}
