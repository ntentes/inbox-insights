# Publish weekly-report.Rmd to Posit Connect as a scheduled email report.
#
#   Rscript deploy.R
#
# Needs, in the environment (~/.Renviron is the usual place):
#   CONNECT_SERVER      the Connect URL
#   CONNECT_API_KEY     an API key for the publishing account
#   ANTHROPIC_API_KEY   copied to the content's environment variables on Connect
# and data/funnel_cohort.csv built (Rscript prep/generate_data.R, then
# Rscript prep/build_cohort.R). The Rmd reads the cohort table from the bundle,
# so redeploy after rebuilding the data.
#
# Connect hosts are Linux, so the macOS mcp-repl from the installer is no use
# there. This script fetches the Linux release into bin/ (gitignored) and bundles
# it; weekly-report.Rmd looks there first. The release needs glibc 2.35+ on the
# Connect host, i.e. Ubuntu 22.04 or newer.
#
# Scheduling and recipients are set in the Connect UI after the first deploy.
# The email body is rsc_email_body_html with inline base64 images; some mail
# clients block those, so check the delivered email, not only the rendered page.

MCP_REPL_RELEASE <- "latest"  # or a tag, e.g. "v0.4.0", to pin
MCP_REPL_TARGET <- "x86_64-unknown-linux-gnu"

APP_NAME <- "chickencloud-weekly-insights"
APP_TITLE <- "ChickenCloud weekly insights"

# Everything the Rmd sources, transitively, plus the files those read.
# config.example.R is here because inbox_root() finds the project by it.
app_files <- c(
  "weekly-report.Rmd",
  "weekly-report.template.html",
  "config.example.R",
  "R/config.R",
  "R/recipes.R",
  "R/report_contract.R",
  "R/snapshot.R",
  "R/theme.R",
  "R/charts.R",
  "R/email_preview.R",
  "R/live_report.R",
  "R/token_budget.R",
  "prep/generate_data.R",
  "prep/build_cohort.R",
  "data/funnel_cohort.csv",
  "images/chickencloud-logo.png",
  "bin/mcp-repl"
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

# rsconnect keeps its own registry of servers and accounts. Register the one
# named in CONNECT_SERVER once and reuse it on every later run.
connect_account <- function(url, api_key) {
  strip <- function(x) sub("/+(__api__/*)?$", "", x)
  servers <- rsconnect::servers()
  server <- servers$name[strip(servers$url) == strip(url)]
  if (!length(server)) {
    server <- "connect"
    rsconnect::addServer(url = url, name = server, quiet = TRUE)
  }
  server <- server[[1]]

  accounts <- rsconnect::accounts(server = server)
  if (is.null(accounts) || !nrow(accounts)) {
    rsconnect::connectApiUser(server = server, apiKey = api_key, quiet = TRUE)
    accounts <- rsconnect::accounts(server = server)
  }
  list(server = server, account = accounts$name[[1]])
}

deploy_weekly_report <- function() {
  if (!file.exists("weekly-report.Rmd")) stop("Run from the project root.", call. = FALSE)
  url <- require_env("CONNECT_SERVER")
  api_key <- require_env("CONNECT_API_KEY")
  require_env("ANTHROPIC_API_KEY")
  if (!file.exists("data/funnel_cohort.csv")) {
    stop(
      "data/funnel_cohort.csv is missing. Build it first:\n",
      "  Rscript prep/generate_data.R\n  Rscript prep/build_cohort.R",
      call. = FALSE
    )
  }

  fetch_mcp_repl()
  who <- connect_account(url, api_key)

  # Provider overrides travel with the deploy only if set here.
  env_vars <- c(
    "ANTHROPIC_API_KEY",
    Filter(function(v) nzchar(Sys.getenv(v)), c("INBOX_CHAT_PROVIDER", "INBOX_CHAT_MODEL"))
  )

  rsconnect::deployApp(
    appDir = ".",
    appFiles = app_files,
    appPrimaryDoc = "weekly-report.Rmd",
    appName = APP_NAME,
    appTitle = APP_TITLE,
    envVars = env_vars,
    server = who$server,
    account = who$account,
    forceUpdate = TRUE,
    launch.browser = FALSE,
    lint = FALSE
  )
}

if (sys.nframe() == 0L) {
  deploy_weekly_report()
}
