source(here::here("R", "live_report.R"))
source(here::here("R", "context_archive.R"))

# What the report runner and the chat app share: where the sandboxed REPL is,
# how a snapshot is handed to it, and what the model is told about the data and
# the approved context. Two agents warned differently about the same traps
# would be two AI systems; the point is that there is one.

# deploy.R bundles a Linux build at bin/mcp-repl for Connect. Locally that file
# is the wrong architecture, so it is used only if it runs. The installer puts
# a copy in ~/.local/bin, which a render or Shiny process often does not have
# on PATH.
live_repl_binary <- function() {
  bundled <- here::here("bin", "mcp-repl")
  if (file.exists(bundled)) {
    if (file.access(bundled, mode = 1L) != 0L) Sys.chmod(bundled, "0755")
    runs <- suppressWarnings(system2(bundled, "--help", stdout = FALSE, stderr = FALSE))
    if (identical(runs, 0L)) return(bundled)
  }
  found <- Sys.which("mcp-repl")[[1]]
  if (nzchar(found)) return(found)
  fallback <- path.expand("~/.local/bin/mcp-repl")
  if (file.exists(fallback)) return(fallback)
  stop(
    "mcp-repl not found. Install it with:\n",
    "  curl -fsSL https://raw.githubusercontent.com/posit-dev/mcp-repl/main/scripts/install.sh | sh",
    call. = FALSE
  )
}

# The house recipes as one file the sandbox can source on its own. recipes.R
# pulls in snapshot.R through here::here(), which has no project to find inside
# the sandbox, so the two are concatenated and INBOX_AS_OF is set from the
# snapshot itself. The recipes are copied, not rewritten: the model computes
# with exactly the functions the business uses.
live_recipes_source <- function(snapshot) {
  recipes <- readLines(here::here("R", "recipes.R"))
  recipes <- recipes[!grepl("source(here::here(", recipes, fixed = TRUE)]
  c(
    "# The house recipes and the snapshot contract, copied from the report's",
    "# repository for this session. source(\"recipes.R\") and use them as they are.",
    sprintf("INBOX_AS_OF <- as.Date(\"%s\")", require_snapshot(snapshot)),
    "",
    readLines(here::here("R", "snapshot.R")),
    "",
    recipes
  )
}

# mcp-repl's sandbox mode. workspace-write confines the worker to the directory
# it is spawned in, the scratch directory below. INBOX_REPL_SANDBOX overrides
# it for experiments; the values are mcp-repl's own.
live_repl_sandbox <- function() {
  Sys.getenv("INBOX_REPL_SANDBOX", "workspace-write")
}

# The REPL is spawned with this directory as its working directory, so the
# workspace it may write to is this and not the repository, and saved tables
# land where the caller can read them back. The arguments are the ones the
# marketing generator deploys with; the release deploy.R pins is the one that
# runs on Connect.
live_repl_scratch <- function(snapshot, binary = live_repl_binary(),
                              sandbox = live_repl_sandbox()) {
  dir <- tempfile("inbox-repl-")
  dir.create(dir, recursive = TRUE)
  saveRDS(snapshot, file.path(dir, "snapshot.rds"))
  writeLines(live_recipes_source(snapshot), file.path(dir, "recipes.R"))
  config <- file.path(dir, "mcp-repl.json")
  jsonlite::write_json(list(mcpServers = list(`r-repl` = list(
    command = binary,
    args = list("--sandbox", sandbox, "--interpreter", "r")
  ))), config, auto_unbox = TRUE, pretty = TRUE)
  list(dir = dir, config = config)
}

# Spawned from the scratch directory so its writable workspace is that
# directory, not wherever the caller is running.
live_repl_tools <- function(scratch) {
  # mcp-repl starts R by name. On Connect R lives under /opt/R and is not on
  # PATH, so the REPL would start and then fail on its first call.
  path <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  if (!R.home("bin") %in% path) {
    Sys.setenv(PATH = paste(c(R.home("bin"), path), collapse = .Platform$path.sep))
  }
  tools <- withr::with_dir(scratch$dir, mcptools::mcp_tools(config = scratch$config))
  if (!length(tools)) stop("The REPL exposed no tools.", call. = FALSE)
  tools
}

# Progress lines for the process log. knitr's message = FALSE swallows
# message(). stderr bypasses knitr, so on Connect this is what the job log
# shows while a run is in progress.
live_log <- function(...) {
  cat(format(Sys.time(), "%H:%M:%S"), paste0(..., collapse = ""), "\n", file = stderr())
}

# The conversation as clipped text. A run that ends without a report is read
# back from this.
live_transcript <- function(turns, width = 300L) {
  clip <- function(x) {
    x <- gsub("\\s+", " ", paste(format(x), collapse = " "))
    if (nchar(x) > width) paste0(substr(x, 1, width), " ...") else x
  }
  lines <- character()
  for (turn in turns) {
    for (content in turn@contents) {
      line <- if (S7::S7_inherits(content, ellmer::ContentToolRequest)) {
        arguments <- tryCatch(
          jsonlite::toJSON(content@arguments, auto_unbox = TRUE),
          error = function(e) "<arguments>"
        )
        paste0("[", turn@role, "] call ", content@name, "(", clip(arguments), ")")
      } else if (S7::S7_inherits(content, ellmer::ContentToolResult)) {
        if (!is.null(content@error)) {
          error <- if (inherits(content@error, "condition")) conditionMessage(content@error) else content@error
          paste0("[tool] ", content@request@name, " ERROR: ", clip(error))
        } else {
          paste0("[tool] ", content@request@name, " -> ", clip(content@value))
        }
      } else if (S7::S7_inherits(content, ellmer::ContentText)) {
        paste0("[", turn@role, "] ", clip(content@text))
      } else {
        paste0("[", turn@role, "] <", class(content)[[1]], ">")
      }
      lines <- c(lines, line)
    }
  }
  lines
}

live_snapshot_prompt <- function() {
  paste(
    "You have a sandboxed R session. `snapshot.rds` in the working directory is a",
    "point-in-time view of the lead funnel: one row per lead, with entry and stage",
    "dates, durations, and dimensions assigned at entry. Start every session with",
    "`snapshot <- readRDS(\"snapshot.rds\"); source(\"recipes.R\")`.",
    "",
    "HOUSE RECIPES",
    "recipes.R holds the five functions the business uses for the questions it",
    "asks most. Use them for any conversion, duration, funnel or campaign figure,",
    "so that your numbers are computed the way everyone else's are. Each reports",
    "its own denominator; read it.",
    "- cohort_conversion(snapshot, within_days = NULL, complete_months_only = TRUE):",
    "  entry-to-won conversion by monthly entry cohort. within_days = 30 gives the",
    "  equal-window comparison the approved rule asks for; NULL is wins known at",
    "  the cutoff, which is not comparable across cohorts of different ages.",
    "- conversion_by(snapshot, by = NULL, within_days = NULL): the same, grouped by",
    "  an entry-assigned dimension such as channel, company_size or region.",
    "- median_days_to(snapshot, stage = \"won\", by = NULL): median days from entry",
    "  to a stage, honouring the use_for_time_* flags, with the excluded count.",
    "- stage_funnel(snapshot, by = NULL): leads that had reached each stage at the",
    "  cutoff, long format.",
    "- campaign_reach(snapshot): touches per campaign. Touches, not leads.",
    "Grouping by a late_* column or by campaigns is refused on purpose; the error",
    "explains why. Anything the recipes do not cover you compute in plain dplyr,",
    "with the same care about denominators.",
    sep = "\n"
  )
}

live_data_prompt <- function() {
  paste(
    "WHAT THE DATA WILL DO TO YOU IF YOU ARE CARELESS",
    "- Recent cohorts have had less time to convert. Comparing conversion across",
    "  cohorts of different ages shows a decline that is only delay. Compare over",
    "  an equal window from entry, or say the newest cohort is incomplete.",
    "- late_industry, late_deal_value and late_competitor are captured partway",
    "  down the funnel and are blank for leads that stopped earlier. Grouping a",
    "  full-funnel denominator by one of them silently changes the denominator.",
    "- campaigns holds several values per lead. Splitting it before grouping",
    "  counts a lead once per campaign. Report touches, not leads.",
    "- Some stage dates were backfilled; the use_for_time_* flags say which are",
    "  trustworthy for durations. A duration from a guessed date is fiction.",
    "- won_as_of is what was known at the cutoff. There is no hindsight here and",
    "  you should not imply any.",
    sep = "\n"
  )
}

# The approved context, phrased for a prompt. context_archive() is the one
# place that decides what the model may know, so pending feedback never appears
# here. A board with no guidance yet contributes nothing.
live_context_prompt <- function(board, snapshot) {
  if (!pins::pin_exists(board, GUIDANCE_PIN)) {
    return("APPROVED DIRECTIVES\nNone yet.")
  }
  archive <- context_archive(board, snapshot)
  definitions <- vapply(names(archive$definitions), function(name) {
    paste0("- ", name, ": ", archive$definitions[[name]])
  }, character(1))
  directives <- if (length(archive$rules)) {
    vapply(archive$rules, function(rule) paste0("- ", rule$text), character(1))
  } else {
    "None yet."
  }
  paste(c(
    "DEFINITIONS", definitions, "",
    "APPROVED DIRECTIVES",
    if (length(archive$rules)) {
      "Reviewers of earlier reports approved these corrections as standing directives. Follow them."
    },
    directives
  ), collapse = "\n")
}
