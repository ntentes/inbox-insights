# Copy this file to config.R and edit it. config.R is gitignored.
#
# You do not need this file to build the talk. Fixture mode is the default:
# every artifact -- the data, the cohort table, the plots, the email, and both
# Shiny apps -- builds from the seeded generator and the committed fixtures,
# with no model credentials. Live mode is only for regenerating the fixtures.

# "fixture" (default) or "live".
Sys.setenv(INBOX_MODE = "fixture")

# Live mode only. Any provider ellmer supports will do, including a local model
# served by Ollama. Keep the API key itself in ~/.Renviron, not here.
Sys.setenv(INBOX_CHAT_PROVIDER = "anthropic")
Sys.setenv(INBOX_CHAT_MODEL = "claude-sonnet-4-5")

# Where the pin board lives. The talk uses a versioned folder board so the demo
# runs on one laptop; see the README for the Connect and S3 equivalents.
Sys.setenv(INBOX_BOARD = "board")
