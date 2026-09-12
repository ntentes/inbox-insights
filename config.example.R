# Copy this file to config.R and edit it. config.R is gitignored.
#
# You do not need this file to build the talk. Fixture mode is the default:
# every artifact builds from the seeded generator and the committed fixtures
# with no model credentials. Live mode is only for regenerating the fixtures.

# "fixture" (default) or "live".
Sys.setenv(INBOX_MODE = "fixture")

# Live mode only. Any provider ellmer supports works, including a local model
# served by Ollama. Keep the API key in ~/.Renviron, not here.
Sys.setenv(INBOX_CHAT_PROVIDER = "anthropic")
Sys.setenv(INBOX_CHAT_MODEL = "claude-sonnet-5")

# Where the pin board lives: a folder path, or "connect" for the Posit Connect
# board at CONNECT_SERVER (with CONNECT_API_KEY in ~/.Renviron). Content
# running on Connect defaults to "connect". The talk uses a versioned folder
# board so the demo runs on one laptop.
Sys.setenv(INBOX_BOARD = "board")
