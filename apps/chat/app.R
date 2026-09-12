# A thin interface over what the report already accumulated, not a second AI
# system. The chat gets the same data warnings and approved context as the
# report runner (R/live_session.R), a sandboxed R session over the same frozen
# snapshot, and the latest headlines from the shared board. It opens already
# knowing what the reader is looking at.
#
# Locally this file is apps/chat/app.R; deploy.R stages it at the root of the
# Connect bundle next to the R/ and prep/ files it sources.
here::i_am(if (file.exists("config.example.R")) "app.R" else "apps/chat/app.R")

library(shiny)
library(bslib)
library(shinychat)

source(here::here("R", "snapshot_pin.R"))
source(here::here("R", "live_session.R"))
source(here::here("R", "headline_history.R"))
source(here::here("R", "theme.R"))

board <- inbox_board()
snapshot <- current_snapshot(board)
latest <- latest_headlines(read_headline_history(board))

# Without the REPL the chat can still discuss the report; it cannot compute.
repl_binary <- tryCatch(live_repl_binary(), error = function(e) {
  message(conditionMessage(e))
  NULL
})

# Each session's scratch directory is served over HTTP under its own prefix,
# so a PNG the model saves there can appear in the chat as a markdown image.
# The prompt must know the prefix, so it is built per session.
plot_prompt <- function(files_prefix) {
  if (is.null(files_prefix)) return("")
  paste(
    "SHOWING PLOTS",
    "You can show charts, and you should when one answers better than a table",
    "or the reader asks for one. Build a ggplot in the R session (ggplot2 is",
    "installed; prefer it to base graphics), save it in the working directory",
    "under a unique name, then put it in your reply as a markdown image whose",
    "path is the file name behind this prefix:",
    "  library(ggplot2)",
    "  p <- ggplot(df, aes(x, y)) + geom_col() + theme_minimal()",
    "  ggsave(\"plot_<slug>.png\", p, width = 8, height = 4.5, dpi = 110)",
    paste0("  ![Leads by entry month](", files_prefix, "/plot_<slug>.png)"),
    "Only the file name after the prefix, never a full path. Give the image a",
    "short caption and keep the key numbers in the text beside it. Never",
    "describe a chart you did not save.",
    "",
    sep = "\n"
  )
}

chat_system_prompt <- function(files_prefix = NULL) paste(
  paste(
    "You answer questions about ChickenCloud's lead funnel for the people who",
    "receive its weekly report. ChickenCloud is a B2B SaaS company."
  ),
  "",
  live_snapshot_prompt(),
  "",
  live_data_prompt(),
  "",
  live_context_prompt(board, snapshot),
  "",
  headline_report_prompt(latest),
  "",
  plot_prompt(files_prefix),
  "HOW TO ANSWER",
  "- Compute in the R session first, then answer in prose with the numbers",
  "  inline. Say what each number is: a count, or a rate over which window and",
  "  which denominator.",
  "- When asked about a headline, start from its evidence table and its code.",
  "  Verify or extend it, and say plainly if it does not hold up.",
  "- Prefer a clearly stated null result to an overclaim. If the snapshot cannot",
  "  support an answer, say so and why.",
  "- When asked for code the reader can run, give plain dplyr against `snapshot`,",
  "  spelling out what a recipe did: the reader has the snapshot pin and dplyr,",
  "  not recipes.R.",
  "- Nothing after the cutoff is known. Do not imply that it is.",
  sep = "\n"
)

greeting <- function() {
  if (!nrow(latest)) {
    return(paste(
      "No live report has run against this board yet. Ask me anything about the",
      "funnel snapshot and I will look in the R session."
    ))
  }
  bullets <- sprintf("%d. **%s** %s", latest$slot, latest$title, latest$finding)
  paste0(
    "I have the weekly report generated ", substr(latest$generated_at[[1]], 1, 10),
    " (data as of ", latest$data_as_of[[1]], ") in front of me. Its headlines:\n\n",
    paste(bullets, collapse = "\n"),
    "\n\nAsk me to check one, cut it differently, or look at something it missed.",
    " I work from the same frozen snapshot the report used, and nothing here has",
    " been reviewed."
  )
}

headline_summary <- function(row) {
  div(class = "accent-rule mb-3",
    div(class = "small text-muted-warm", sprintf("Headline %d", row$slot)),
    div(class = "fw-semibold", row$title),
    div(class = "small", row$finding)
  )
}

ui <- page_sidebar(
  title = inbox_app_masthead(paste(INBOX_COMPANY, "Weekly Insights: Chat")),
  window_title = paste(INBOX_COMPANY, "Weekly Insights: Chat"),
  theme = inbox_bs_theme(),
  fillable = TRUE,
  sidebar = sidebar(width = 360, open = "desktop", title = "The loaded report",
    if (nrow(latest)) {
      tagList(
        p(class = "small text-muted-warm",
          sprintf("Generated %s, data as of %s.", substr(latest$generated_at[[1]], 1, 10), latest$data_as_of[[1]])),
        lapply(seq_len(nrow(latest)), function(i) headline_summary(latest[i, ]))
      )
    } else {
      p(class = "small text-muted-warm", "No live report has run yet.")
    },
    hr(),
    actionButton("reset", "New conversation", class = "btn-outline-secondary btn-sm w-100"),
    p(class = "small text-muted-warm mt-3",
      if (is.null(repl_binary)) {
        "The R session is unavailable: mcp-repl was not found, so the chat can discuss the report but not compute."
      } else {
        "Every answer is computed in a sandboxed R session over the same snapshot the report used."
      })
  ),
  div(class = "small text-muted-warm mb-2",
    "A model wrote the report and writes the answers here; neither has been reviewed.",
    "Check a number before acting on it."),
  chat_ui("chat", messages = list(list(role = "assistant", content = greeting())),
    width = "100%", fill = TRUE)
)

server <- function(input, output, session) {
  provider <- inbox_provider()

  # One REPL per session, in its own scratch directory holding the snapshot.
  # The directory is served over HTTP for plots and removed at session end.
  scratch <- NULL
  files_prefix <- NULL
  repl_tools <- list()
  if (!is.null(repl_binary)) {
    scratch <- live_repl_scratch(snapshot, repl_binary)
    repl_tools <- tryCatch(live_repl_tools(scratch), error = function(e) {
      message("The REPL did not start: ", conditionMessage(e))
      list()
    })
    if (length(repl_tools)) {
      files_prefix <- paste0("replfiles-", session$token)
      addResourcePath(files_prefix, scratch$dir)
    }
  }

  client <- ellmer::chat(
    paste0(provider$provider, "/", provider$model),
    system_prompt = chat_system_prompt(files_prefix), echo = "none"
  )
  if (length(repl_tools)) client$set_tools(repl_tools)

  observeEvent(input$chat_user_input, {
    chat_append("chat", client$stream_async(input$chat_user_input))
  })

  observeEvent(input$reset, {
    reset_tool <- Filter(function(tool) identical(tool@name, "repl_reset"), repl_tools)
    if (length(reset_tool)) try(reset_tool[[1]](), silent = TRUE)
    client$set_turns(list())
    chat_clear("chat")
    chat_append("chat", greeting(), role = "assistant")
  })

  session$onSessionEnded(function() {
    if (!is.null(files_prefix)) removeResourcePath(files_prefix)
    if (!is.null(scratch)) unlink(scratch$dir, recursive = TRUE)
  })
}

shinyApp(ui, server)
