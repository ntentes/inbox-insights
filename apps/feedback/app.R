# The reader's side of the loop. Opens on the latest live report's headlines,
# takes a correction about one, and saves it as pending feedback. Approving a
# correction as a standing directive is a separate explicit action on its own
# tab, bound to the state the approver was shown. Nothing reaches a prompt
# until then: context_archive() decides what the model may know and never
# includes pending feedback.
#
# Locally this file is apps/feedback/app.R; deploy.R stages it at the root of
# the Connect bundle next to the R/ and prep/ files it sources.
here::i_am(if (file.exists("config.example.R")) "app.R" else "apps/feedback/app.R")

library(shiny)
library(bslib)

source(here::here("R", "snapshot_pin.R"))
source(here::here("prep", "build_initial_report.R"))
source(here::here("R", "context_archive.R"))
source(here::here("R", "headline_history.R"))
source(here::here("R", "theme.R"))
source(here::here("R", "connect_access.R"))

board <- inbox_board()
snapshot <- current_snapshot(board)
latest <- latest_headlines(read_headline_history(board))

read_guidance_or_null <- function() {
  if (!pins::pin_exists(board, GUIDANCE_PIN)) return(NULL)
  read_guidance(board, snapshot)
}

evidence_table <- function(evidence_json) {
  evidence <- jsonlite::fromJSON(evidence_json, simplifyVector = FALSE)
  formatted <- format_evidence_table(evidence)
  tags$table(class = "table table-sm tabular mb-2",
    tags$thead(tags$tr(
      tags$th(evidence$label_column),
      lapply(unlist(evidence$value_columns), function(name) tags$th(class = "text-end", name))
    )),
    tags$tbody(lapply(seq_along(evidence$rows), function(i) tags$tr(
      tags$th(scope = "row", evidence$rows[[i]]$label),
      lapply(formatted, function(column) tags$td(class = "text-end", column[[i]]))
    )))
  )
}

headline_card <- function(row) {
  card(
    card_header(sprintf("Headline %d", row$slot)),
    card_body(
      h4(row$title),
      p(row$finding),
      div(class = "accent-rule mb-3", p(class = "mb-0", tags$strong("Suggested action. "), row$suggested_action)),
      p(class = "small text-muted-warm", row$metric_definition),
      evidence_table(row$evidence_json),
      p(class = "small", tags$strong("Caveat. "), row$caveat),
      tags$details(
        tags$summary(class = "small", "The model's code for that table (not re-run)"),
        tags$pre(class = "mt-2", tags$code(row$reproducible_code))
      )
    )
  )
}

about_choices <- function(rows) {
  choices <- c("The report as a whole" = "report")
  if (nrow(rows)) choices <- c(choices, stats::setNames(rows$title, rows$title))
  choices
}

ui <- page_navbar(
  id = "nav",
  title = inbox_app_masthead(paste(INBOX_COMPANY, "Weekly Insights: Feedback")),
  window_title = paste(INBOX_COMPANY, "Weekly Insights: Feedback"),
  theme = inbox_bs_theme(),
  # The page scrolls; cards take the height of their content instead of
  # clipping a headline's callout behind an inner scrollbar.
  fillable = FALSE,
  nav_panel("Latest report",
    layout_sidebar(
      sidebar = sidebar(width = 380, open = "always", title = "Send a correction",
        uiOutput("author_ui"),
        selectInput("about", "About", choices = about_choices(latest)),
        textAreaInput("feedback_text", "What should change, and why?", rows = 7),
        actionButton("submit_feedback", "Save as pending feedback", class = "btn-primary w-100"),
        p(class = "small text-muted-warm mt-3",
          "Saved feedback is pending. It changes nothing until a collaborator on this",
          "app approves it as a directive, and the report's model never sees it",
          "before that.")
      ),
      if (nrow(latest)) {
        tagList(
          p(class = "text-muted-warm small",
            sprintf("Live model run generated %s, data as of %s, written by %s. Submitted for review, not reviewed.",
              latest$generated_at[[1]], latest$data_as_of[[1]], latest$model[[1]])),
          lapply(seq_len(nrow(latest)), function(i) headline_card(latest[i, ]))
        )
      } else {
        p(class = "text-muted-warm",
          "No live report has run against this board yet. Render weekly-report.Rmd and its headlines appear here.")
      }
    )
  ),
  nav_panel("Pending feedback and approval",
    layout_columns(col_widths = c(7, 5),
      card(card_header("Feedback"), card_body(uiOutput("feedback_list"))),
      card(card_header("Directives"), card_body(uiOutput("approval_ui")))
    )
  ),
  nav_panel("Approved context",
    card(
      card_header("What the report's model is told"),
      card_body(
        p(class = "small text-muted-warm",
          "The context archive, exactly as R/context_archive.R builds it: definitions",
          "and approved directives. Pending feedback is not in it."),
        verbatimTextOutput("archive")
      )
    )
  )
)

server <- function(input, output, session) {
  guidance <- reactiveVal(read_guidance_or_null())
  shown_state_id <- reactiveVal(NULL)
  refresh <- function() guidance(read_guidance_or_null())
  notify <- function(text, type = "message") showNotification(text, type = type, duration = 7)

  on_connect <- !is.null(session$user)
  output$author_ui <- renderUI({
    if (on_connect) {
      p(tags$strong("From: "), session$user)
    } else {
      textInput("author", "From", value = Sys.info()[["user"]])
    }
  })
  current_user <- reactive({
    if (on_connect) session$user else trimws(input$author %||% Sys.info()[["user"]])
  })

  # On Connect only the app's collaborators (its owner and anyone given the
  # collaborator role) see the approval tab or can approve; see
  # R/connect_access.R. The content name is the one deploy.R publishes under.
  # Locally there is no login and anyone can approve.
  access <- if (on_connect) {
    feedback_access(session$user, session$groups, "chickencloud-weekly-insights-feedback")
  } else {
    list(allowed = TRUE, reason = NULL)
  }
  if (!access$allowed) {
    message("approval disabled for ", session$user, ": ", access$reason)
    nav_hide("nav", "Pending feedback and approval")
  }
  refused <- function() {
    if (access$allowed) return(FALSE)
    notify("Only a collaborator on this app can approve directives.", "error")
    TRUE
  }

  observeEvent(input$submit_feedback, {
    text <- trimws(input$feedback_text %||% "")
    if (!nzchar(text)) return(notify("Write the correction first.", "warning"))
    if (is.null(guidance())) {
      return(notify("Initialize the guidance state on the directives tab first.", "warning"))
    }
    if (!identical(input$about, "report")) {
      text <- paste0("About \"", input$about, "\": ", text)
    }
    saved <- tryCatch(
      record_feedback(board, snapshot, text, current_user()),
      error = function(e) e
    )
    if (inherits(saved, "error")) return(notify(conditionMessage(saved), "error"))
    refresh()
    updateTextAreaInput(session, "feedback_text", value = "")
    notify("Saved as pending feedback. Nothing changes until it is approved as a directive.")
  })

  feedback_entries <- function(state) {
    stats::setNames(state$feedback, vapply(state$feedback, `[[`, character(1), "feedback_id"))
  }
  pending_entries <- function(state) {
    approved_from <- vapply(state$rules, `[[`, character(1), "source_feedback_id")
    Filter(function(entry) !entry$feedback_id %in% approved_from, state$feedback)
  }

  output$feedback_list <- renderUI({
    state <- guidance()
    if (is.null(state)) return(p(class = "text-muted-warm", "No guidance state on this board yet."))
    if (!length(state$feedback)) return(p(class = "text-muted-warm", "No corrections yet."))
    approved_from <- vapply(state$rules, `[[`, character(1), "source_feedback_id")
    lapply(rev(state$feedback), function(entry) {
      div(class = "accent-rule mb-3",
        p(class = "mb-1", entry$text),
        p(class = "small text-muted-warm mb-0",
          entry$author, ", ", entry$created_at,
          if (entry$feedback_id %in% approved_from) tags$span(class = "badge bg-secondary ms-2", "approved as a directive")
          else tags$span(class = "badge bg-light text-dark border ms-2", "pending"))
      )
    })
  })

  output$approval_ui <- renderUI({
    state <- guidance()
    if (is.null(state)) {
      return(tagList(
        p("This board has no guidance state. Initializing it records the first-run",
          "report as the source that corrections refer to. It approves nothing."),
        actionButton("initialize", "Initialize from the first-run report", class = "btn-outline-secondary")
      ))
    }
    directives <- if (length(state$rules)) {
      lapply(rev(state$rules), function(rule) {
        div(class = "accent-rule mb-3",
          p(class = "mb-1", rule$text),
          p(class = "small text-muted-warm mb-0",
            "Approved by ", rule$approved_by, ", ", rule$approved_at))
      })
    } else {
      p(class = "text-muted-warm", "None approved yet.")
    }
    pending <- pending_entries(state)
    approval <- if (!length(pending)) {
      p(class = "text-muted-warm", "Nothing pending. Save a correction on the first tab to approve it.")
    } else {
      # The state hash the approver is looking at. approve_directive() refuses
      # to approve against a state that has changed since.
      shown_state_id(report_hash(state))
      choices <- stats::setNames(
        vapply(pending, `[[`, character(1), "feedback_id"),
        vapply(pending, function(entry) {
          paste0(entry$author, ", ", substr(entry$created_at, 1, 10), ": ", substr(entry$text, 1, 60))
        }, character(1))
      )
      tagList(
        selectInput("source_feedback", "Pending correction", choices = choices, width = "100%"),
        textAreaInput("directive_text", "Approve as this directive", rows = 5,
          value = pending[[1]]$text, width = "100%"),
        actionButton("approve", "Approve directive", class = "btn-primary"),
        p(class = "small text-muted-warm mt-3",
          "The words in the box are what gets approved, under your name and the time;",
          "the correction itself stays as written. From the next run on, every report",
          "and chat is given this directive.")
      )
    }
    tagList(
      h6("Approved"), directives,
      hr(),
      h6("Pending"), approval
    )
  })

  observeEvent(input$source_feedback, {
    state <- guidance()
    req(state, input$source_feedback %in% names(feedback_entries(state)))
    updateTextAreaInput(session, "directive_text",
      value = feedback_entries(state)[[input$source_feedback]]$text)
  })

  observeEvent(input$initialize, {
    if (refused()) return()
    result <- tryCatch(
      initialize_guidance(board, initial_bad_report(snapshot), snapshot),
      error = function(e) e
    )
    if (inherits(result, "error")) return(notify(conditionMessage(result), "error"))
    refresh()
    notify("Guidance state initialized. No rule has been approved.")
  })

  observeEvent(input$approve, {
    if (refused()) return()
    result <- tryCatch(
      approve_directive(
        board, snapshot, input$source_feedback, current_user(), shown_state_id(),
        text = trimws(input$directive_text %||% "")
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      refresh()
      return(notify(conditionMessage(result), "error"))
    }
    refresh()
    notify("Directive approved. Every later report and chat is given it.")
  })

  output$archive <- renderText({
    if (is.null(guidance())) return("No guidance state on this board yet.")
    jsonlite::toJSON(context_archive(board, snapshot),
      auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE)
  })
}

shinyApp(ui, server)
