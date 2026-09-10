source(here::here("R", "report_contract.R"))
source(here::here("R", "theme.R"))
source(here::here("R", "charts.R"))

# Render the report's evidence chart and embed it, so the preview is one
# self-contained file with nothing to hotlink.
#
# Note this makes the HTML bytes machine-dependent, the same way the stills are:
# the embedded PNG depends on the local font rendering. Nothing asserts the
# bytes of either.
insight_chart_uri <- function(insight) {
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  ggplot2::ggsave(
    path, insight_evidence_chart(insight),
    width = 7.2, height = 2.9, dpi = 150, bg = INBOX_PALETTE$page
  )
  base64enc::dataURI(file = path, mime = "image/png")
}

email_version_label <- function(purpose) {
  switch(purpose,
    teaching_example = "First-run teaching example",
    weekly_email_first_run = "First run, before the correction",
    weekly_email = "After the approved correction",
    weekly_email_live = "Live model run",
    "Corrected preview"
  )
}

email_period <- function(value) paste(value$start, "through", value$end)

# The masthead and the lines under it. The lead insight supplies the dates,
# because every insight in one email shares a cutoff and a reporting period.
email_header <- function(report, lead_insight) {
  tags <- htmltools::tags
  st <- inbox_email_styles()
  logo <- base64enc::dataURI(
    file = inbox_path("images", "chickencloud-logo.png"), mime = "image/png"
  )
  tags$header(style = "display: block;",
    # A table, not a flex row: mail clients lay out tables and nothing else.
    tags$table(class = "masthead", style = st$masthead, role = "presentation",
      cellpadding = "0", cellspacing = "0", border = "0",
      tags$tr(
        tags$td(style = st$masthead_logo_cell,
          tags$img(src = logo, alt = report$company, width = 210, height = 54, style = st$logo)
        ),
        tags$td(class = "titles", style = st$masthead_titles_cell,
          tags$h1(style = st$h1, "Weekly inbox insights"),
          tags$p(class = "muted small", style = st$muted_small,
            tags$strong(email_version_label(report$purpose)),
            " \u00b7 Data as of ", lead_insight$data_as_of
          )
        )
      )
    ),
    tags$p(class = "muted small", style = paste("margin-top: 20px;", st$muted_small),
      "Reporting period: ", email_period(lead_insight$reporting_period), " (28 days)"
    ),
    tags$p(class = "muted small", style = st$muted_small,
      switch(report$purpose,
        teaching_example = "Authored teaching example; not a captured model run.",
        weekly_email_live = paste(
          "Live model run; not an authored example.",
          "Submitted for review, not reviewed."
        ),
        "Authored example; not a captured model run."
      )
    )
  )
}

# One insight: the finding, what it rests on, the caveat, and how to redo it.
# Shared by the single-insight previews and the weekly email so three insights
# cannot drift into three layouts.
email_insight_section <- function(insight) {
  tags <- htmltools::tags
  st <- inbox_email_styles()
  kind <- insight_kind(insight)
  percent <- function(value) sprintf("%.1f%%", 100 * value)
  count <- function(value) {
    format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  th_col <- function(...) tags$th(scope = "col", style = st$th_col, ...)
  th_row <- function(...) tags$th(scope = "row", style = st$th_row, ...)
  td <- function(...) tags$td(style = st$td, ...)
  header_row <- function(first, ...) {
    tags$thead(tags$tr(tags$th(scope = "col", style = st$th_col_first, first), lapply(c(...), th_col)))
  }

  evidence_table <- switch(kind,
    cohort_conversion = tags$table(class = "evidence", style = st$table,
      tags$caption(style = st$caption, if (insight$outcome_horizon == "30_days") {
        "Wins within 30 days of entry; every lead observed for at least 30 days."
      } else {
        "Wins known at the snapshot; observation ages differ between cohorts."
      }),
      header_row("Entry month", "Leads", "Wins", "Conversion", "Observed share"),
      tags$tbody(lapply(insight$evidence, function(row) {
        tags$tr(
          th_row(format(as.Date(row$cohort_month), "%Y-%m")),
          td(count(row$leads)), td(count(row$won)),
          td(percent(row$conversion)),
          td(percent(row$share_of_group_observed))
        )
      }))
    ),
    segment_timing = tags$table(class = "evidence", style = st$table,
      tags$caption(style = st$caption, paste(
        "Median days from entry to won, over wins with recorded stage dates.",
        "Deals not yet won are not counted, so these are lower bounds."
      )),
      header_row("Segment", "Leads", "Measured wins", "Median days to won"),
      tags$tbody(lapply(insight$evidence, function(row) {
        tags$tr(
          th_row(row$segment),
          td(count(row$leads)), td(count(row$measured_wins)),
          td(count(row$median_days_to_won))
        )
      }))
    ),
    stage_progression = tags$table(class = "evidence", style = st$table,
      tags$caption(style = st$caption, paste(
        "Leads reaching each stage by the cutoff.",
        "Later stages are understated because recent leads have not got there yet."
      )),
      header_row("Stage", "Leads", "Share of entered"),
      tags$tbody(lapply(insight$evidence, function(row) {
        tags$tr(
          th_row(row$stage),
          td(count(row$leads)), td(percent(row$share_of_entered))
        )
      }))
    )
  )

  tags$section(class = "insight", style = st$insight,
    tags$h2(style = st$h2, insight$title),
    tags$p(style = st$p, insight$finding),
    tags$div(class = "callout", style = st$callout,
      tags$p(style = st$callout_p, tags$strong("Suggested action. "), insight$suggested_action)
    ),

    tags$h3(class = "label", style = st$label, switch(kind,
      cohort_conversion = "Historical cohort evidence",
      segment_timing = "Segment evidence",
      stage_progression = "Funnel evidence"
    )),
    tags$p(class = "muted small", style = st$muted_small, paste0(
      "Entry dates: ", email_period(insight$evidence_window), ". ",
      if (identical(kind, "cohort_conversion")) {
        "These historical cohorts are separate from the reporting period."
      } else {
        "This evidence is separate from the 28-day reporting period."
      }
    )),
    tags$p(style = st$p, insight$metric_definition),
    # Drawn from insight$evidence, which validate_insight() has already checked
    # reproduces from the snapshot. The chart and the table below it are
    # therefore the same numbers by construction rather than by agreement.
    tags$img(
      class = "chart",
      style = st$chart,
      src = insight_chart_uri(insight),
      alt = insight_evidence_alt(insight)
    ),
    evidence_table,

    tags$div(class = "callout callout-quiet", style = st$callout_quiet,
      tags$p(style = st$callout_p, tags$strong("Caveat. "), insight$caveat)
    ),

    tags$h3(class = "label", style = st$label, "Reproduce the evidence"),
    tags$p(class = "muted small", style = st$muted_small,
      "Against the same frozen snapshot, using only dplyr and pins:"
    ),
    # .noWS is load-bearing here. htmltools indents child tags, and inside a
    # <pre> that indentation is content.
    tags$pre(.noWS = "inside", style = st$pre,
      tags$code(.noWS = "inside", style = st$code, insight$reproducible_code)
    )
  )
}

# A live insight's evidence has whatever shape the model chose, so it gets a
# generic table and no chart. Charting an arbitrary table without knowing what
# the columns mean would be decoration standing in for understanding.
#
# The caption and the code heading are worded for what actually happened: the
# table is the one the model saved from its session, and the code is what it
# said produced it. Neither was re-run by the runner.
live_insight_section <- function(insight) {
  tags <- htmltools::tags
  st <- inbox_email_styles()
  evidence <- insight$evidence
  columns <- unlist(evidence$value_columns)
  formatted <- format_evidence_table(evidence)

  tags$section(class = "insight", style = st$insight,
    tags$h2(style = st$h2, insight$title),
    tags$p(style = st$p, insight$finding),
    tags$div(class = "callout", style = st$callout,
      tags$p(style = st$callout_p, tags$strong("Suggested action. "), insight$suggested_action)
    ),
    tags$h3(class = "label", style = st$label, "Evidence"),
    tags$p(style = st$p, insight$metric_definition),
    tags$table(class = "evidence", style = st$table,
      tags$caption(style = st$caption, paste(
        "The table the model saved from its sandboxed session against the",
        "frozen snapshot. Money in dollars, shares as percentages, counts whole;",
        "the values themselves are as submitted."
      )),
      tags$thead(tags$tr(
        tags$th(scope = "col", style = st$th_col_first, evidence$label_column),
        lapply(columns, function(name) tags$th(scope = "col", style = st$th_col, name))
      )),
      tags$tbody(lapply(seq_along(evidence$rows), function(i) {
        tags$tr(
          tags$th(scope = "row", style = st$th_row, evidence$rows[[i]]$label),
          lapply(formatted, function(column) tags$td(style = st$td, column[[i]]))
        )
      }))
    ),
    tags$div(class = "callout callout-quiet", style = st$callout_quiet,
      tags$p(style = st$callout_p, tags$strong("Caveat. "), insight$caveat)
    ),
    tags$h3(class = "label", style = st$label, "Reproduce the evidence"),
    tags$p(class = "muted small", style = st$muted_small, paste(
      "The snapshot is pinned on the Connect server that sent this. Below the",
      "lines that load it is the model's account of how the table above was",
      "produced from it, as submitted and not re-run:"
    )),
    # The preamble is the runner's; only the code after the comment is the
    # model's. The comment marks the seam so nothing is attributed to the wrong
    # author.
    tags$pre(.noWS = "inside", style = st$pre,
      tags$code(.noWS = "inside", style = st$code, paste(
        "library(dplyr)",
        "library(pins)",
        "snapshot <- pin_read(board_connect(), \"funnel-snapshot\")",
        "",
        "# The model's code, as submitted:",
        insight$reproducible_code,
        sep = "\n"
      ))
    )
  )
}

live_email_html <- function(report, snapshot) {
  # Same gate as the authored renderers: a report from another snapshot, or
  # one whose strip no longer reproduces, is not rendered against this one.
  validate_live_report(report, snapshot)
  lead <- list(
    data_as_of = as.character(require_snapshot(snapshot)),
    reporting_period = worked_time_contract(snapshot)$reporting_period
  )
  email_document(
    report, lead,
    lapply(report$insights, live_insight_section),
    email_stat_strip(report$headline_metrics, headline_metric_windows(snapshot))
  )
}

write_live_email_preview <- function(report, snapshot, path) {
  write_preview_html(live_email_html(report, snapshot), path)
}

# Two ways back into the loop, placed under the numbers and above the
# headlines. Only when the apps have an address: see inbox_app_url().
email_banners <- function() {
  chat <- inbox_app_url("chat")
  feedback <- inbox_app_url("feedback")
  if (is.null(chat)) return(NULL)
  tags <- htmltools::tags
  st <- inbox_email_styles()
  htmltools::tagList(
    tags$div(class = "banner", style = st$banner,
      tags$p(style = st$callout_p,
        tags$strong("Have a question about these headlines? "),
        tags$a(href = chat, style = st$link, "Chat with this week's report"),
        ": the same snapshot and the same house recipes, with the answers computed in R."
      )
    ),
    tags$div(class = "banner banner-quiet", style = st$banner_quiet,
      tags$p(style = st$callout_p,
        tags$strong("Something wrong, or missing? "),
        tags$a(href = feedback, style = st$link, "Send a correction"),
        ". Approved corrections become standing directives for every later report."
      )
    )
  )
}

email_footer <- function(report) {
  tags <- htmltools::tags
  st <- inbox_email_styles()
  count <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  line <- function(...) tags$p(style = st$footer_p, ...)
  tags$footer(style = st$footer,
    line("Preview only. ", report$provenance$note),
    if (identical(report$provenance$kind, "live_model_run")) {
      line(paste0(
        "Written by ", report$provenance$model,
        " via ", report$provenance$provider,
        " at ", report$provenance$generated_at,
        ", using ", count(report$provenance$tokens$input), " input and ",
        count(report$provenance$tokens$output), " output tokens."
      ))
    },
    line("Report: ", report$report_id),
    line("Snapshot: ", report$snapshot_id),
    if (!is.null(report$archive_id)) line("Context archive: ", report$archive_id),
    if (length(report$applied_rule_ids)) {
      line("Applied rule: ", paste(unlist(report$applied_rule_ids), collapse = ", "))
    }
  )
}

# The deterministic strip: what happened in the reporting period, counted.
#
# No interpretation, no rate, no colour. Everything above the insights is
# arithmetic the reader can redo; everything below is a claim about it.
email_stat_strip <- function(metrics, windows) {
  tags <- htmltools::tags
  st <- inbox_email_styles()
  count <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

  htmltools::tagList(
    # One table row of four cells rather than a flex row, for the mail clients.
    tags$table(class = "stats", style = st$stats, role = "presentation",
      cellpadding = "0", cellspacing = "0", border = "0",
      tags$tr(lapply(seq_along(metrics), function(i) {
        row <- metrics[[i]]
        change <- if (row$previous_value > 0) {
          sprintf("%+.0f%%", 100 * (row$value / row$previous_value - 1))
        } else {
          "n/a"
        }
        tags$td(style = if (i == length(metrics)) st$stats_cell_last else st$stats_cell,
          tags$div(class = "stat", style = st$stat,
            tags$div(class = "k", style = st$stat_k, row$metric),
            tags$div(class = "v", style = st$stat_v, count(row$value)),
            tags$div(class = "d", style = st$stat_d, change, " vs ", count(row$previous_value), " prior")
          )
        )
      }))
    ),
    tags$p(class = "muted small", style = st$muted_small, paste0(
      "Events dated in the reporting period, against the 28 days before it ",
      "(", windows$prior_start, " to ", windows$prior_end, "). ",
      "Both windows are closed, so both are fully counted. ",
      "Counts, not conversion rates: a rate across cohorts of different ages ",
      "is the mistake this report exists to avoid. A small number of stage ",
      "dates were backfilled and are counted at their estimated date."
    ))
  )
}

email_document <- function(report, lead_insight, sections, metrics = NULL) {
  tags <- htmltools::tags
  st <- inbox_email_styles()
  tags$html(lang = "en",
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title(paste(
        report$company, email_version_label(report$purpose), sep = " - "
      )),
      tags$style(htmltools::HTML(inbox_preview_css()))
    ),
    tags$body(style = st$body,
      tags$div(class = "sheet", style = st$sheet,
        email_header(report, lead_insight),
        metrics,
        email_banners(),
        tags$main(style = "display: block;", email_sections(sections)),
        email_footer(report)
      )
    )
  )
}

# Successive insights are separated by a rule and space. The stylesheet does it
# with `.insight + .insight`; the inline copy has to know which section it is.
email_sections <- function(sections) {
  st <- inbox_email_styles()
  if (inherits(sections, "shiny.tag")) sections <- list(sections)
  lapply(seq_along(sections), function(i) {
    if (i == 1L) return(sections[[i]])
    htmltools::tagAppendAttributes(sections[[i]], style = st$insight_next)
  })
}

# What Connect sends, as opposed to what the browser shows.
#
# Mail clients drop `data:` images (Gmail strips them as an anti-abuse
# measure), so every embedded PNG becomes a CID attachment: the <img> points at
# `cid:image-N.png` and the image travels in rsc_email_images, which is how
# blastula delivers images on Connect. The styles are already inline, so the
# rest of the HTML goes as it is.
email_deliverable <- function(html) {
  text <- htmltools::doRenderTags(html)
  pattern <- 'src="data:image/png;base64,([^"]*)"'
  found <- regmatches(text, gregexpr(pattern, text))[[1]]
  images <- list()
  for (i in seq_along(found)) {
    cid <- paste0("image-", i, ".png")
    images[[cid]] <- sub(pattern, "\\1", found[[i]])
    text <- sub(found[[i]], paste0('src="cid:', cid, '"'), text, fixed = TRUE)
  }
  list(html = text, images = images)
}

example_email_html <- function(report, snapshot) {
  validate_example_report(report, snapshot)
  email_document(
    report, report$insight, email_insight_section(report$insight)
  )
}

weekly_email_html <- function(report, snapshot) {
  validate_weekly_report(report, snapshot)
  email_document(
    report, report$insights[[1]],
    lapply(report$insights, email_insight_section),
    email_stat_strip(report$headline_metrics, headline_metric_windows(snapshot))
  )
}

write_preview_html <- function(html, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("<!DOCTYPE html>", htmltools::doRenderTags(html)), path, useBytes = TRUE)
  invisible(path)
}

write_email_preview <- function(report, snapshot, path) {
  write_preview_html(example_email_html(report, snapshot), path)
}

write_weekly_email_preview <- function(report, snapshot, path) {
  write_preview_html(weekly_email_html(report, snapshot), path)
}
