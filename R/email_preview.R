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
  logo <- base64enc::dataURI(
    file = inbox_path("images", "chickencloud-logo.png"), mime = "image/png"
  )
  tags$header(
    tags$div(class = "masthead",
      tags$img(src = logo, alt = report$company, width = 300, height = 77),
      tags$div(class = "titles",
        tags$h1("Weekly inbox insights"),
        tags$p(class = "muted small",
          tags$strong(email_version_label(report$purpose)),
          " \u00b7 Data as of ", lead_insight$data_as_of
        )
      )
    ),
    tags$p(class = "muted small",
      "Reporting period: ", email_period(lead_insight$reporting_period), " (28 days)"
    ),
    tags$p(class = "muted small",
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
  kind <- insight_kind(insight)
  percent <- function(value) sprintf("%.1f%%", 100 * value)
  count <- function(value) {
    format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  evidence_table <- switch(kind,
    cohort_conversion = tags$table(
      tags$caption(if (insight$outcome_horizon == "30_days") {
        "Wins within 30 days of entry; every lead observed for at least 30 days."
      } else {
        "Wins known at the snapshot; observation ages differ between cohorts."
      }),
      tags$thead(tags$tr(
        tags$th(scope = "col", "Entry month"), tags$th(scope = "col", "Leads"),
        tags$th(scope = "col", "Wins"), tags$th(scope = "col", "Conversion"),
        tags$th(scope = "col", "Observed share")
      )),
      tags$tbody(lapply(insight$evidence, function(row) {
        tags$tr(
          tags$th(scope = "row", format(as.Date(row$cohort_month), "%Y-%m")),
          tags$td(count(row$leads)), tags$td(count(row$won)),
          tags$td(percent(row$conversion)),
          tags$td(percent(row$share_of_group_observed))
        )
      }))
    ),
    segment_timing = tags$table(
      tags$caption(paste(
        "Median days from entry to won, over wins with recorded stage dates.",
        "Deals not yet won are not counted, so these are lower bounds."
      )),
      tags$thead(tags$tr(
        tags$th(scope = "col", "Segment"), tags$th(scope = "col", "Leads"),
        tags$th(scope = "col", "Measured wins"),
        tags$th(scope = "col", "Median days to won")
      )),
      tags$tbody(lapply(insight$evidence, function(row) {
        tags$tr(
          tags$th(scope = "row", row$segment),
          tags$td(count(row$leads)), tags$td(count(row$measured_wins)),
          tags$td(count(row$median_days_to_won))
        )
      }))
    ),
    stage_progression = tags$table(
      tags$caption(paste(
        "Leads reaching each stage by the cutoff.",
        "Later stages are understated because recent leads have not got there yet."
      )),
      tags$thead(tags$tr(
        tags$th(scope = "col", "Stage"), tags$th(scope = "col", "Leads"),
        tags$th(scope = "col", "Share of entered")
      )),
      tags$tbody(lapply(insight$evidence, function(row) {
        tags$tr(
          tags$th(scope = "row", row$stage),
          tags$td(count(row$leads)), tags$td(percent(row$share_of_entered))
        )
      }))
    )
  )

  tags$section(class = "insight",
    tags$h2(insight$title),
    tags$p(insight$finding),
    tags$div(class = "callout",
      tags$p(tags$strong("Suggested action. "), insight$suggested_action)
    ),

    tags$h3(class = "label", switch(kind,
      cohort_conversion = "Historical cohort evidence",
      segment_timing = "Segment evidence",
      stage_progression = "Funnel evidence"
    )),
    tags$p(class = "muted small", paste0(
      "Entry dates: ", email_period(insight$evidence_window), ". ",
      if (identical(kind, "cohort_conversion")) {
        "These historical cohorts are separate from the reporting period."
      } else {
        "This evidence is separate from the 28-day reporting period."
      }
    )),
    tags$p(insight$metric_definition),
    # Drawn from insight$evidence, which validate_insight() has already checked
    # reproduces from the snapshot. The chart and the table below it are
    # therefore the same numbers by construction rather than by agreement.
    tags$img(
      class = "chart",
      src = insight_chart_uri(insight),
      alt = insight_evidence_alt(insight)
    ),
    evidence_table,

    tags$div(class = "callout callout-quiet",
      tags$p(tags$strong("Caveat. "), insight$caveat)
    ),

    tags$h3(class = "label", "Reproduce the evidence"),
    tags$p(class = "muted small",
      "Against the same frozen snapshot, using only dplyr and pins:"
    ),
    # .noWS is load-bearing here. htmltools indents child tags, and inside a
    # <pre> that indentation is content.
    tags$pre(.noWS = "inside",
      tags$code(.noWS = "inside", insight$reproducible_code)
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
  evidence <- insight$evidence
  columns <- unlist(evidence$value_columns)
  number <- function(x) {
    format(round(as.numeric(x), 4), big.mark = ",", trim = TRUE, scientific = FALSE)
  }

  tags$section(class = "insight",
    tags$h2(insight$title),
    tags$p(insight$finding),
    tags$div(class = "callout",
      tags$p(tags$strong("Suggested action. "), insight$suggested_action)
    ),
    tags$h3(class = "label", "Evidence"),
    tags$p(insight$metric_definition),
    tags$table(
      tags$caption(paste(
        "The table the model saved from its sandboxed session against the",
        "frozen snapshot, shown as submitted."
      )),
      tags$thead(tags$tr(
        tags$th(scope = "col", evidence$label_column),
        lapply(columns, function(name) tags$th(scope = "col", name))
      )),
      tags$tbody(lapply(evidence$rows, function(row) {
        tags$tr(
          tags$th(scope = "row", row$label),
          lapply(unlist(row$values), function(v) tags$td(number(v)))
        )
      }))
    ),
    tags$div(class = "callout callout-quiet",
      tags$p(tags$strong("Caveat. "), insight$caveat)
    ),
    tags$h3(class = "label", "Reproduce the evidence"),
    tags$p(class = "muted small",
      "The model's account of how the table above was produced from `snapshot`:"
    ),
    tags$pre(.noWS = "inside",
      tags$code(.noWS = "inside", insight$reproducible_code)
    )
  )
}

live_email_html <- function(report, snapshot) {
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

email_footer <- function(report) {
  tags <- htmltools::tags
  tags$footer(
    tags$p("Preview only. ", report$provenance$note),
    if (identical(report$provenance$kind, "live_model_run")) {
      tags$p(
        "Written by ", report$provenance$model,
        " via ", report$provenance$provider,
        " at ", report$provenance$generated_at, "."
      )
    },
    tags$p("Report: ", report$report_id),
    tags$p("Snapshot: ", report$snapshot_id),
    if (!is.null(report$archive_id)) tags$p("Context archive: ", report$archive_id),
    if (length(report$applied_rule_ids)) {
      tags$p("Applied rule: ", paste(unlist(report$applied_rule_ids), collapse = ", "))
    }
  )
}

# The deterministic strip: what happened in the reporting period, counted.
#
# No interpretation, no rate, no colour. Everything above the insights is
# arithmetic the reader can redo; everything below is a claim about it.
email_stat_strip <- function(metrics, windows) {
  tags <- htmltools::tags
  count <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

  htmltools::tagList(
    tags$div(class = "stats", lapply(metrics, function(row) {
      change <- if (row$previous_value > 0) {
        sprintf("%+.0f%%", 100 * (row$value / row$previous_value - 1))
      } else {
        "n/a"
      }
      tags$div(class = "stat",
        tags$div(class = "k", row$metric),
        tags$div(class = "v", count(row$value)),
        tags$div(class = "d", change, " vs ", count(row$previous_value), " prior")
      )
    })),
    tags$p(class = "muted small", paste0(
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
  tags$html(lang = "en",
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title(paste(
        report$company, email_version_label(report$purpose), sep = " - "
      )),
      tags$style(htmltools::HTML(inbox_preview_css()))
    ),
    tags$body(
      tags$div(class = "sheet",
        email_header(report, lead_insight),
        metrics,
        tags$main(sections),
        email_footer(report)
      )
    )
  )
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
