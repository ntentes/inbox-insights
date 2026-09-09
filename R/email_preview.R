source(here::here("R", "report_contract.R"))

example_email_html <- function(report, snapshot) {
  validate_example_report(report, snapshot)
  tags <- htmltools::tags
  insight <- report$insight
  version <- if (report$purpose == "teaching_example") {
    "First-run teaching example"
  } else {
    "Corrected preview"
  }
  period <- function(value) paste(value$start, "through", value$end)
  percent <- function(value) sprintf("%.1f%%", 100 * value)
  count <- function(value) format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
  rows <- lapply(insight$evidence, function(row) {
    tags$tr(
      tags$th(scope = "row", format(as.Date(row$cohort_month), "%Y-%m")),
      tags$td(count(row$leads)), tags$td(count(row$won)),
      tags$td(percent(row$conversion)), tags$td(percent(row$share_of_group_observed))
    )
  })
  tags$html(lang = "en",
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title(paste(report$company, version, sep = " - ")),
      tags$style(htmltools::HTML(paste(
        "body { font: 18px/1.5 system-ui, sans-serif; color: #222;",
        "max-width: 850px; margin: 2rem auto; padding: 0 1rem; }",
        "h1 { margin-bottom: 0; } h2 { line-height: 1.2; }",
        "table { border-collapse: collapse; width: 100%; }",
        "th, td { padding: .35rem .6rem; border-bottom: 1px solid #bbb; text-align: right; }",
        "th:first-child { text-align: left; }",
        "pre { white-space: pre-wrap; overflow-wrap: anywhere; background: #f4f4f4; padding: 1rem; }",
        "footer { border-top: 1px solid #bbb; margin-top: 2rem; font-size: .8em; overflow-wrap: anywhere; }"
      )))
    ),
    tags$body(
      tags$header(
        tags$h1(report$company),
        tags$p("Weekly inbox insights"),
        tags$p(tags$strong(version), " | Data as of ", insight$data_as_of),
        tags$p("Reporting period: ", period(insight$reporting_period), " (28 days)")
      ),
      tags$main(
        tags$section(class = "insight",
          tags$h2(insight$title),
          tags$p(insight$finding),
          tags$p(tags$strong("Suggested action: "), insight$suggested_action),
          tags$h3("Historical cohort evidence"),
          tags$p("Entry dates: ", period(insight$evidence_window),
            ". These historical cohorts are separate from the reporting period."),
          tags$p(insight$metric_definition),
          tags$table(
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
            tags$tbody(rows)
          ),
          tags$p(tags$strong("Caveat: "), insight$caveat),
          tags$h3("Reproduce the evidence"),
          tags$p("Using the same frozen snapshot and the house recipes in R/recipes.R:"),
          tags$pre(tags$code(insight$reproducible_code))
        )
      ),
      tags$footer(
        tags$p("Preview only. ", report$provenance$note),
        tags$p("Report: ", report$report_id),
        tags$p("Snapshot: ", report$snapshot_id),
        if (!is.null(report$archive_id)) tags$p("Context archive: ", report$archive_id),
        if (length(report$applied_rule_ids)) {
          tags$p("Applied rule: ", paste(unlist(report$applied_rule_ids), collapse = ", "))
        }
      )
    )
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
