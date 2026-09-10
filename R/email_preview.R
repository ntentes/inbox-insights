source(here::here("R", "report_contract.R"))
source(here::here("R", "theme.R"))

# The rendered email.
#
# Structure carries the hierarchy: a masthead, then the finding, then the
# evidence it rests on, then the caveat, then the code to reproduce it. Small
# uppercase labels open each section so the reader can find the evidence without
# reading the prose, which is the whole point of showing your work.
#
# All styling comes from inbox_preview_css() so the email, the approval replays
# and the charts cannot drift apart.
example_email_html <- function(report, snapshot) {
  validate_example_report(report, snapshot)
  tags <- htmltools::tags
  insight <- report$insight
  logo <- base64enc::dataURI(
    file = inbox_path("images", "chickencloud-logo.png"), mime = "image/png"
  )
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
      tags$style(htmltools::HTML(inbox_preview_css()))
    ),
    tags$body(
      tags$div(class = "sheet",
        tags$header(
          tags$div(class = "masthead",
            tags$img(
              src = logo, alt = report$company, width = 300, height = 77
            ),
            tags$div(class = "titles",
              tags$h1("Weekly inbox insights"),
              tags$p(class = "muted small",
                tags$strong(version), " \u00b7 Data as of ", insight$data_as_of
              )
            )
          ),
          tags$p(class = "muted small",
            "Reporting period: ", period(insight$reporting_period), " (28 days)"
          ),
          tags$p(class = "muted small",
            "Authored teaching example; not a captured model run."
          )
        ),
        tags$main(
          tags$section(class = "insight",
            tags$h2(insight$title),
            tags$p(insight$finding),
            tags$div(class = "callout",
              tags$p(tags$strong("Suggested action. "), insight$suggested_action)
            ),

            tags$h3(class = "label", "Historical cohort evidence"),
            # Built with paste0 rather than as separate children: htmltools puts
            # each child on its own line, and the browser renders that newline
            # as a space, which lands a gap before the full stop.
            tags$p(class = "muted small", paste0(
              "Entry dates: ", period(insight$evidence_window),
              ". These historical cohorts are separate from the reporting period."
            )),
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

            tags$div(class = "callout callout-quiet",
              tags$p(tags$strong("Caveat. "), insight$caveat)
            ),

            tags$h3(class = "label", "Reproduce the evidence"),
            tags$p(class = "muted small",
              "Using the same frozen snapshot and the house recipes in R/recipes.R:"
            ),
            # .noWS is load-bearing here. htmltools indents child tags, and
            # inside a <pre> that indentation is content -- it rendered the
            # first line of the snippet pushed halfway across the block.
            tags$pre(.noWS = "inside",
              tags$code(.noWS = "inside", insight$reproducible_code)
            )
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
