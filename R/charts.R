source(here::here("prep", "build_cohort.R"))
source(here::here("R", "recipes.R"))
source(here::here("R", "theme.R"))

latest_complete_chart_month <- function(as_of) {
  current_month <- as.Date(format(as_of, "%Y-%m-01"))
  as.Date(format(current_month - 1, "%Y-%m-01"))
}

recent_complete_chart_months <- function(data) {
  months <- tail(sort(unique(data$cohort_month)), 4)
  dplyr::filter(data, cohort_month %in% months)
}

as_of_conversion_chart_data <- function(funnel_cohort, as_of = INBOX_AS_OF) {
  as_of <- valid_cutoff(as_of)
  snapshot <- funnel_snapshot(funnel_cohort, as_of = as_of)
  observed <- cohort_conversion(snapshot, as_of = as_of) |>
    dplyr::mutate(basis = "as_of", outcome_horizon = "snapshot")
  explanatory_cohort <- funnel_cohort |>
    dplyr::mutate(observation_age_days = as.numeric(as_of - entered_date))
  eventual <- cohort_conversion(
    explanatory_cohort, basis = "eventual", as_of = as_of
  ) |>
    dplyr::mutate(basis = "eventual", outcome_horizon = "eventual")

  dplyr::bind_rows(observed, eventual) |>
    recent_complete_chart_months() |>
    dplyr::mutate(
      company = INBOX_COMPANY,
      data_as_of = as_of,
      data_scope = "explanatory_only_not_for_reports",
      latest_complete_cohort = cohort_month == latest_complete_chart_month(as_of)
    ) |>
    dplyr::arrange(cohort_month, basis)
}

equal_30_day_chart_data <- function(snapshot) {
  if ("won_eventually" %in% names(snapshot)) {
    stop(
      "The 30-day chart needs a snapshot without hindsight. ",
      "Pass funnel_snapshot(cohort), not the full cohort.",
      call. = FALSE
    )
  }
  as_of <- require_snapshot(snapshot)
  complete <- cohort_conversion(snapshot, as_of = as_of) |>
    recent_complete_chart_months() |>
    dplyr::select(cohort_month, full_cohort_leads = leads)

  # Keep the recipe's original denominator until eligibility is measured.
  # A complete calendar month can still have only an older subset observed.
  cohort_conversion(snapshot, within_days = 30, as_of = as_of) |>
    dplyr::inner_join(complete, by = "cohort_month") |>
    dplyr::filter(share_of_group_observed == 1) |>
    dplyr::mutate(
      company = INBOX_COMPANY,
      data_as_of = as_of,
      basis = "as_of",
      outcome_horizon = "30_days",
      data_scope = "snapshot_only",
      latest_complete_cohort = cohort_month == latest_complete_chart_month(as_of)
    ) |>
    dplyr::arrange(cohort_month)
}

# A small chart of a report's own evidence, for embedding in the email.
#
# It draws from insight$evidence rather than recomputing from the snapshot, and
# that is the whole design. validate_insight() already asserts those rows
# reproduce from the snapshot, so the chart cannot disagree with the table
# printed beside it, and it cannot contain anything the report did not have --
# there is no path here through which hindsight could arrive.
#
# Which is the point. The explanatory chart on slide 7 plots won_eventually and
# is tagged explanatory_only_not_for_reports; it must never appear inside a
# report. This one is structurally incapable of showing that.
insight_evidence_chart <- function(insight) {
  pal <- inbox_chart_colours()
  data <- do.call(rbind, lapply(insight$evidence, function(row) {
    data.frame(
      cohort_month = as.Date(row$cohort_month),
      conversion = row$conversion,
      leads = row$leads,
      won = row$won
    )
  }))
  latest <- max(data$cohort_month)

  ggplot2::ggplot(data, ggplot2::aes(cohort_month, conversion)) +
    ggplot2::geom_rect(
      data = data.frame(cohort_month = latest),
      ggplot2::aes(xmin = cohort_month - 13, xmax = cohort_month + 13, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE, fill = pal$highlight, alpha = 0.25
    ) +
    ggplot2::geom_line(ggplot2::aes(group = 1), colour = pal$primary, linewidth = 0.7) +
    ggplot2::geom_point(colour = pal$primary, size = 2.1) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", 100 * conversion)),
      nudge_y = max(data$conversion) * 0.09, size = 3.6, colour = pal$text
    ) +
    ggplot2::scale_x_date(
      breaks = data$cohort_month, date_labels = "%b %Y",
      expand = ggplot2::expansion(add = 19)
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) sprintf("%.0f%%", 100 * x),
      limits = c(0, max(data$conversion) * 1.22),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      x = NULL,
      y = if (insight$outcome_horizon == "30_days") {
        "Wins within 30 days"
      } else {
        "Wins known at the cutoff"
      }
    ) +
    inbox_theme_ggplot(base_size = 12) +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 10, 2, 4))
}

# Alt text built from the same rows, so a screen reader gets the figures rather
# than being told there is a chart.
insight_evidence_alt <- function(insight) {
  parts <- vapply(insight$evidence, function(row) {
    sprintf("%s %.1f%%", format(as.Date(row$cohort_month), "%Y-%m"), 100 * row$conversion)
  }, character(1))
  paste0(
    if (insight$outcome_horizon == "30_days") {
      "Conversion within 30 days of entry by entry month: "
    } else {
      "Conversion known at the cutoff by entry month: "
    },
    paste(parts, collapse = ", "), "."
  )
}

chart_count <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

chart_subtitle <- function(data) {
  paste0(
    unique(data$company), " | Data as of ", unique(data$data_as_of),
    "\nComplete entry months | Latest complete month: ",
    format(latest_complete_chart_month(unique(data$data_as_of)), "%b %Y"),
    if (any(data$latest_complete_cohort)) " (shaded)" else " (not shown)"
  )
}

chart_month_scale <- function(data) {
  ggplot2::scale_x_date(
    breaks = sort(unique(data$cohort_month)),
    date_labels = "%b %Y",
    expand = ggplot2::expansion(add = 19)
  )
}

chart_highlight <- function(data) {
  highlighted <- data |>
    dplyr::filter(latest_complete_cohort) |>
    dplyr::distinct(cohort_month)
  ggplot2::geom_rect(
    data = highlighted,
    ggplot2::aes(
      xmin = cohort_month - 13, xmax = cohort_month + 13,
      ymin = -Inf, ymax = Inf
    ),
    inherit.aes = FALSE,
    # The one place the accent appears in a chart. The band marks the cohort
    # under discussion, which is exactly the "one thing that matters per view"
    # the accent is reserved for.
    fill = inbox_chart_colours()$highlight, alpha = 0.25
  )
}

plot_as_of_conversion <- function(data) {
  if (nrow(data) == 0) {
    stop("No complete entry months to plot.", call. = FALSE)
  }
  data <- data |>
    dplyr::mutate(
      series = factor(
        basis,
        levels = c("as_of", "eventual"),
        labels = c("Known at the cutoff", "Eventual: hindsight")
      ),
      label = paste0(
        sprintf("%.1f%%", 100 * conversion), "\n",
        chart_count(won), " / ", chart_count(leads)
      ),
      label_y = conversion + dplyr::if_else(basis == "eventual", 0.012, -0.012)
    )

  ggplot2::ggplot(
    data, ggplot2::aes(cohort_month, conversion, colour = series, group = series)
  ) +
    chart_highlight(data) +
    ggplot2::geom_line(ggplot2::aes(linetype = series), linewidth = 0.9) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(y = label_y, label = label), size = 4.5, show.legend = FALSE
    ) +
    chart_month_scale(data) +
    ggplot2::scale_y_continuous(
      labels = function(x) sprintf("%.0f%%", 100 * x),
      limits = c(0, max(data$conversion) + 0.03),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_colour_manual(
      # Ink for what was actually known; muted grey for the hindsight series,
      # which is explanatory and must not look like the headline.
      values = unname(unlist(inbox_chart_colours()[c("primary", "secondary")])),
      name = NULL
    ) +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
    ggplot2::labs(
      title = "Conversion at the cutoff and with hindsight",
      subtitle = chart_subtitle(data),
      x = "Entry month",
      y = "Wins / all leads entering the month",
      caption = paste(
        "Labels show conversion and wins / leads. Snapshot cohorts have unequal time to convert.",
        "Hindsight is explanatory only: unavailable to reports. Current partial entry month excluded.",
        sep = "\n"
      )
    ) +
    inbox_theme_ggplot()
}

plot_equal_30_day_conversion <- function(data) {
  if (nrow(data) == 0) {
    stop("No complete entry months with every lead observed for 30 days.", call. = FALSE)
  }
  data <- data |>
    dplyr::mutate(
      label = paste0(
        sprintf("%.1f%%", 100 * conversion), "\n",
        chart_count(won), " / ", chart_count(leads), " leads\n",
        sprintf("%.0f%%", 100 * share_of_group_observed), " of cohort eligible"
      )
    )

  ggplot2::ggplot(data, ggplot2::aes(cohort_month, conversion)) +
    chart_highlight(data) +
    ggplot2::geom_line(
      ggplot2::aes(group = 1),
      colour = inbox_chart_colours()$primary, linewidth = 0.9
    ) +
    ggplot2::geom_point(colour = inbox_chart_colours()$primary, size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = label), nudge_y = 0.009, size = 4.5,
      colour = inbox_chart_colours()$text
    ) +
    chart_month_scale(data) +
    ggplot2::scale_y_continuous(
      labels = function(x) sprintf("%.0f%%", 100 * x),
      limits = c(0, max(data$conversion) + 0.025),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = "Conversion within 30 days of entry",
      subtitle = chart_subtitle(data),
      x = "Entry month",
      y = "Wins within 30 days / all leads entering the month",
      caption = paste(
        "Snapshot only. Wins must occur within 30 days of entry; every lead must have at least 30 days of observation.",
        "Denominator: the full entry cohort. Partial months and partly eligible cohorts excluded. Not eventual conversion.",
        sep = "\n"
      )
    ) +
    inbox_theme_ggplot()
}
