source(here::here("prep", "build_cohort.R"))
source(here::here("R", "recipes.R"))

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
  eventual <- cohort_conversion(
    funnel_cohort, basis = "eventual", as_of = as_of
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
    inherit.aes = FALSE, fill = "grey90"
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
    ggplot2::scale_colour_manual(values = c("#3366A8", "#666666"), name = NULL) +
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
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, size = 12)
    )
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
    ggplot2::geom_line(ggplot2::aes(group = 1), colour = "#3366A8", linewidth = 0.9) +
    ggplot2::geom_point(colour = "#3366A8", size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = label), nudge_y = 0.009, size = 4.5
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
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, size = 12)
    )
}
