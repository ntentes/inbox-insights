# The house recipes.
#
# Five functions for the five things people actually ask this data. Without
# them everyone writes their own group_by and summarise, and one question ends
# up with four numbers and no way to tell which is right.
#
# The design rule: the correct calculation is the easy one, and the incorrect
# one is hard to reach by accident. Conversion is anchored on entry. Durations
# respect the eligibility flags without being asked. Grouping by an attribute
# that cannot support a full-funnel denominator is an error, not a warning. The
# multi-value column has one function whose name says it counts touches, not
# leads. Every recipe reports its own denominator, because a rate you cannot
# check is a rate nobody should have to trust.

library(dplyr)

# require_snapshot() and the cutoff contract it enforces.
source(here::here("R", "snapshot.R"))

# Grouping by one of these silently changes the question. late_ columns are
# only populated for leads that got far enough down the funnel, so grouping by
# one restricts a full-funnel denominator to leads that qualified. campaigns
# holds several values per lead: grouping without splitting compares whole
# combinations, and splitting first double-counts leads.
unsafe_grouping_columns <- function(cohort) {
  c(grep("^late_", names(cohort), value = TRUE), "campaigns")
}

check_grouping <- function(cohort, by) {
  unsafe <- intersect(by, unsafe_grouping_columns(cohort))
  if (length(unsafe) == 0) {
    return(invisible(NULL))
  }
  stop(
    "Cannot group a full-funnel metric by: ", paste(unsafe, collapse = ", "),
    ".\n",
    "  late_* columns are captured partway down the funnel, so they are blank\n",
    "  for leads that stopped earlier -- the denominator would silently become\n",
    "  'leads that qualified'. Group by channel, company_size or region, which\n",
    "  are assigned at entry.\n",
    "  For campaigns, use campaign_reach(), which counts touches rather than\n",
    "  pretending each lead belongs to one campaign.",
    call. = FALSE
  )
}

# Pick the outcome column, and refuse the combination that produces a funnel
# nobody observed. A table with won_eventually is the full cohort, so its stage
# dates run past the as-of date too. Reading won_as_of off it mixes a censored
# outcome with uncensored progression: the won count is what was known in June,
# the qualified count includes leads that qualified in August. That is neither
# a snapshot nor hindsight.
outcome_column <- function(cohort, basis) {
  has_hindsight <- "won_eventually" %in% names(cohort)

  if (basis == "eventual") {
    if (!has_hindsight) {
      stop(
        "basis = \"eventual\" needs the full cohort table, which has a\n",
        "  won_eventually column. A snapshot has it removed on purpose, because\n",
        "  the agent must never see outcomes from after the as-of date.",
        call. = FALSE
      )
    }
    return("won_eventually")
  }

  require_snapshot(cohort)
  if (!"won_as_of" %in% names(cohort)) {
    stop("This table has no won_as_of column. Was it built by build_funnel_cohort()?",
      call. = FALSE
    )
  }
  "won_as_of"
}

# --- Conversion -------------------------------------------------------------

# Entry-to-won conversion. The denominator is always leads that entered,
# because that is the only denominator every lead qualifies for.
#
# within_days does two things on purpose. It drops leads that have not yet had
# that long to convert, and it stops counting wins that arrived later. Both
# halves are needed. Do only the first and an eighteen-month-old cohort still
# gets eighteen months of wins while a four-month-old cohort gets four, so the
# series slopes downward with recency. Measuring every cohort over the same
# window from entry is the only comparison that answers the question people
# mean to ask. NULL gives the raw snapshot rate: correct about what has
# happened so far, not comparable across cohorts of different ages.
#
# basis = "eventual" uses hindsight and is for the explanatory chart and the
# tests only. It is never available on a snapshot, because funnel_snapshot()
# drops the column it needs.
conversion_by <- function(cohort,
                          by = NULL,
                          within_days = NULL,
                          basis = c("as_of", "eventual")) {
  basis <- match.arg(basis)
  check_grouping(cohort, by)
  outcome <- outcome_column(cohort, basis)

  # Group sizes are recorded before the age filter, so the result can report
  # how much of each group survived it. Otherwise a young cohort trimmed to a
  # handful of early leads reports a respectable rate on almost no data, and
  # the reader has no way to notice.
  eligible <- mutate(cohort, .group_leads = n(), .by = all_of(by))

  if (is.null(within_days)) {
    eligible <- mutate(eligible, .won = .data[[outcome]])
  } else {
    eligible <- eligible |>
      filter(observation_age_days >= within_days) |>
      mutate(
        .won = .data[[outcome]] &
          !is.na(days_to_won) &
          days_to_won <= within_days
      )
  }

  result <- eligible |>
    summarise(
      leads = n(),
      won = sum(.won),
      conversion = mean(.won),
      youngest_lead_days = min(observation_age_days),
      share_of_group_observed = leads / max(.group_leads),
      .by = all_of(by)
    ) |>
    arrange(across(all_of(by)))

  if (is.null(within_days)) {
    result <- select(result, -share_of_group_observed)
  }
  result
}

# Conversion by entry month. complete_months_only defaults to TRUE because a
# partial month is a cohort in progress, and putting it beside finished months
# invites the comparison this whole exercise exists to avoid.
cohort_conversion <- function(cohort,
                              complete_months_only = TRUE,
                              within_days = NULL,
                              basis = c("as_of", "eventual"),
                              as_of = INBOX_AS_OF) {
  if (complete_months_only) {
    current_month <- as.Date(format(as_of, "%Y-%m-01"))
    cohort <- filter(cohort, cohort_month < current_month)
  }
  conversion_by(
    cohort,
    by = "cohort_month",
    within_days = within_days,
    basis = basis
  )
}

# --- Durations --------------------------------------------------------------

# Median days from entry to a stage, over leads whose dates for that stage were
# recorded rather than backfilled. The excluded count comes back alongside the
# median, unasked: if a quarter of the leads dropped out of the denominator,
# the reader should learn that with the number.
median_days_to <- function(cohort, stage = c("won", "opportunity", "qualified"), by = NULL) {
  stage <- match.arg(stage)
  check_grouping(cohort, by)

  duration <- paste0("days_to_", stage)
  eligibility <- paste0("use_for_time_to_", stage)
  stage_date <- paste0(stage, "_date")

  # Reaching the stage is decided by the stage date, not by whether a duration
  # could be computed from it. Filtering on the duration first would drop the
  # backdated records (qualification before arrival) before they could be
  # counted as excluded. The records the exclusion count most needs to report
  # would vanish from both sides of it.
  cohort |>
    filter(!is.na(.data[[stage_date]])) |>
    summarise(
      reached = n(),
      eligible = sum(.data[[eligibility]]),
      excluded = sum(!.data[[eligibility]]),
      median_days = stats::median(
        .data[[duration]][.data[[eligibility]]],
        na.rm = TRUE
      ),
      .by = all_of(by)
    ) |>
    arrange(across(all_of(by)))
}

# --- Stage counts -----------------------------------------------------------

# How many leads had reached each stage at the snapshot. Long rather than wide,
# because the next thing anyone does with this is plot it.
stage_funnel <- function(cohort, by = NULL, basis = c("as_of", "eventual")) {
  basis <- match.arg(basis)
  check_grouping(cohort, by)
  # Every stage has to come from the same table, so the guard does more than
  # pick a column name: it rejects the full cohort under basis = "as_of", where
  # the stage dates and the outcome disagree about what date it is.
  outcome <- outcome_column(cohort, basis)

  cohort |>
    summarise(
      entered = n(),
      qualified = sum(!is.na(qualified_date)),
      opportunity = sum(!is.na(opportunity_date)),
      won = sum(.data[[outcome]]),
      .by = all_of(by)
    ) |>
    tidyr::pivot_longer(
      c(entered, qualified, opportunity, won),
      names_to = "stage",
      values_to = "leads"
    ) |>
    mutate(stage = factor(
      stage,
      levels = c("entered", "qualified", "opportunity", "won")
    )) |>
    arrange(across(all_of(by)), stage)
}

# --- The multi-value column -------------------------------------------------

# The only sanctioned way to look at campaigns. It returns touches, not leads,
# and the column is named so the number cannot be reused as a lead count. The
# totals do not sum to the number of leads: one lead touched by three campaigns
# is three touches. "What share of our leads came from campaign X" has no answer
# in this data. Say so rather than divide by something.
campaign_reach <- function(cohort) {
  cohort |>
    select(lead_id, campaigns, won_as_of) |>
    tidyr::separate_longer_delim(campaigns, ", ") |>
    summarise(
      touches = n(),
      won_touches = sum(won_as_of),
      .by = campaigns
    ) |>
    rename(campaign = campaigns) |>
    mutate(
      leads_in_table = nrow(cohort),
      touches_per_lead = round(sum(touches) / nrow(cohort), 2)
    ) |>
    arrange(desc(touches))
}
