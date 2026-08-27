# The house recipes.
#
# Five functions for the five things anybody actually asks this data. They exist
# because the alternative -- everyone writing their own group_by and summarise
# from scratch each time -- is how you end up with four numbers for one question
# and no way to tell which is right.
#
# The design rule is that the correct calculation should be the easy one, and the
# incorrect calculation should be difficult to reach by accident. So conversion is
# anchored on entry, durations respect the eligibility flags without being asked,
# grouping by an attribute that cannot support a full-funnel denominator is an
# error rather than a warning, and the multi-value column has one function whose
# name says out loud that it counts touches instead of leads.
#
# Every recipe reports its own denominator. A rate you cannot check is a rate
# nobody should have to trust.

library(dplyr)

# Grouping by one of these silently changes the question. The late_ columns are
# only populated for leads that got far enough down the funnel, so grouping a
# full-funnel denominator by one of them quietly restricts the denominator to
# leads that qualified. campaigns holds several values per lead, so grouping by
# it without splitting compares whole combinations, and splitting it first
# double-counts leads.
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

# Pick the outcome column, and refuse the combination that quietly produces a
# funnel nobody ever observed.
#
# A table carrying won_eventually is the full cohort, which means its stage dates
# run past the as-of date too. Reading won_as_of off it while reading the stage
# dates at face value mixes a censored outcome with uncensored progression: the
# won count is what was known in June and the qualified count includes leads that
# qualified in August. That is not a snapshot and not hindsight, it is neither.
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

  if (has_hindsight) {
    stop(
      "basis = \"as_of\" needs a snapshot, and this table still carries\n",
      "  won_eventually -- so its stage dates have not been censored either.\n",
      "  Counting censored outcomes against uncensored stage dates describes a\n",
      "  funnel that existed at no point in time.\n",
      "  Pass funnel_snapshot(cohort), or ask for basis = \"eventual\".",
      call. = FALSE
    )
  }
  if (!"won_as_of" %in% names(cohort)) {
    stop("This table has no won_as_of column. Was it built by build_funnel_cohort()?",
      call. = FALSE
    )
  }
  "won_as_of"
}

# --- Conversion -------------------------------------------------------------

# Entry-to-won conversion. The denominator is leads that entered, always, because
# that is the only denominator every lead qualifies for.
#
# within_days is the argument that matters, and it does two things at once
# on purpose. It drops leads that have not yet had that long to convert, and it
# stops counting wins that arrived later than that. Both halves are necessary. Do
# only the first and an eighteen-month-old cohort still gets eighteen months of
# wins while a four-month-old cohort gets four, so the series slopes downward with
# recency and the trap is intact. Measuring every cohort over the same window
# from entry is the only comparison that answers the question people think they
# are asking.
#
# Leave it NULL and you get the raw snapshot rate: correct as a statement about
# what has happened so far, and not comparable across cohorts of different ages.
#
# basis = "eventual" uses hindsight and is for the explanatory chart and the tests
# only. It is never available on a snapshot, because funnel_snapshot() drops the
# column it needs.
conversion_by <- function(cohort,
                          by = NULL,
                          within_days = NULL,
                          basis = c("as_of", "eventual")) {
  basis <- match.arg(basis)
  check_grouping(cohort, by)
  outcome <- outcome_column(cohort, basis)

  # Group sizes are recorded before the age filter, so the result can report how
  # much of each group survived it. Without that, a young cohort trimmed down to
  # a handful of its earliest leads reports a perfectly respectable-looking rate
  # on almost no data, and the reader has no way to notice.
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

# Conversion by entry month.
#
# complete_months_only defaults to TRUE because a partial month is not a cohort,
# it is a cohort in progress, and putting it on the same axis as finished months
# invites exactly the comparison this whole exercise is about avoiding.
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

# Median days from entry to a stage, over the leads whose dates for that stage
# were actually recorded rather than backfilled.
#
# The excluded count comes back alongside the median, unasked. If a quarter of
# the leads dropped out of the denominator, whoever reads the number should find
# that out at the same time as the number.
median_days_to <- function(cohort, stage = c("won", "opportunity", "qualified"), by = NULL) {
  stage <- match.arg(stage)
  check_grouping(cohort, by)

  duration <- paste0("days_to_", stage)
  eligibility <- paste0("use_for_time_to_", stage)

  cohort |>
    filter(!is.na(.data[[duration]])) |>
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
  # Every stage in the result has to come from the same table, so the guard here
  # is doing more work than picking a column name: it rejects the full cohort
  # under basis = "as_of", where the stage dates and the outcome disagree about
  # what date it is.
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

# The only sanctioned way to look at campaigns.
#
# It returns touches, not leads, and the column is named that way so the number
# cannot be quietly reused as a lead count. The totals do not sum to the number
# of leads and they are not supposed to: one lead touched by three campaigns is
# three touches. Anything phrased as "what share of our leads came from campaign
# X" has no answer in this data, and the honest response is to say so rather than
# to divide by something.
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
