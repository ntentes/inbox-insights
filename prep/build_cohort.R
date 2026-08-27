# Raw funnel table -> funnel_cohort, the one intermediate table everything else
# reads.
#
# Two jobs. The first is cleaning: resolve the duplicate id, backfill the skipped
# checkpoints, and refuse to emit a negative duration. The second, and the more
# important one, is to be honest about what the cleaning cost -- every repair
# leaves a flag behind saying so, because a backfilled date is a guess and a
# metric computed from a guess should be able to opt out.
#
# It also draws the line the whole talk depends on. won_as_of is what was known
# at the snapshot; won_eventually is what happened later. The agent only ever
# sees the first. The second exists for the explanatory chart and for the tests,
# and funnel_snapshot() is what enforces that separation.

library(dplyr)

source(here::here("prep", "generate_data.R"))

# --- Cleaning ---------------------------------------------------------------

# The generator puts one lead id on two different rows. Keep the earlier entry
# and drop the later one, deterministically, so downstream code can rely on
# lead_id being a key. Silently allowing the duplicate through would double-count
# one lead in every denominator.
deduplicate_leads <- function(funnel_raw) {
  funnel_raw |>
    arrange(entered_date, lead_id) |>
    distinct(lead_id, .keep_all = TRUE)
}

# Fill a missing intermediate stage date with the midpoint of the interval it has
# to sit inside. The value is defensible for ordering and useless for duration,
# which is precisely why it gets flagged rather than just filled.
midpoint_date <- function(earlier, later) {
  earlier + floor(as.numeric(later - earlier) / 2)
}

backfill_skipped_checkpoints <- function(leads) {
  leads |>
    mutate(
      qualified_date_backfilled = is.na(qualified_date) &
        (!is.na(opportunity_date) | !is.na(won_date)),
      qualified_date = if_else(
        qualified_date_backfilled,
        midpoint_date(entered_date, coalesce(opportunity_date, won_date)),
        qualified_date
      )
    ) |>
    mutate(
      opportunity_date_backfilled = is.na(opportunity_date) & !is.na(won_date),
      opportunity_date = if_else(
        opportunity_date_backfilled,
        midpoint_date(qualified_date, won_date),
        opportunity_date
      )
    )
}

# A duration is only reported when it is non-negative. The two backdated
# qualifications and the won-before-opportunity record would otherwise produce
# negative days, and a negative duration averaged into a median is worse than a
# missing one.
non_negative_days <- function(from, to) {
  days <- as.numeric(to - from)
  if_else(!is.na(days) & days >= 0, days, NA_real_)
}

# --- The cohort table -------------------------------------------------------

build_funnel_cohort <- function(funnel_raw = read_funnel_raw(), as_of = INBOX_AS_OF) {
  funnel_raw |>
    deduplicate_leads() |>
    backfill_skipped_checkpoints() |>
    mutate(
      cohort_month = as.Date(format(entered_date, "%Y-%m-01")),
      observation_age_days = as.numeric(as_of - entered_date),

      # The line the talk is about. won_as_of is the snapshot; won_eventually is
      # hindsight. Anything shown to the agent must be built from the first.
      won_as_of = !is.na(won_date) & won_date <= as_of,
      won_eventually = !is.na(won_date),

      days_to_qualified = non_negative_days(entered_date, qualified_date),
      days_to_opportunity = non_negative_days(entered_date, opportunity_date),
      days_to_won = non_negative_days(entered_date, won_date),

      days_qualified_to_opportunity =
        non_negative_days(qualified_date, opportunity_date),
      days_opportunity_to_won = non_negative_days(opportunity_date, won_date),

      # Eligibility for time-based metrics. A lead is eligible only when every
      # date the duration depends on was actually recorded -- not guessed -- and
      # every step along the way came out non-negative.
      #
      # The flags chain deliberately. Reaching won through an unrecorded
      # qualification says nothing trustworthy about how long the deal took, even
      # though entered-to-won subtracts to a perfectly plausible number. The
      # chaining is also what catches the won-before-opportunity record: its
      # entered-to-won duration is positive and completely fictional.
      use_for_time_to_qualified = !qualified_date_backfilled &
        !is.na(days_to_qualified),
      use_for_time_to_opportunity = use_for_time_to_qualified &
        !opportunity_date_backfilled &
        !is.na(days_to_opportunity) &
        !is.na(days_qualified_to_opportunity),
      use_for_time_to_won = use_for_time_to_opportunity &
        !is.na(days_to_won) &
        !is.na(days_opportunity_to_won)
    ) |>
    # The late_ prefix is deliberate and load-bearing. These three are captured
    # partway down the funnel, so they are blank for every lead that stopped
    # earlier, and grouping a full-funnel denominator by one of them is a bug.
    # Naming them so that the hazard travels with the column beats documenting
    # it somewhere the reader will not be looking.
    rename(
      late_industry = industry,
      late_deal_value = deal_value,
      late_competitor = competitor
    ) |>
    select(
      lead_id,
      cohort_month,
      entered_date,
      qualified_date,
      opportunity_date,
      won_date,
      lost_date,
      qualified_date_backfilled,
      opportunity_date_backfilled,
      observation_age_days,
      won_as_of,
      won_eventually,
      days_to_qualified,
      days_to_opportunity,
      days_to_won,
      days_qualified_to_opportunity,
      days_opportunity_to_won,
      use_for_time_to_qualified,
      use_for_time_to_opportunity,
      use_for_time_to_won,
      channel,
      company_size,
      region,
      campaigns,
      late_industry,
      late_deal_value,
      late_competitor
    ) |>
    arrange(lead_id)
}

# --- The snapshot the agent sees --------------------------------------------

# Everything the agent is allowed to look at goes through here. Hindsight columns
# are dropped, and any date after the as-of date is blanked -- a won date sitting
# in the future is just as much of a leak as won_eventually is, and easier to
# miss.
COHORT_HINDSIGHT_COLUMNS <- "won_eventually"

funnel_snapshot <- function(funnel_cohort, as_of = INBOX_AS_OF) {
  censor <- function(x) if_else(!is.na(x) & x <= as_of, x, as.Date(NA))

  funnel_cohort |>
    filter(entered_date <= as_of) |>
    mutate(across(
      c(qualified_date, opportunity_date, won_date, lost_date),
      censor
    )) |>
    mutate(
      # Durations have to be recomputed after censoring. Carrying the original
      # days_to_won through would leak the future outcome as a number even with
      # the date blanked out.
      days_to_qualified = non_negative_days(entered_date, qualified_date),
      days_to_opportunity = non_negative_days(entered_date, opportunity_date),
      days_to_won = non_negative_days(entered_date, won_date),
      days_qualified_to_opportunity =
        non_negative_days(qualified_date, opportunity_date),
      days_opportunity_to_won = non_negative_days(opportunity_date, won_date),
      use_for_time_to_qualified = use_for_time_to_qualified &
        !is.na(days_to_qualified),
      use_for_time_to_opportunity = use_for_time_to_opportunity &
        !is.na(days_to_opportunity),
      use_for_time_to_won = use_for_time_to_won & !is.na(days_to_won)
    ) |>
    select(-all_of(COHORT_HINDSIGHT_COLUMNS))
}

# --- Serialisation ----------------------------------------------------------

inbox_funnel_cohort_path <- function() inbox_data_path("funnel_cohort.csv")

write_funnel_cohort <- function(funnel_cohort, path = inbox_funnel_cohort_path()) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(funnel_cohort, path, na = "")
  invisible(path)
}

read_funnel_cohort <- function(path = inbox_funnel_cohort_path()) {
  if (!file.exists(path)) {
    stop(
      "No cohort table at ", path, ".\n",
      "Run prep/build_cohort.R first, or prep/build_all.R for everything."
    )
  }
  readr::read_csv(path, col_types = readr::cols(), progress = FALSE)
}

if (sys.nframe() == 0L) {
  funnel_cohort <- build_funnel_cohort()
  path <- write_funnel_cohort(funnel_cohort)
  message("Wrote ", nrow(funnel_cohort), " leads to ", path)
}
