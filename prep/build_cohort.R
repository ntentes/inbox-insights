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

# Undo the backfill, recovering the dates as they were actually recorded. The
# flags make this exact rather than approximate: a flagged date was NA in the
# source, so blanking it restores the source precisely.
#
# This exists for funnel_snapshot(), which has to censor the *recorded* dates. A
# date invented from a future opportunity is future knowledge however ordinary it
# looks, so it has to come off before the censoring and be recomputed from
# whatever was observable afterwards.
restore_recorded_dates <- function(leads) {
  leads |>
    mutate(
      qualified_date = if_else(
        qualified_date_backfilled,
        as.Date(NA),
        qualified_date
      ),
      opportunity_date = if_else(
        opportunity_date_backfilled,
        as.Date(NA),
        opportunity_date
      )
    ) |>
    select(-qualified_date_backfilled, -opportunity_date_backfilled)
}

# A duration is only reported when it is non-negative. The two backdated
# qualifications and the won-before-opportunity record would otherwise produce
# negative days, and a negative duration averaged into a median is worse than a
# missing one.
non_negative_days <- function(from, to) {
  days <- as.numeric(to - from)
  if_else(!is.na(days) & days >= 0, days, NA_real_)
}

# --- Derived fields ---------------------------------------------------------

# Everything that follows from the stage dates. Factored out because the snapshot
# has to recompute all of it after censoring, and recomputing it by hand in two
# places is how two definitions of the same column drift apart.
#
# The rule this function obeys: every column here is derivable from the dates in
# front of it. A derived value that could only have come from a date since
# censored is a leak wearing a number's clothing.
derive_cohort_fields <- function(leads, as_of) {
  leads |>
    mutate(
      cohort_month = as.Date(format(entered_date, "%Y-%m-01")),
      observation_age_days = as.numeric(as_of - entered_date),
      won_as_of = !is.na(won_date) & won_date <= as_of,

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
    )
}

# The column order of funnel_cohort, in one place, so the cohort and the snapshot
# cannot quietly disagree about it.
COHORT_HINDSIGHT_COLUMNS <- "won_eventually"

COHORT_COLUMNS <- c(
  "lead_id",
  "cohort_month",
  "entered_date",
  "qualified_date",
  "opportunity_date",
  "won_date",
  "lost_date",
  "qualified_date_backfilled",
  "opportunity_date_backfilled",
  "observation_age_days",
  "won_as_of",
  "won_eventually",
  "days_to_qualified",
  "days_to_opportunity",
  "days_to_won",
  "days_qualified_to_opportunity",
  "days_opportunity_to_won",
  "use_for_time_to_qualified",
  "use_for_time_to_opportunity",
  "use_for_time_to_won",
  "channel",
  "company_size",
  "region",
  "campaigns",
  "late_industry",
  "late_deal_value",
  "late_competitor"
)

# A snapshot carries its own cutoff. This is what lets a downstream metric check
# that it was given censored data instead of inferring it from the absence of a
# column, which any stray select() can arrange.
SNAPSHOT_COLUMNS <- c(
  setdiff(COHORT_COLUMNS, COHORT_HINDSIGHT_COLUMNS),
  "snapshot_as_of"
)

# --- The cohort table -------------------------------------------------------

build_funnel_cohort <- function(funnel_raw = read_funnel_raw(), as_of = INBOX_AS_OF) {
  funnel_raw |>
    deduplicate_leads() |>
    # The late_ prefix is deliberate and load-bearing. These three are captured
    # partway down the funnel, so they are blank for every lead that stopped
    # earlier, and grouping a full-funnel denominator by one of them is a bug.
    # Naming them so the hazard travels with the column beats documenting it
    # somewhere the reader will not be looking.
    rename(
      late_industry = industry,
      late_deal_value = deal_value,
      late_competitor = competitor
    ) |>
    backfill_skipped_checkpoints() |>
    mutate(won_eventually = !is.na(won_date)) |>
    derive_cohort_fields(as_of) |>
    select(all_of(COHORT_COLUMNS)) |>
    arrange(lead_id)
}

# --- The snapshot the agent sees --------------------------------------------

# Blank everything that was not observable at the as-of date.
#
# The dates are the obvious half. The late attributes are the half that is easy
# to miss: late_industry is recorded when a lead qualifies, so a lead that
# qualified in July has no industry as far as a June snapshot is concerned, even
# though the cohort table has one sitting right there. Leaving it in tells the
# agent which leads were about to progress.
censor_unobservable <- function(leads, as_of) {
  observed <- function(x) !is.na(x) & x <= as_of
  observed_date <- function(x) if_else(observed(x), x, as.Date(NA))

  # Visibility is decided from the recorded capture dates, before any of them are
  # censored.
  #
  # Inferring "this lead had qualified" from a later stage is right for a skipped
  # checkpoint, where the capture date is genuinely missing, and wrong whenever
  # the capture date exists and is simply still in the future. The out-of-order
  # record makes the difference visible: it was recorded won two days before it
  # became an opportunity, so a cutoff falling between those two dates sees the
  # win, would infer the opportunity from it, and would hand over a deal value
  # captured at an event that has not happened yet.
  knows_qualified <- observed(leads$qualified_date) |
    (is.na(leads$qualified_date) &
      (observed(leads$opportunity_date) | observed(leads$won_date)))
  knows_opportunity <- observed(leads$opportunity_date) |
    (is.na(leads$opportunity_date) & observed(leads$won_date))

  leads |>
    mutate(across(
      c(qualified_date, opportunity_date, won_date, lost_date),
      observed_date
    )) |>
    mutate(
      late_industry = if_else(.env$knows_qualified, late_industry, NA_character_),
      late_deal_value = if_else(.env$knows_opportunity, late_deal_value, NA_real_),
      late_competitor = if_else(.env$knows_opportunity, late_competitor, NA_character_)
    )
}

# Everything the agent is allowed to look at goes through here.
#
# The order of operations is the whole point. Undo the backfill, censor what was
# not observable, then run the same cleaning and derivation the cohort ran. That
# is not just a way to blank some columns -- it reconstructs what the pipeline
# would have produced had it run on the as-of date, which is the only honest
# definition of a point-in-time view. Censoring the already-cleaned table instead
# leaves backfilled dates and late attributes computed from the future.
funnel_snapshot <- function(funnel_cohort, as_of = INBOX_AS_OF) {
  # Snapshots reconstruct backwards, never forwards. Censoring throws information
  # away, so asking a January table for a June view returns whatever survived
  # January -- fewer leads, fewer wins, and nothing in the result to say so.
  if ("snapshot_as_of" %in% names(funnel_cohort)) {
    source_as_of <- max(funnel_cohort$snapshot_as_of)
    if (as_of > source_as_of) {
      stop(
        "Cannot take a ", as_of, " snapshot of a table already censored at ",
        source_as_of, ".\n",
        "  Everything after ", source_as_of, " has been discarded, so the result\n",
        "  would be a ", source_as_of, " view wearing a later date.\n",
        "  Start again from the cohort table.",
        call. = FALSE
      )
    }
  }

  funnel_cohort |>
    filter(entered_date <= as_of) |>
    restore_recorded_dates() |>
    censor_unobservable(as_of) |>
    backfill_skipped_checkpoints() |>
    derive_cohort_fields(as_of) |>
    mutate(snapshot_as_of = as_of) |>
    select(all_of(SNAPSHOT_COLUMNS)) |>
    arrange(lead_id)
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
      "Run: Rscript prep/build_cohort.R"
    )
  }
  readr::read_csv(path, col_types = readr::cols(), progress = FALSE)
}

if (sys.nframe() == 0L) {
  funnel_cohort <- build_funnel_cohort()
  path <- write_funnel_cohort(funnel_cohort)
  message("Wrote ", nrow(funnel_cohort), " leads to ", path)
}
