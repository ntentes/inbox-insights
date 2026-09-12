# Raw funnel table -> funnel_cohort, the one intermediate table everything else
# reads.
#
# Two jobs. Cleaning: resolve the duplicate id, backfill skipped checkpoints,
# refuse to emit a negative duration. And recording what the cleaning cost:
# every repair leaves a flag, because a backfilled date is a guess and a metric
# built on a guess should be able to opt out. It also draws the talk's central
# line. won_as_of is what was known at the snapshot; won_eventually is what
# happened later. The agent only sees the first. funnel_snapshot() enforces
# that separation.

library(dplyr)

source(here::here("prep", "generate_data.R"))
source(here::here("R", "snapshot.R"))

# --- Cleaning ---------------------------------------------------------------

# Keep the earlier entry of the duplicated id, deterministically, so downstream
# code can rely on lead_id being a key. Letting it through would double-count
# one lead in every denominator.
deduplicate_leads <- function(funnel_raw) {
  funnel_raw |>
    arrange(entered_date, lead_id) |>
    distinct(lead_id, .keep_all = TRUE)
}

# The midpoint is defensible for ordering and useless for duration, which is
# why backfilled dates are flagged rather than just filled.
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

# Undo the backfill. A flagged date was NA in the source, so blanking it
# restores the source exactly. funnel_snapshot() needs this because it must
# censor the recorded dates: a date invented from a future opportunity is
# future knowledge, so it comes off before censoring and is recomputed after.
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

# The backdated qualifications and the won-before-opportunity record would
# otherwise give negative days. A negative duration in a median is worse than
# a missing one.
non_negative_days <- function(from, to) {
  days <- as.numeric(to - from)
  if_else(!is.na(days) & days >= 0, days, NA_real_)
}

# --- Derived fields ---------------------------------------------------------

# Factored out because the snapshot recomputes all of this after censoring,
# and two hand-written copies is how column definitions drift apart. Rule:
# every column here derives only from the dates in front of it. A value that
# needed a censored date is a leak.
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

      # Eligible for time metrics only when every date the duration depends on
      # was recorded, not guessed, and every step was non-negative. The flags
      # chain deliberately: reaching won through an unrecorded qualification
      # says nothing trustworthy about how long the deal took, even though
      # entered-to-won subtracts to a plausible number. Chaining also catches
      # the won-before-opportunity record, whose entered-to-won is positive
      # and fictional.
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

# Column order in one place so the cohort and the snapshot cannot disagree.
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

# A snapshot carries its own cutoff, so a downstream metric can check it was
# given censored data rather than infer it from a missing column, which any
# stray select() can arrange.
SNAPSHOT_COLUMNS <- c(
  setdiff(COHORT_COLUMNS, COHORT_HINDSIGHT_COLUMNS),
  SNAPSHOT_CUTOFF_COLUMN
)

# --- The cohort table -------------------------------------------------------

build_funnel_cohort <- function(funnel_raw = read_funnel_raw(), as_of = INBOX_AS_OF) {
  funnel_raw |>
    deduplicate_leads() |>
    # The late_ prefix carries the hazard with the column. These three are
    # captured partway down the funnel, so they are blank for leads that
    # stopped earlier, and grouping a full-funnel denominator by one is a bug.
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

# Blank everything not observable at the as-of date. The dates are the obvious
# half. The late attributes are easy to miss: late_industry is recorded at
# qualification, so a lead that qualified in July has no industry in a June
# snapshot. Leaving it in tells the agent which leads were about to progress.
censor_unobservable <- function(leads, as_of) {
  observed <- function(x) !is.na(x) & x <= as_of
  observed_date <- function(x) if_else(observed(x), x, as.Date(NA))

  # Visibility is decided from the recorded capture dates before censoring.
  # Inferring qualification from a later stage is right for a skipped
  # checkpoint, where the date is missing, and wrong when the date exists but
  # is still in the future. The out-of-order record shows why: it was won two
  # days before it became an opportunity, so a cutoff between those dates
  # would infer the opportunity and hand over a deal value captured at an
  # event that has not happened yet.
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

# Everything the agent may look at goes through here. The order matters: undo
# the backfill, censor what was not observable, then rerun the cohort's own
# cleaning and derivation. That reconstructs what the pipeline would have
# produced on the as-of date, the only honest point-in-time view. Censoring
# the cleaned table instead leaves backfilled dates and late attributes
# computed from the future.
funnel_snapshot <- function(funnel_cohort, as_of = INBOX_AS_OF) {
  # Snapshots reconstruct backwards, never forwards. Censoring discards
  # information, so a June view of a January table would return whatever
  # survived January with nothing in the result to say so.
  as_of <- valid_cutoff(as_of)
  source_as_of <- snapshot_cutoff(funnel_cohort)
  if (!is.null(source_as_of) && as_of > source_as_of) {
    stop(
      "Cannot take a ", as_of, " snapshot of a table already censored at ",
      source_as_of, ".\n",
      "  Everything after ", source_as_of, " has been discarded, so the result\n",
      "  would be a ", source_as_of, " view wearing a later date.\n",
      "  Start again from the cohort table.",
      call. = FALSE
    )
  }

  funnel_cohort |>
    filter(entered_date <= as_of) |>
    restore_recorded_dates() |>
    censor_unobservable(as_of) |>
    backfill_skipped_checkpoints() |>
    derive_cohort_fields(as_of) |>
    stamp_snapshot_cutoff(as_of) |>
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
