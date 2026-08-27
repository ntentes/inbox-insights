# Seeded generator for the synthetic funnel used throughout the talk.
#
# Nothing here is a summary statistic. The script simulates the event process --
# leads arrive, some progress through stages, each transition takes time -- and
# every pathology the talk relies on falls out of that process rather than being
# written in by hand. That is the point: anyone who clones the repo can change a
# parameter and watch the trap move.
#
# Deterministic. Re-running it must produce a byte-identical CSV, so the figures
# quoted in the script and locked by the tests cannot drift.

library(dplyr)

source(here::here("R", "config.R"))

INBOX_SEED <- 4817L

# Entries span roughly fourteen months and stop at the as-of date. Outcomes are
# free to land after it -- censoring happens in prep/build_cohort.R, not here.
gen_first_entry <- as.Date("2025-05-01")
gen_last_entry <- INBOX_AS_OF

# --- Arrival ----------------------------------------------------------------
# Mild growth plus weekday seasonality. The growth is load-bearing: it makes the
# recent cohorts the large ones, which is what gives the outcome-delay trap
# enough weight to look like a real decline.
gen_daily_base <- 14.1
gen_annual_growth <- 1.6
gen_weekday_factor <- c(
  Sunday = 0.30,
  Monday = 1.18,
  Tuesday = 1.24,
  Wednesday = 1.20,
  Thursday = 1.12,
  Friday = 0.94,
  Saturday = 0.34
)

# --- Dimensions -------------------------------------------------------------
# All three are assigned at entry and are always populated, so they are safe to
# group a full-funnel conversion denominator by. The late-arriving attributes,
# which are not, come later in the build.
gen_channel_p <- c(
  `Organic Search` = 0.22,
  `Paid Search` = 0.18,
  Social = 0.14,
  Email = 0.13,
  Referral = 0.11,
  Direct = 0.14,
  Partner = 0.08
)
gen_company_size_p <- c(Small = 0.42, Medium = 0.31, Large = 0.19, Enterprise = 0.08)
gen_region_p <- c(
  `North America` = 0.48,
  EMEA = 0.28,
  APAC = 0.16,
  LATAM = 0.08
)

# --- Late-populated attributes ----------------------------------------------
# These are captured partway down the funnel, not at entry, so they are blank
# for every lead that never got that far. That makes them unsafe to group a
# full-funnel conversion denominator by -- the denominator silently becomes
# "leads that qualified" -- and it means blank does not mean zero. Sales does
# not record an industry for a lead nobody ever spoke to.
gen_industry_p <- c(
  Manufacturing = 0.24,
  Retail = 0.19,
  Technology = 0.17,
  `Financial Services` = 0.14,
  Healthcare = 0.12,
  `Public Sector` = 0.08,
  Education = 0.06
)

# Whoever the deal was competed against, recorded when it becomes an
# opportunity. All invented; any resemblance to a real freight company is
# accidental.
gen_competitor_p <- c(
  `Sagebrush Freight` = 0.31,
  `Dustdevil Cargo` = 0.24,
  `Mesa Logistics Group` = 0.18,
  `Prickly Pear Transit` = 0.11,
  `no competitor identified` = 0.16
)

# Deal size, also recorded at opportunity. Scaled by company size.
gen_deal_value_median <- 24000
gen_deal_value_sdlog <- 0.62
gen_deal_value_size <- c(
  Small = 0.45,
  Medium = 1.00,
  Large = 2.60,
  Enterprise = 7.50
)

# --- Campaigns --------------------------------------------------------------
# One lead can be touched by several campaigns, and the source system records
# them as a single comma-separated string. Splitting that column before grouping
# gives one row per lead-campaign pair, so any count or conversion denominator
# computed afterwards is inflated -- a lead touched by three campaigns is
# counted three times. Populated at entry, so it is never blank.
gen_campaign_pool <- c(
  "Spring Freight Webinar",
  "Cold Chain Guide",
  "Rate Card Promo",
  "Last-Mile Newsletter",
  "Warehouse Automation Ebook",
  "Regional Roadshow",
  "Customer Referral Push",
  "Fleet Efficiency Report"
)
gen_campaign_count_p <- c(`1` = 0.35, `2` = 0.35, `3` = 0.22, `4` = 0.08)

# --- Progression ------------------------------------------------------------
# Three sequential gates, each a logistic function of channel, company size, and
# a per-lead quality term shared across all three gates. Eventual entry-to-won
# conversion is the product, tuned to roughly 11%.
gen_gate_intercept <- c(qualified = -0.43, opportunity = -0.51, won = -0.05)
gen_channel_effect <- c(
  `Organic Search` = 0.06,
  `Paid Search` = -0.10,
  Social = -0.22,
  Email = 0.02,
  Referral = 0.24,
  Direct = 0.08,
  Partner = 0.18
)
gen_company_size_effect <- c(
  Small = -0.10,
  Medium = 0.00,
  Large = 0.12,
  Enterprise = 0.22
)
gen_quality_sd <- 0.70

# --- Delay ------------------------------------------------------------------
# Lognormal per stage, so the entry-to-won total is right-skewed with a long
# tail. Medians are in days.
gen_stage_median <- c(qualified = 7, opportunity = 11.5, won = 13.5)
gen_stage_sdlog <- c(qualified = 0.75, opportunity = 0.80, won = 0.85)

# Enterprise deals move materially slower and referred and partner-sourced leads
# move faster. This produces a second trap for free: at the as-of date Enterprise
# looks worse than it is, purely from delay.
gen_size_delay <- c(Small = 0.85, Medium = 1.00, Large = 1.25, Enterprise = 1.84)
gen_channel_delay <- c(
  `Organic Search` = 1.00,
  `Paid Search` = 1.05,
  Social = 1.05,
  Email = 1.00,
  Referral = 0.72,
  Direct = 0.95,
  Partner = 0.72
)

# --- Skipped checkpoints ----------------------------------------------------
# A small fraction of records show up at a late stage with no date on an earlier
# one. Nothing sinister: somebody moved a deal forward in the CRM without filling
# in the step it passed through. The lead did reach the stage, so the attribute
# captured there is still recorded -- only the timestamp is missing.
#
# This is the subtlest trap of the four. A pipeline has to backfill the missing
# date to keep the stage sequence usable, but a backfilled date is a guess, so
# any time-in-stage metric computed from it is fiction. That is what makes the
# use_for_time_* eligibility flags in the cohort table load-bearing rather than
# decorative.
gen_skip_qualified <- 0.030
gen_skip_opportunity <- 0.015

# Leads that fail a gate are marked lost some time after the last stage they did
# reach.
gen_lost_median <- 21
gen_lost_sdlog <- 0.90

# --- Deliberate data-entry anomalies ----------------------------------------
# Everything above this point is a plausible business process. These are not.
# They are a handful of broken records injected on purpose so that
# prep/validate_data.R has something real to fail on -- a validation script that
# can only ever pass teaches nobody anything.
#
# The affected rows are picked by position within a filtered set rather than at
# random, so they land on the same leads every run and the validator's output is
# stable enough to quote.
gen_anomaly_negative_lag <- c(250L, 1900L)
gen_anomaly_bad_ordering <- 600L
gen_anomaly_duplicate_id <- c(from = 100L, to = 101L)

# ---------------------------------------------------------------------------

logistic <- function(x) 1 / (1 + exp(-x))

# A lognormal parameterised by the median, which is what the tuning targets are
# expressed in, rather than by meanlog.
rlnorm_median <- function(n, median, sdlog) {
  stats::rlnorm(n, meanlog = log(median), sdlog = sdlog)
}

# Daily arrival counts, Poisson around a trend-and-seasonality mean.
generate_arrivals <- function() {
  dates <- seq(gen_first_entry, gen_last_entry, by = "day")
  elapsed_years <- as.numeric(dates - gen_first_entry) / 365
  weekday <- weekdays(dates)
  lambda <- gen_daily_base *
    gen_annual_growth^elapsed_years *
    gen_weekday_factor[weekday]
  rep(dates, times = stats::rpois(length(dates), lambda))
}

# One row per lead: the four stage dates, the terminal lost date, and the three
# always-populated dimensions.
generate_leads <- function() {
  entered_date <- generate_arrivals()
  n <- length(entered_date)

  channel <- sample(names(gen_channel_p), n, replace = TRUE, prob = gen_channel_p)
  company_size <- sample(
    names(gen_company_size_p),
    n,
    replace = TRUE,
    prob = gen_company_size_p
  )
  region <- sample(names(gen_region_p), n, replace = TRUE, prob = gen_region_p)

  quality <- stats::rnorm(n, 0, gen_quality_sd)
  offset <- gen_channel_effect[channel] + gen_company_size_effect[company_size] + quality

  # Sequential gates. Failing one stops the lead where it is.
  passed_qualified <- stats::runif(n) <
    logistic(gen_gate_intercept[["qualified"]] + offset)
  passed_opportunity <- passed_qualified &
    stats::runif(n) < logistic(gen_gate_intercept[["opportunity"]] + offset)
  passed_won <- passed_opportunity &
    stats::runif(n) < logistic(gen_gate_intercept[["won"]] + offset)

  # Delays are drawn for every lead regardless of how far it got, so that the
  # random stream does not depend on the gate outcomes. Unreached stages are
  # blanked afterwards.
  delay_scale <- gen_size_delay[company_size] * gen_channel_delay[channel]
  lag_qualified <- rlnorm_median(
    n,
    gen_stage_median[["qualified"]],
    gen_stage_sdlog[["qualified"]]
  ) * delay_scale
  lag_opportunity <- rlnorm_median(
    n,
    gen_stage_median[["opportunity"]],
    gen_stage_sdlog[["opportunity"]]
  ) * delay_scale
  lag_won <- rlnorm_median(
    n,
    gen_stage_median[["won"]],
    gen_stage_sdlog[["won"]]
  ) * delay_scale
  lag_lost <- rlnorm_median(n, gen_lost_median, gen_lost_sdlog)

  # Rounding up guarantees each stage date is strictly later than the previous
  # one, so the table is monotonic by construction.
  qualified_date <- entered_date + ceiling(lag_qualified)
  opportunity_date <- qualified_date + ceiling(lag_opportunity)
  won_date <- opportunity_date + ceiling(lag_won)

  qualified_date[!passed_qualified] <- NA
  opportunity_date[!passed_opportunity] <- NA
  won_date[!passed_won] <- NA

  # A lost lead is marked lost some time after the furthest stage it reached.
  last_reached <- pmax(
    entered_date,
    dplyr::coalesce(qualified_date, entered_date),
    dplyr::coalesce(opportunity_date, entered_date),
    na.rm = TRUE
  )
  lost_date <- last_reached + ceiling(lag_lost)
  lost_date[passed_won] <- NA

  tibble::tibble(
    lead_id = sprintf("TL-%06d", seq_len(n)),
    entered_date = entered_date,
    qualified_date = qualified_date,
    opportunity_date = opportunity_date,
    won_date = won_date,
    lost_date = lost_date,
    channel = channel,
    company_size = company_size,
    region = region
  )
}

# Attributes captured partway down the funnel. Values are drawn for every lead
# and then blanked for the leads that never reached the capturing stage, so the
# missingness is a consequence of where each lead stopped rather than an
# independent coin flip.
add_late_attributes <- function(leads) {
  n <- nrow(leads)

  industry <- sample(
    names(gen_industry_p),
    n,
    replace = TRUE,
    prob = gen_industry_p
  )
  competitor <- sample(
    names(gen_competitor_p),
    n,
    replace = TRUE,
    prob = gen_competitor_p
  )
  deal_value <- unname(round(
    rlnorm_median(n, gen_deal_value_median, gen_deal_value_sdlog) *
      gen_deal_value_size[leads$company_size]
  ))

  # Captured at qualification.
  industry[is.na(leads$qualified_date)] <- NA
  # Captured when the lead becomes an opportunity.
  competitor[is.na(leads$opportunity_date)] <- NA
  deal_value[is.na(leads$opportunity_date)] <- NA

  leads |>
    mutate(
      industry = .env$industry,
      deal_value = .env$deal_value,
      competitor = .env$competitor
    )
}

# The multi-value column: a comma-separated list of the campaigns that touched
# each lead. Names are sorted so the string is canonical rather than carrying the
# draw order, which makes the column diffable and the regeneration test honest.
add_campaigns <- function(leads) {
  n <- nrow(leads)
  counts <- as.integer(sample(
    names(gen_campaign_count_p),
    n,
    replace = TRUE,
    prob = gen_campaign_count_p
  ))
  campaigns <- vapply(
    counts,
    function(k) paste(sort(sample(gen_campaign_pool, k)), collapse = ", "),
    character(1)
  )
  mutate(leads, campaigns = .env$campaigns)
}

# Blank an intermediate stage date on a small fraction of records that reached a
# later stage. Only records with the later date are eligible, so this drops a
# timestamp without ever changing how far a lead actually got -- conversion
# counts are untouched.
add_skipped_checkpoints <- function(leads) {
  n <- nrow(leads)
  skip_qualified <- stats::runif(n) < gen_skip_qualified
  skip_opportunity <- stats::runif(n) < gen_skip_opportunity

  leads |>
    mutate(
      qualified_date = if_else(
        .env$skip_qualified & !is.na(opportunity_date),
        as.Date(NA),
        qualified_date
      ),
      opportunity_date = if_else(
        .env$skip_opportunity & !is.na(won_date),
        as.Date(NA),
        opportunity_date
      )
    )
}

# Break a few records on purpose. Called last, so nothing downstream in the
# generator can quietly repair the damage.
add_data_entry_anomalies <- function(leads) {
  qualified_rows <- which(!is.na(leads$qualified_date))
  full_path_rows <- which(!is.na(leads$opportunity_date) & !is.na(leads$won_date))

  # A qualification date before the lead ever arrived, which yields a negative
  # time in stage. Real CRMs produce these through manual backdating.
  negative_lag <- qualified_rows[gen_anomaly_negative_lag]
  leads$qualified_date[negative_lag] <-
    leads$entered_date[negative_lag] - c(3, 11)

  # A deal recorded as won before it became an opportunity.
  bad_ordering <- full_path_rows[gen_anomaly_bad_ordering]
  leads$won_date[bad_ordering] <- leads$opportunity_date[bad_ordering] - 2

  # The same lead id on two different rows, so anything that assumes lead_id is
  # a key will silently double-count or silently drop one of them.
  leads$lead_id[full_path_rows[gen_anomaly_duplicate_id[["to"]]]] <-
    leads$lead_id[full_path_rows[gen_anomaly_duplicate_id[["from"]]]]

  leads
}

generate_funnel_raw <- function(seed = INBOX_SEED) {
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  # Order matters: the skipped checkpoints are applied last, after the late
  # attributes have been assigned from the intact stage dates. A lead whose
  # qualification date went missing still qualified, so it still has an
  # industry -- which is exactly the inconsistency the pipeline has to notice.
  generate_leads() |>
    add_late_attributes() |>
    add_campaigns() |>
    add_skipped_checkpoints() |>
    add_data_entry_anomalies()
}

# --- Serialisation ----------------------------------------------------------
# The CSV is gitignored: it is rebuilt from the seed rather than tracked. These
# helpers exist so that every reader agrees on the column types, because guessed
# types are a slow way to introduce a difference between two runs.

inbox_funnel_raw_path <- function() inbox_data_path("funnel_raw.csv")

funnel_raw_col_types <- function() {
  readr::cols(
    lead_id = readr::col_character(),
    entered_date = readr::col_date(),
    qualified_date = readr::col_date(),
    opportunity_date = readr::col_date(),
    won_date = readr::col_date(),
    lost_date = readr::col_date(),
    channel = readr::col_character(),
    company_size = readr::col_character(),
    region = readr::col_character(),
    industry = readr::col_character(),
    deal_value = readr::col_double(),
    competitor = readr::col_character(),
    campaigns = readr::col_character()
  )
}

write_funnel_raw <- function(funnel_raw, path = inbox_funnel_raw_path()) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(funnel_raw, path, na = "")
  invisible(path)
}

read_funnel_raw <- function(path = inbox_funnel_raw_path()) {
  if (!file.exists(path)) {
    stop(
      "No generated funnel table at ", path, ".\n",
      "Run prep/generate_data.R first, or prep/build_all.R for everything."
    )
  }
  readr::read_csv(path, col_types = funnel_raw_col_types(), progress = FALSE)
}

if (sys.nframe() == 0L) {
  funnel_raw <- generate_funnel_raw()
  path <- write_funnel_raw(funnel_raw)
  message("Wrote ", nrow(funnel_raw), " leads to ", path)
}
