# Seeded generator for the synthetic funnel used throughout the talk.
#
# The script simulates the event process: leads arrive, some progress through
# stages, each transition takes time. Every trap the talk relies on falls out
# of that process rather than being written in by hand, so anyone can change a
# parameter and watch the trap move. It is deterministic: re-running must give
# a byte-identical CSV, so figures quoted in the script cannot drift.

library(dplyr)

source(here::here("R", "config.R"))

INBOX_SEED <- 4817L

# Entries span about fourteen months and stop at the as-of date. Outcomes may
# land after it. Censoring happens in prep/build_cohort.R, not here.
gen_first_entry <- as.Date("2025-05-01")
gen_last_entry <- INBOX_AS_OF

# --- Arrival ----------------------------------------------------------------
# Mild growth plus weekday seasonality. The growth matters: it makes the recent
# cohorts the large ones, which gives the outcome-delay trap enough weight to
# look like a real decline.
#
# The base rate is set for statistical power, not realism. The talk's
# correction compares cohorts over an equal window from entry. The newest
# complete cohort can only support a 30-day window, which catches about a third
# of a cohort's eventual wins. At a few hundred leads per month that leaves
# about sixteen wins per cohort, which swings by a quarter on chance alone. The
# first tuning showed this: the newest cohort's corrected rate was still 40%
# below its neighbours from sampling noise, so the correction visibly corrected
# nothing. Tripling the arrival rate fixes it for the right reason. Across five
# seeds at this size the corrected ratio lands between 0.96 and 1.19 of the
# reference months, four of them inside 0.99 to 1.02. The counts are large
# enough; the seed was not picked to flatter the result.
gen_daily_base <- 42.3
gen_annual_growth <- 1.6
# Looked up by position, Sunday first, to match POSIXlt's day-of-week numbering.
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
# All three are assigned at entry and always populated, so they are safe to
# group a full-funnel conversion denominator by.
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
# Captured partway down the funnel, so blank for every lead that never got
# that far. Grouping a full-funnel denominator by them silently turns it into
# "leads that qualified". Blank does not mean zero: sales does not record an
# industry for a lead nobody spoke to.
gen_industry_p <- c(
  Manufacturing = 0.24,
  Retail = 0.19,
  Technology = 0.17,
  `Financial Services` = 0.14,
  Healthcare = 0.12,
  `Public Sector` = 0.08,
  Education = 0.06
)

# Recorded when the lead becomes an opportunity. All names are invented.
gen_competitor_p <- c(
  `Sagebrush Software` = 0.31,
  `Dustdevil Cloud` = 0.24,
  `Mesa Systems` = 0.18,
  `Prickly Pear Apps` = 0.11,
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
# A lead can be touched by several campaigns, stored as one comma-separated
# string. Splitting it before grouping gives one row per lead-campaign pair, so
# a lead touched by three campaigns is counted three times in any denominator.
# Populated at entry, so never blank.
gen_campaign_pool <- c(
  "Spring Workflow Webinar",
  "Approval Automation Guide",
  "Team Plan Promo",
  "Workflow Tips Newsletter",
  "Process Automation Ebook",
  "Regional Roadshow",
  "Customer Referral Push",
  "Team Efficiency Report"
)
gen_campaign_count_p <- c(`1` = 0.35, `2` = 0.35, `3` = 0.22, `4` = 0.08)

# --- Progression ------------------------------------------------------------
# Three sequential gates, each logistic in channel, company size, and a
# per-lead quality term shared across gates. Eventual entry-to-won conversion
# is the product, tuned to about 11%.
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
# Lognormal per stage, so the entry-to-won total is right-skewed. Medians are
# in days.
gen_stage_median <- c(qualified = 7, opportunity = 11.5, won = 13.5)
gen_stage_sdlog <- c(qualified = 0.75, opportunity = 0.80, won = 0.85)

# Enterprise deals move slower; referred and partner leads move faster. This
# gives a second trap for free: at the as-of date Enterprise looks worse than
# it is, purely from delay.
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
# A few records reach a late stage with no date on an earlier one: someone
# moved a deal forward in the CRM without filling in the step. The lead did
# reach the stage, so the attribute captured there is still recorded. Only the
# timestamp is missing. This is the subtlest trap of the four. A pipeline must
# backfill the date to keep the sequence usable, but a backfilled date is a
# guess, so any time-in-stage metric built on it is fiction. That is why the
# use_for_time_* flags in the cohort table matter.
gen_skip_qualified <- 0.030
gen_skip_opportunity <- 0.015

# Leads that fail a gate are marked lost some time after the last stage reached.
gen_lost_median <- 21
gen_lost_sdlog <- 0.90

# --- Deliberate data-entry anomalies ----------------------------------------
# Everything above is a plausible business process. These are broken records
# injected on purpose so prep/validate_data.R has something real to fail on. A
# validator that can only pass teaches nobody anything. Rows are picked by
# position within a filtered set, not at random, so they land on the same leads
# every run and the validator's output is stable enough to quote.
gen_anomaly_negative_lag <- c(250L, 1900L)
gen_anomaly_bad_ordering <- 600L
gen_anomaly_duplicate_id <- c(from = 100L, to = 101L)

# ---------------------------------------------------------------------------

logistic <- function(x) 1 / (1 + exp(-x))

# seq() over dates returns an integer-backed Date; as.Date() and readr return
# double-backed ones. They print and serialise the same, so the difference only
# shows when identical() compares the in-memory table with the parsed CSV.
# Normalising here is cheaper than tracking which columns are which.
as_double_date <- function(x) structure(as.numeric(x), class = "Date")

# Parameterised by the median because the tuning targets are medians.
rlnorm_median <- function(n, median, sdlog) {
  stats::rlnorm(n, meanlog = log(median), sdlog = sdlog)
}

# Daily arrival counts, Poisson around a trend-and-seasonality mean.
generate_arrivals <- function() {
  dates <- seq(gen_first_entry, gen_last_entry, by = "day")
  elapsed_years <- as.numeric(dates - gen_first_entry) / 365
  # Indexed by position, not name. weekdays() returns localised names, so under
  # a non-English LC_TIME every lookup misses, lambda becomes NA, and rpois()
  # dies with an invalid 'times' error. POSIXlt numbers days from 0 for Sunday,
  # the order gen_weekday_factor is written in.
  weekday <- as.POSIXlt(dates)$wday + 1L
  lambda <- gen_daily_base *
    gen_annual_growth^elapsed_years *
    unname(gen_weekday_factor[weekday])
  as_double_date(rep(dates, times = stats::rpois(length(dates), lambda)))
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
  # unname() throughout: a lookup by name returns a named vector, and the names
  # ride into every derived column. The CSV drops them, but callers using the
  # in-memory table got date columns carrying 21,000 names apiece.
  offset <- unname(
    gen_channel_effect[channel] + gen_company_size_effect[company_size] + quality
  )

  # Sequential gates. Failing one stops the lead where it is.
  passed_qualified <- stats::runif(n) <
    logistic(gen_gate_intercept[["qualified"]] + offset)
  passed_opportunity <- passed_qualified &
    stats::runif(n) < logistic(gen_gate_intercept[["opportunity"]] + offset)
  passed_won <- passed_opportunity &
    stats::runif(n) < logistic(gen_gate_intercept[["won"]] + offset)

  # Delays are drawn for every lead so the random stream does not depend on
  # gate outcomes. Unreached stages are blanked afterwards.
  delay_scale <- unname(gen_size_delay[company_size] * gen_channel_delay[channel])
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

  # Rounding up keeps each stage date strictly later than the previous one.
  qualified_date <- entered_date + ceiling(lag_qualified)
  opportunity_date <- qualified_date + ceiling(lag_opportunity)
  won_date <- opportunity_date + ceiling(lag_won)

  qualified_date[!passed_qualified] <- NA
  opportunity_date[!passed_opportunity] <- NA
  won_date[!passed_won] <- NA

  last_reached <- pmax(
    entered_date,
    dplyr::coalesce(qualified_date, entered_date),
    dplyr::coalesce(opportunity_date, entered_date),
    na.rm = TRUE
  )
  lost_date <- last_reached + ceiling(lag_lost)
  lost_date[passed_won] <- NA

  tibble::tibble(
    lead_id = sprintf("CC-%06d", seq_len(n)),
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

# Values are drawn for every lead, then blanked where the lead never reached
# the capturing stage. Missingness follows where each lead stopped rather than
# an independent coin flip.
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

# Names are sorted so the string is canonical rather than carrying draw order.
# That keeps the column diffable and the regeneration test honest.
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

# Only records with the later date are eligible, so this drops a timestamp
# without changing how far a lead got. Conversion counts are untouched.
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

# Exported rather than kept inside the injector so the validator can check
# identity instead of counting. Counting is fooled by substitution: repair one
# configured anomaly, break a different row the same way, and totals are
# unchanged. The selection is stable before and after injection, because
# injection changes date values but not which dates are missing.
anomaly_rows <- function(leads) {
  qualified_rows <- which(!is.na(leads$qualified_date))
  full_path_rows <- which(!is.na(leads$opportunity_date) & !is.na(leads$won_date))

  list(
    negative_lag = qualified_rows[gen_anomaly_negative_lag],
    bad_ordering = full_path_rows[gen_anomaly_bad_ordering],
    duplicate_from = full_path_rows[gen_anomaly_duplicate_id[["from"]]],
    duplicate_to = full_path_rows[gen_anomaly_duplicate_id[["to"]]]
  )
}

# Called last so nothing downstream in the generator can quietly repair the
# damage.
add_data_entry_anomalies <- function(leads) {
  rows <- anomaly_rows(leads)

  # Qualified before arrival: a negative time in stage. Real CRMs produce these
  # through manual backdating.
  leads$qualified_date[rows$negative_lag] <-
    leads$entered_date[rows$negative_lag] - c(3, 11)

  # Won before it became an opportunity.
  leads$won_date[rows$bad_ordering] <-
    leads$opportunity_date[rows$bad_ordering] - 2

  # Duplicate lead id, so anything treating lead_id as a key double-counts or
  # drops one row.
  leads$lead_id[rows$duplicate_to] <- leads$lead_id[rows$duplicate_from]

  leads
}

generate_funnel_raw <- function(seed = INBOX_SEED) {
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  # Order matters: skipped checkpoints come after late attributes are assigned
  # from intact stage dates. A lead with a missing qualification date still
  # qualified, so it still has an industry. That inconsistency is the one the
  # pipeline has to notice.
  generate_leads() |>
    add_late_attributes() |>
    add_campaigns() |>
    add_skipped_checkpoints() |>
    add_data_entry_anomalies()
}

# --- Serialisation ----------------------------------------------------------
# The CSV is gitignored and rebuilt from the seed. Explicit column types keep
# every reader in agreement; guessed types are a slow way to make two runs
# differ.

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
      "Run: Rscript prep/generate_data.R"
    )
  }
  readr::read_csv(path, col_types = funnel_raw_col_types(), progress = FALSE)
}

if (sys.nframe() == 0L) {
  funnel_raw <- generate_funnel_raw()
  path <- write_funnel_raw(funnel_raw)
  message("Wrote ", nrow(funnel_raw), " leads to ", path)
}
