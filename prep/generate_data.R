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

# Leads that fail a gate are marked lost some time after the last stage they did
# reach.
gen_lost_median <- 21
gen_lost_sdlog <- 0.90

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

generate_funnel_raw <- function(seed = INBOX_SEED) {
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  generate_leads()
}

if (sys.nframe() == 0L) {
  funnel_raw <- generate_funnel_raw()
  dir.create(inbox_data_path(), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(funnel_raw, inbox_data_path("funnel_raw.csv"), na = "")
  message(
    "Wrote ", nrow(funnel_raw), " leads to ", inbox_data_path("funnel_raw.csv")
  )
}
