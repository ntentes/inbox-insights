source(here::here("prep", "build_cohort.R"))
source(here::here("R", "report_contract.R"))

initial_bad_report <- function(snapshot) {
  contract <- worked_time_contract(snapshot)
  evidence <- worked_evidence(snapshot, "snapshot")
  newest <- evidence[[4]]
  prior_rate <- sum(vapply(evidence[1:3], `[[`, numeric(1), "won")) /
    sum(vapply(evidence[1:3], `[[`, numeric(1), "leads"))
  insight <- list(
    title = "The newest cohort appears to be converting less well",
    finding = sprintf(
      "%s converted at %.1f%%, %.0f%% of the pooled rate for the preceding three cohorts.",
      format(as.Date(newest$cohort_month), "%Y-%m"),
      100 * newest$conversion, 100 * newest$conversion / prior_rate
    ),
    suggested_action = "Investigate the apparent decline in the newest cohort before the next report.",
    data_as_of = contract$data_as_of,
    reporting_period = contract$reporting_period,
    evidence_window = contract$evidence_window,
    outcome_horizon = "snapshot",
    metric_definition = "Entry-to-won conversion: all wins known at the cutoff divided by all leads entering each month.",
    evidence = evidence,
    caveat = "First-run teaching example: the arithmetic is reproducible, but unequal observation ages make the trend interpretation misleading. Do not publish as a recommendation.",
    reproducible_code = worked_evidence_code(snapshot, "snapshot")
  )
  new_example_report(insight, snapshot, "initial-bad-report", "teaching_example")
}

if (sys.nframe() == 0L) {
  snapshot <- funnel_snapshot(read_funnel_cohort())
  path <- inbox_fixture_path("initial_bad_report.json")
  write_example_report(initial_bad_report(snapshot), snapshot, path)
  message("Wrote authored teaching fixture to ", path)
}
