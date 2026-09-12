# The snapshot contract, shared by the builder that produces snapshots and the
# recipes that consume them. A boundary enforced in two places is enforced
# inconsistently, and the whole talk rests on this one, so it gets one
# definition. Lives in R/ rather than prep/ so both sides can source it without
# a script pulling in another script.

SNAPSHOT_CUTOFF_COLUMN <- "snapshot_as_of"

# Every date that must not fall after the cutoff. entered_date belongs here
# even though it is never censored. Leads that entered after the cutoff are
# dropped, not blanked, so a row appended later with a future entry date has
# no stage dates to give itself away. stage_funnel() counts every row as
# entered, so it would inflate every denominator in the table.
SNAPSHOT_CENSORED_DATES <- c(
  "entered_date", "qualified_date", "opportunity_date", "won_date", "lost_date"
)

# A cutoff has to be one real date, and it has to be bare. as.Date(NA) sails
# through every comparison, produces an empty snapshot that looks merely
# uneventful, and only surfaces later as "missing value where TRUE/FALSE
# needed" somewhere unrelated. Names are stripped because unique() drops them
# from a column while an attribute keeps them, so a named date would make the
# two carriers disagree about a value they both hold.
valid_cutoff <- function(as_of) {
  # is.finite() rather than is.na() rules out missing and infinite together.
  # as.Date(Inf) is constructible and not missing: positive infinity would wave
  # every future event through, negative gives the same quietly empty snapshot.
  if (!inherits(as_of, "Date") || length(as_of) != 1 || !is.finite(as_of)) {
    stop(
      "A snapshot cutoff has to be one finite, non-missing Date. Got: ",
      paste(utils::capture.output(str(as_of)), collapse = " "),
      call. = FALSE
    )
  }
  unname(as_of)
}

# The cutoff a table was censored at, or NULL if it is not a snapshot. The
# cutoff is recorded twice on purpose. The column detects a table assembled
# from more than one snapshot: bind_rows() keeps the first input's attributes,
# so an attribute alone would let January rows ride inside a June table. The
# attribute survives a snapshot with no rows, where a column has nowhere to
# put anything.
snapshot_cutoff <- function(leads) {
  if (!SNAPSHOT_CUTOFF_COLUMN %in% names(leads)) {
    return(NULL)
  }

  meta <- attr(leads, SNAPSHOT_CUTOFF_COLUMN, exact = TRUE)
  recorded <- unique(leads[[SNAPSHOT_CUTOFF_COLUMN]])

  # No rows, so the attribute is the only witness left.
  if (nrow(leads) == 0) {
    if (is.null(meta)) {
      stop(
        "This empty table has a ", SNAPSHOT_CUTOFF_COLUMN, " column but no\n",
        "  record of the cutoff it was built at. Rebuild it with funnel_snapshot().",
        call. = FALSE
      )
    }
    return(valid_cutoff(meta))
  }

  if (length(recorded) > 1) {
    stop(
      "This table carries ", length(recorded), " different snapshot cutoffs: ",
      paste(sort(recorded), collapse = ", "), ".\n",
      "  Rows censored at different dates describe different moments, and a\n",
      "  metric computed over the mixture describes none of them.\n",
      "  Rebuild from the cohort table.",
      call. = FALSE
    )
  }
  if (anyNA(recorded)) {
    stop(
      "This table has rows but no cutoff recorded against them.\n",
      "  Rebuild it with funnel_snapshot().",
      call. = FALSE
    )
  }

  # The two carriers have to agree. Rewriting the column uniformly (January to
  # June, say) leaves the attribute saying January. Without this comparison the
  # relabelled table would be accepted as June and reconstructed from data that
  # stops in January. An operation that drops attributes, such as a join or
  # as.data.frame(), leaves the column as the only witness, so this raises the
  # cost of an accident rather than defeating a determined forgery. The
  # mixed-cutoff and date-range checks hold in every case.
  recorded <- valid_cutoff(recorded)
  if (!is.null(meta) && !identical(valid_cutoff(meta), recorded)) {
    stop(
      "This table's cutoff column says ", recorded, ", but the table was built\n",
      "  at ", meta, ". One of the two has been edited since.\n",
      "  Rebuild from the cohort table.",
      call. = FALSE
    )
  }
  recorded
}

# Record the cutoff on both carriers at once, so they cannot drift apart.
stamp_snapshot_cutoff <- function(leads, as_of) {
  as_of <- valid_cutoff(as_of)
  leads <- dplyr::mutate(leads, !!SNAPSHOT_CUTOFF_COLUMN := as_of)
  attr(leads, SNAPSHOT_CUTOFF_COLUMN) <- as_of
  leads
}

# Assert that a table really is a snapshot, and return the cutoff. This checks
# the data, not the shape: a select() can remove the hindsight column from a
# cohort, a bind_rows() can splice two cutoffs together, and a mutate() can put
# a date back after the fact.
require_snapshot <- function(leads) {
  as_of <- snapshot_cutoff(leads)
  if (is.null(as_of)) {
    stop(
      "This metric needs a snapshot.\n",
      "  Pass funnel_snapshot(cohort): it censors every date at the as-of date\n",
      "  and records that date so this check can verify it.\n",
      "  For hindsight over the full cohort, ask for basis = \"eventual\".",
      call. = FALSE
    )
  }

  # Every censored date has to be present, not just those that happen to
  # remain. Otherwise dropping a column hides what it would have revealed:
  # remove entered_date and a row appended for a lead that had not arrived yet
  # becomes invisible while still counting towards every denominator.
  missing_dates <- setdiff(SNAPSHOT_CENSORED_DATES, names(leads))
  if (length(missing_dates) > 0) {
    stop(
      "This table is missing ", paste(missing_dates, collapse = ", "), ".\n",
      "  A snapshot's cutoff can only be verified against the dates it censors,\n",
      "  so all of them have to be present.",
      call. = FALSE
    )
  }

  offenders <- vapply(
    leads[SNAPSHOT_CENSORED_DATES],
    function(x) any(!is.na(x) & x > as_of),
    logical(1)
  )
  if (any(offenders)) {
    stop(
      "This table says it was censored at ", as_of, ", but ",
      paste(names(offenders)[offenders], collapse = ", "),
      " runs past that date.\n",
      "  Something has edited or reassembled it since it was built.",
      call. = FALSE
    )
  }
  as_of
}
