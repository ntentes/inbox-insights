# The snapshot contract.
#
# Shared by the builder that produces snapshots and the recipes that consume
# them. A boundary enforced in two places is a boundary enforced inconsistently,
# and this is the boundary the whole talk rests on, so it gets one definition.
#
# Lives in R/ rather than prep/ so both sides can source it without a script
# having to pull in another script.

SNAPSHOT_CUTOFF_COLUMN <- "snapshot_as_of"

# Every date that must not fall after the cutoff.
#
# entered_date belongs on this list even though it is never censored. Rows are
# dropped rather than blanked when a lead entered after the cutoff, so a row
# appended afterwards with a future entry date carries no stage dates to give
# itself away -- and stage_funnel() counts every row as entered, so it would
# inflate every denominator in the table.
SNAPSHOT_CENSORED_DATES <- c(
  "entered_date", "qualified_date", "opportunity_date", "won_date", "lost_date"
)

# The cutoff a table was censored at, or NULL if it is not a snapshot.
#
# The cutoff is recorded twice on purpose, and neither copy is redundant. The
# column is what detects a table assembled from more than one snapshot:
# bind_rows() keeps the first input's attributes, so an attribute on its own
# would let January rows ride along inside a June table. The attribute is what
# survives a snapshot with no rows, where a column has nowhere to put anything.
snapshot_cutoff <- function(leads) {
  if (!SNAPSHOT_CUTOFF_COLUMN %in% names(leads)) {
    return(NULL)
  }

  recorded <- unique(leads[[SNAPSHOT_CUTOFF_COLUMN]])
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
  if (length(recorded) == 1 && !is.na(recorded)) {
    return(recorded)
  }

  meta <- attr(leads, SNAPSHOT_CUTOFF_COLUMN, exact = TRUE)
  if (is.null(meta)) {
    stop(
      "This table has a ", SNAPSHOT_CUTOFF_COLUMN, " column with no usable\n",
      "  cutoff in it. Rebuild it with funnel_snapshot().",
      call. = FALSE
    )
  }
  meta
}

# Record the cutoff on both carriers at once, so they cannot drift apart.
stamp_snapshot_cutoff <- function(leads, as_of) {
  leads <- dplyr::mutate(leads, !!SNAPSHOT_CUTOFF_COLUMN := as_of)
  attr(leads, SNAPSHOT_CUTOFF_COLUMN) <- as_of
  leads
}

# Assert that a table really is a snapshot, and return the cutoff.
#
# This checks the data rather than the shape of the table. Column presence is not
# proof of anything -- a select() can remove the hindsight column from a cohort,
# a bind_rows() can splice two cutoffs together, and a mutate() can put a date
# back after the fact.
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

  present <- intersect(SNAPSHOT_CENSORED_DATES, names(leads))
  offenders <- vapply(
    leads[present],
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
