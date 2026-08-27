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

  meta <- attr(leads, SNAPSHOT_CUTOFF_COLUMN, exact = TRUE)
  recorded <- unique(leads[[SNAPSHOT_CUTOFF_COLUMN]])

  # No rows, so the column has nowhere to keep anything and the attribute is the
  # only witness left.
  if (nrow(leads) == 0) {
    if (is.null(meta)) {
      stop(
        "This empty table has a ", SNAPSHOT_CUTOFF_COLUMN, " column but no\n",
        "  record of the cutoff it was built at. Rebuild it with funnel_snapshot().",
        call. = FALSE
      )
    }
    return(meta)
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

  # The two carriers have to agree. Rewriting the column uniformly -- January to
  # June, say -- leaves the attribute behind saying January, and without this
  # comparison the relabelled table would be accepted as June and reconstructed
  # from data that stops in January.
  #
  # A limitation worth being honest about: an operation that drops attributes,
  # such as a join or as.data.frame(), leaves the column as the only witness, so
  # this raises the cost of an accident rather than defeating a determined
  # forgery. The mixed-cutoff and date-range checks are the ones that hold in
  # every case.
  if (!is.null(meta) && !identical(as.Date(meta), as.Date(recorded))) {
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

  # Every censored date has to be present, not merely every censored date that
  # happens to still be there. Skipping the absent ones means dropping a column
  # is enough to hide what it would have revealed -- remove entered_date and a
  # row appended for a lead that had not arrived yet becomes invisible while
  # still counting towards every denominator.
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
