#' Decide whether a fetched name table is trustworthy.
#'
#' A successful-but-partial fetch (a truncated index served with a 200) is not an
#' error, so callers cannot rely on error handling alone. This gate rejects a
#' table that is below an absolute `floor` or that has shrunk more than
#' `(1 - tolerance)` below the last known good `baseline`. A FALSE result means
#' the caller should treat the source as unreachable and reuse last known good,
#' and must not overwrite the baseline with this run.
#'
#' @param count Integer row count of the freshly fetched table.
#' @param baseline Last known good row count, or NULL on the first run.
#' @param floor Absolute minimum acceptable row count.
#' @param tolerance Fraction of the baseline that must be retained (default 0.98).
#' @return TRUE if the count is trustworthy, FALSE otherwise.
#' @export
check_size <- function(count, baseline = NULL, floor, tolerance = 0.98) {
  if (!is.numeric(count) || length(count) != 1L || is.na(count)) return(FALSE)
  if (count < floor) return(FALSE)
  if (!is.null(baseline) && length(baseline) == 1L && !is.na(baseline) &&
      count < baseline * tolerance) {
    return(FALSE)
  }
  TRUE
}
