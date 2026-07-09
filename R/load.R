#' Load the two published name tables and build the identity lookup.
#'
#' Reads `name_lower`, `canonical_name`, and `identity_state` from the CRAN and
#' Bioc name tables (each an SQLite file the caller has already downloaded) and
#' returns the unified lookup from [resolve_identity_set()]. Apply [check_size()]
#' to `maps$n_cran` and `maps$n_bioc` against per-source baselines before trusting
#' the result.
#'
#' @param cran_db_path Path to the SQLite file holding the CRAN name table.
#' @param bioc_db_path Path to the SQLite file holding the Bioc name table.
#' @param cran_table CRAN table name (default "cran_names_all").
#' @param bioc_table Bioc table name (default "bioc_names_all").
#' @return The `maps` object from [resolve_identity_set()].
#' @export
load_identity <- function(cran_db_path, bioc_db_path,
                          cran_table = "cran_names_all",
                          bioc_table = "bioc_names_all") {
  read_tbl <- function(path, tbl) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, sprintf(
      "SELECT name_lower, canonical_name, identity_state FROM %s", tbl))
  }
  resolve_identity_set(read_tbl(cran_db_path, cran_table),
                       read_tbl(bioc_db_path, bioc_table))
}
