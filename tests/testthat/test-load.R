.write_names_db <- function(path, table, df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE %s (name_lower TEXT PRIMARY KEY, canonical_name TEXT NOT NULL,
       identity_state TEXT NOT NULL, first_seen TEXT, last_seen TEXT)", table))
  DBI::dbAppendTable(con, table, cbind(df, first_seen = "2026-01-01", last_seen = "2026-07-09"))
}

test_that("load_identity reads two name DBs and resolves against them", {
  d <- withr::local_tempdir()
  cran_db <- file.path(d, "cran-archive.db")
  bioc_db <- file.path(d, "bioc-metadata.db")
  .write_names_db(cran_db, "cran_names_all",
    data.frame(name_lower = c("mass", "maptools"),
               canonical_name = c("MASS", "maptools"),
               identity_state = c("live", "archived"), stringsAsFactors = FALSE))
  .write_names_db(bioc_db, "bioc_names_all",
    data.frame(name_lower = "complexheatmap", canonical_name = "ComplexHeatmap",
               identity_state = "live", stringsAsFactors = FALSE))

  maps <- load_identity(cran_db, bioc_db)
  expect_true(check_size(maps$n_cran, floor = 1L))
  expect_equal(maps$n_cran, 2L)
  expect_equal(maps$n_bioc, 1L)
  expect_equal(resolve_identity("mass", maps = maps)$canonical_name, "MASS")
  expect_equal(resolve_identity("complexheatmap", maps = maps)$origin, "bioc")
  expect_equal(resolve_identity("maptools", maps = maps)$identity_state, "archived")
  expect_false(resolve_identity("yr", maps = maps)$in_scope)
})

test_that("load_identity errors on a missing DB path", {
  expect_error(load_identity("/no/such/cran.db", "/no/such/bioc.db"), "not found")
})
