.df <- function(name_lower, canonical_name, identity_state) {
  data.frame(name_lower = name_lower, canonical_name = canonical_name,
             identity_state = identity_state, stringsAsFactors = FALSE)
}

test_that("resolve_identity applies precedence, casing, and scope", {
  # maptools is CRAN-only and archived; biocgenerics is the dual-listed case
  # (archived on CRAN, live on Bioc) that exercises the precedence tie-break.
  cran <- .df(c("mass", "ggplot2", "maptools", "biocgenerics"),
              c("MASS", "ggplot2", "maptools", "BiocGenerics"),
              c("live", "live", "archived", "archived"))
  bioc <- .df(c("complexheatmap", "biocgenerics"),
              c("ComplexHeatmap", "BiocGenerics"),
              c("live", "live"))
  maps <- resolve_identity_set(cran, bioc)

  # live CRAN
  expect_equal(resolve_identity("mass", maps = maps),
               list(origin = "cran", canonical_name = "MASS",
                    identity_state = "live", in_scope = TRUE))
  # r-<Bioc>: the stripped token is a live Bioc package, resolved from a single lookup
  expect_equal(resolve_identity("complexheatmap", maps = maps)$origin, "bioc")
  expect_equal(resolve_identity("complexheatmap", maps = maps)$canonical_name, "ComplexHeatmap")
  # archived CRAN stays cran, marked archived
  expect_equal(resolve_identity("maptools", maps = maps)$origin, "cran")
  expect_equal(resolve_identity("maptools", maps = maps)$identity_state, "archived")
  # in BOTH: live-Bioc (BiocGenerics live) beats archived-CRAN
  expect_equal(resolve_identity("biocgenerics", maps = maps)$origin, "bioc")
  expect_equal(resolve_identity("biocgenerics", maps = maps)$identity_state, "live")
  # a miss is out of scope, not a fabricated cran
  expect_equal(resolve_identity("yr", maps = maps),
               list(origin = "other", canonical_name = NA_character_,
                    identity_state = NA_character_, in_scope = FALSE))
  # lookup is case-insensitive on input
  expect_equal(resolve_identity("MASS", maps = maps)$origin, "cran")
  # positional: the second argument is maps
  expect_equal(resolve_identity("ggplot2", maps)$origin, "cran")
})

test_that("resolve_identity warns when prefix_hint disagrees but trusts the tables", {
  cran <- .df("mass", "MASS", "live")
  bioc <- .df(character(0), character(0), character(0))
  maps <- resolve_identity_set(cran, bioc)
  expect_warning(res <- resolve_identity("mass", prefix_hint = "bioc", maps = maps),
                 "disagrees")
  expect_equal(res$origin, "cran")
})

test_that("resolve_identity_set handles empty inputs", {
  empty <- .df(character(0), character(0), character(0))
  maps <- resolve_identity_set(empty, empty)
  expect_equal(maps$n_cran, 0L)
  expect_false(resolve_identity("anything", maps = maps)$in_scope)
})
