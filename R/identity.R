#' Build a unified identity lookup from a CRAN and a Bioc name set.
#'
#' Each input is a data.frame with character columns `name_lower`,
#' `canonical_name`, and `identity_state` (one of "live" or "archived"). A name
#' present in more than one set is resolved by fixed precedence:
#' live-CRAN, then live-Bioc, then archived-CRAN, then archived-Bioc.
#'
#' @param cran_names data.frame of CRAN names (may be empty).
#' @param bioc_names data.frame of Bioc names (may be empty).
#' @return An environment with `lookup` (a hashed environment keyed by
#'   `name_lower` whose values are lists of origin/canonical_name/identity_state),
#'   `n_cran`, and `n_bioc`.
#' @export
resolve_identity_set <- function(cran_names, bioc_names) {
  mk <- function(df, origin) {
    if (is.null(df) || nrow(df) == 0L) {
      return(data.frame(name_lower = character(0), canonical_name = character(0),
                        origin = character(0), identity_state = character(0),
                        rank = integer(0), stringsAsFactors = FALSE))
    }
    live <- df$identity_state == "live"
    # live gets the smaller (better) rank; cran outranks bioc within a liveness.
    base_rank <- if (origin == "cran") 1L else 2L
    data.frame(name_lower = df$name_lower, canonical_name = df$canonical_name,
               origin = origin, identity_state = df$identity_state,
               rank = ifelse(live, base_rank, base_rank + 2L),
               stringsAsFactors = FALSE)
  }
  all <- rbind(mk(cran_names, "cran"), mk(bioc_names, "bioc"))
  if (nrow(all) > 0L) {
    all <- all[order(all$rank), , drop = FALSE]
    all <- all[!duplicated(all$name_lower), , drop = FALSE]
  }
  lookup <- new.env(parent = emptyenv(), size = max(1L, nrow(all)))
  if (nrow(all) > 0L) {
    values <- lapply(seq_len(nrow(all)), function(i) {
      list(origin = all$origin[i], canonical_name = all$canonical_name[i],
           identity_state = all$identity_state[i])
    })
    names(values) <- all$name_lower
    list2env(values, envir = lookup)
  }
  maps <- new.env(parent = emptyenv())
  maps$lookup <- lookup
  maps$n_cran <- if (is.null(cran_names)) 0L else nrow(cran_names)
  maps$n_bioc <- if (is.null(bioc_names)) 0L else nrow(bioc_names)
  maps
}

#' Resolve one package name against a unified identity lookup.
#'
#' @param name Bare package token (the caller has already stripped any channel
#'   prefix such as "r-", "bioconductor-", "r-cran-").
#' @param maps The object returned by `resolve_identity_set` or `load_identity`.
#' @param prefix_hint Optional channel-asserted origin ("cran" or "bioc") used
#'   only to log a disagreement; the tables always decide.
#' @return A list with `origin` ("cran"|"bioc"|"other"), `canonical_name`,
#'   `identity_state` ("live"|"archived"|NA), and `in_scope` (logical).
#' @export
resolve_identity <- function(name, maps, prefix_hint = NULL) {
  key <- tolower(name)
  hit <- get0(key, envir = maps$lookup, inherits = FALSE)
  if (is.null(hit)) {
    return(list(origin = "other", canonical_name = NA_character_,
                identity_state = NA_character_, in_scope = FALSE))
  }
  if (!is.null(prefix_hint) && length(prefix_hint) == 1L && !is.na(prefix_hint) &&
      prefix_hint != hit$origin) {
    warning(sprintf(
      "prefix_hint '%s' disagrees with resolved origin '%s' for '%s'; trusting the tables",
      prefix_hint, hit$origin, name))
  }
  list(origin = hit$origin, canonical_name = hit$canonical_name,
       identity_state = hit$identity_state, in_scope = TRUE)
}
