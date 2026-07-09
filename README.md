# robservatory

Internal utilities shared across the r-observatory pipelines.

The initial content resolves an R package name to its CRAN or Bioconductor
identity from two published name tables (`cran_names_all`, `bioc_names_all`),
with a size gate that rejects a partial or shrunken fetch.

```r
maps <- robservatory::load_identity("cran-archive.db", "bioc-metadata.db")
robservatory::resolve_identity("complexheatmap", maps = maps)
#> $origin        "bioc"
#> $canonical_name "ComplexHeatmap"
#> $identity_state "live"
#> $in_scope       TRUE
```
