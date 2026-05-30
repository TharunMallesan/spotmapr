# spotmapr 0.1.11 — initial CRAN submission

## R CMD check results

* `R CMD check --as-cran` ran with **0 errors, 0 warnings, 0 notes** on:
  * Local: R 4.6.0, Windows 11 (x86_64)

## Cross-platform checks

* Win-builder (R-devel, R-release): pending — running before submission
* R-hub (Linux, Windows, macOS): pending — running before submission

## Reverse dependencies

This is a new release; there are no reverse dependencies.

## Test environments and reproducibility

* All examples that build a map are wrapped in `\dontrun{}` because they
  require an input data file from the user.
* Unit tests cover the column-detection and value-validation helpers
  (`detect_lat_lon`, `detect_outcome`). The spatial join and leaflet
  rendering paths are exercised manually because they depend on the
  bundled boundary files and rely on browser rendering for the
  interactive widget.

## Bundled data files

`inst/extdata/state_boundary_lite.fgb` and
`inst/extdata/district_boundary_lite.fgb` are simplified administrative
boundary geometries for India derived from publicly available open data
sources (GADM / OpenStreetMap-derived). They contain only geometry plus
the state and district name attributes, are redistributable under the
respective open licences, and are stored in compressed FlatGeobuf format
to keep the package well under the CRAN 5 MB limit.

## Maintainer

Muniraj Mallesan <adarv@nieicmr.org.in>
