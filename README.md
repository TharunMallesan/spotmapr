# spotmapr

> Interactive epidemiological spot maps for India — the R port of the
> Python [`spotmap`](https://github.com/TharunMallesan/spotmap) package.

`spotmapr` builds zoomable HTML spot maps from case-control data, with:

- Automatic detection of latitude, longitude, and outcome columns
- Bundled state and district boundary shapefiles (no external download)
- Two display modes: **dot density** (clustered) and **spot pins**
- Live colour pickers, pin-size slider, PNG / PDF export
- A built-in interactive wizard for non-coders

## Installation

```r
# install.packages("remotes")
remotes::install_github("ADARV-Epi-hub/spotmapr")
```

### System requirements

- R >= 4.1.0
- The `sf` package, which on Linux needs `libgdal-dev`, `libproj-dev`,
  and `libgeos-dev` (see the
  [sf installation guide](https://r-spatial.github.io/sf/#installing))

## Quick start

```r
library(spotmapr)

# Option 1 — interactive wizard
spot_map()

# Option 2 — direct call with auto-detection
spot_map(
  data   = "my_cases.csv",
  output = "map.html"
)

# Option 3 — explicit column names
spot_map(
  data        = my_df,
  lat_col     = "lat",
  lon_col     = "long",
  outcome_col = "case_control",
  case_value  = "case",
  output      = "map.html"
)
```

## Input data

A CSV / Excel / TSV file (or `data.frame`) with, at minimum:

| Column          | Example values        |
| --------------- | --------------------- |
| latitude        | `11.01`, `28.61`      |
| longitude       | `76.96`, `77.21`      |
| outcome         | `case`, `control`     |

Column names are auto-detected from common variants (`lat`,
`latitude`, `y`; `lon`, `long`, `longitude`, `x`; `outcome`, `status`,
`case_control`, `class`).

## Output

A self-contained HTML file you can open in any browser, or share. The
sidebar lets you:

- Toggle **Dot Density** ↔ **Spot Pins**
- Pick **cluster** / **case** / **control** colours
- Resize pins with a slider
- Show **Cases Only** or **Cases & Controls**
- Download the map as **PNG** or print as **PDF**

## Custom boundaries

To use your own state / district shapefiles:

```r
spot_map(
  data         = "cases.csv",
  state_shp    = "path/to/states.shp",
  district_shp = "path/to/districts.shp",
  output       = "map.html"
)
```

## License

MIT © Tharun Mallesan
