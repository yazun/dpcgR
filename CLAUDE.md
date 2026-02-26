# CLAUDE.md

## Project Overview

dpcgR is an R package for specialized tasks for Gaia DPCG data manipulation and visualisation, often working with big datasets. It provides PostgreSQL-backed data access, astronomical coordinate transformations, skymap generation, timeseries analysis, classification result extraction, and interactive/static plotting for the Gaia CU7 pipeline.

## Build & Development

- **Build system:** roxygen2 for documentation, standard R package tooling
- **Install from GitHub:** `devtools::install_github("yazun/dpcgR", force=TRUE)`
- **Reload after changes:** `reload(pkgload::inst("dpcgR"))`
- **Regenerate docs:** roxygen2 via RStudio (Build > Document) or `devtools::document()`
- **No formal test suite** — testing is manual via `\dontrun{}` examples in roxygen docs
- **RStudio project:** 2-space indentation, UTF-8 encoding

## Directory Structure

```
R/                  # All source code (~9,300 lines across 14 files)
man/                # Auto-generated roxygen2 docs (do not edit manually)
DESCRIPTION         # Package metadata (version, dependencies)
NAMESPACE           # Auto-generated exports/imports (do not edit manually)
```

## Key Source Files

| File | Purpose |
|------|---------|
| `dbconnection.R` | PostgreSQL connection management via RPostgres/DBI |
| `utils.R` | Coordinate transforms, DB conversions, timeseries folding |
| `plotting.R` | Aitoff skymaps, CMD plots (ggplot2-based, ~1,900 lines) |
| `plotlyPlotting.R` | Interactive Plotly visualizations |
| `tsDynamicPlots.R` | Timeseries dynamic plots |
| `brewingMarkdown.R` | R Markdown chunk generation from plot lists |
| `brewingSkymapsSOSFromClassif.R` | Skymap generation from classification data |
| `classificationExtractionForSOS.R` | Classifier config extraction from XML |
| `classificationHistogram.R` | Classification histogram analysis |
| `sosHistogramingParallel.R` | Parallel histogram processing (future.apply) |
| `importFinalSet.R` | Final selection import from CSV/OwnCloud |
| `derivedTsGraphVis.R` | Timeseries graph visualization (visNetwork) |
| `plotRunMonitoring.R` | Run monitoring plots |

## Code Conventions

- **Documentation:** roxygen2 (`#'`) with `@title`, `@param`, `@return`, `@export`, `@importFrom`
- **Function naming:** mixed — camelCase for public functions (`plotAitoffGalactic`), snake_case for internals (`sanitize_identifier`)
- **Pipes:** magrittr `%>%` throughout
- **Data manipulation:** tidyverse (dplyr, tidyr, readr, dbplyr)
- **SQL:** built with `sprintf()` string interpolation, PostgreSQL-specific (xpath, jsonb, CTEs, window functions)
- **Error handling:** `message()`/`cat()` for user feedback, `warning()` for issues, minimal `stop()`
- **Parallelism:** `future.apply` for histogram generation

## Database

- PostgreSQL via RPostgres/DBI
- Key tables: `run` (with `fconfiguration` XML), `dr3_common_export`, `dr3_classification_export`, `dr4_ops_cs48_mv.dr4_final_run_selection`
- Functions: `clean_common_export()`, `populate_common_export()`
- Schema operations under `cu7gva` role

## Key Astronomical Concepts

- **Aitoff-Hammer projection** for galactic coordinate skymaps
- **HealPix** (Hierarchical Equal Area isoLatitude Pixelation) levels 0-10
- **Coordinate systems:** galactic (gl/gb), ecliptic, RA/Dec
- **SOS** (Specific Object Study) modules as classification units
- **Timeseries phase-folding** with reference time

## Dependencies (by category)

- **Database:** RPostgres, DBI, dbplyr
- **Data:** dplyr, tidyr, readr, jsonlite
- **Visualization:** ggplot2, plotly, highcharter, scattermore, ggnewscale, ggpointdensity, visNetwork
- **Reports:** htmlwidgets, knitr, rmarkdown, brew
- **Parallelism:** future.apply
- **Utilities:** magrittr, bit64
