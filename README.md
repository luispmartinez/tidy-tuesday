# tidy-tuesday

My [TidyTuesday](https://github.com/rfordatascience/tidytuesday) 2026 work in R and Tableau, plus a toolkit for profiling any new dataset.

## Weeks

| Week | Topic | Viz | Code |
|---|---|---|---|
| 2026-01-13 | Languages of Africa | [Tableau Public](https://public.tableau.com/views/AfricaLanguages/AfricanLanguages) | [2026-01-13](2026-01-13) |
| 2026-01-20 | NASA Astronomy Picture of the Day | [Tableau Public](https://public.tableau.com/views/APOD/APOD) | [2026-01-20](2026-01-20) |


## Layout

- `R/` holds the reusable code. `eda_toolkit.R` builds a ten-section profile report for any table. `scan_and_profile.R` runs it on every CSV and Excel sheet in a week's `drop/` folder, skips finished reports, and lists failures.
- `YYYY-MM-DD/` is one folder per week, named like the TidyTuesday folder. It holds the week's scripts (`clean_*.R` or `prep_*.R`, `analyze_*.R`, `build_*_canvas.R`), a README, and a PNG of the dashboard. `drop/` (raw data) and `reports/` (generated) stay on my machine and are not in the repo.
- `tests/` holds sample inputs for checking the runner. They stay on my machine, like `drop/` and `reports/`.
- `renv.lock` pins the R packages. Restore them with `renv::restore()`.

## Start a new week

1. Create `YYYY-MM-DD/drop/` and download the week's files into it.
2. Set `WEEK` at the top of `R/scan_and_profile.R`, then source it.
3. Read the reports in `YYYY-MM-DD/reports/`, then write the week's cleaning script.

Built in R and Tableau by Luis Pablo Martinez.
