# tidy-tuesday

R code for [TidyTuesday](https://github.com/rfordatascience/tidytuesday) 2026, plus a reusable toolkit for profiling any new dataset.

## Week 2: Languages of Africa (2026-01-13)

**Viz:** [Africa Languages on Tableau Public](<TABLEAU_PUBLIC_URL>)

Mirrored bars count the languages in each of 51 African countries, split by language family. A map of Africa lights up the region of any bar or country you hover or click.

**What the data shows:** Niger–Congo makes up 389 of the 510 languages (76%). A belt of Nilo-Saharan and Afroasiatic languages stands out from Chad to Ethiopia.

### How it fits together

1. `2026-01-13/clean_africa.R` reads the raw table, removes 34 exact duplicate rows, gives same-name languages their own `language_id`, groups family labels, and checks the result with `stopifnot()`.
2. `2026-01-13/build_africa_canvas.R` turns the clean table into one CSV of drawing coordinates: bars, axis, zone bands, labels, legend and a map. It builds the map from [Natural Earth](https://www.naturalearthdata.com/) outlines with `sf` and `rnaturalearth`.
3. Tableau reads that CSV and draws each layer with `MAKEPOINT`, so every position is set in R.

### Run it

1. Download `africa.csv` from the [TidyTuesday week folder](https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-01-13) into `2026-01-13/drop/`. Data files are not stored in this repo.
2. In R, restore the packages: `renv::restore()`
3. Source `clean_africa.R`, then `build_africa_canvas.R`.

Both scripts write to `~/TableauData/Tidy Tuesday/`. Change the paths at the top of each script to use another folder.

### Notes on the data

- Languages with no published speaker count are excluded, so counts are minimums.
- Congo combines the Republic of the Congo and the DR Congo. Guinea includes Guinea-Bissau. São Tomé and Príncipe is not in the data.
- Languages that share a name but differ in speaker count are counted as separate languages.
- Families follow Wikipedia. "Other" holds the Khoisan families, creoles and pidgins, and a few small families.

## The profiling toolkit

- `eda_toolkit.R` builds a ten-section Markdown report for any table: shape, types, missing values, duplicates, numeric and categorical summaries, group means, crosstabs, correlations and one-way models.
- `scan_and_profile.R` runs it on every CSV and Excel sheet in a folder, skips finished reports, and lists any failures.

## Credits

Data: Wikipedia, "Languages of Africa," via #TidyTuesday. Map outlines: Natural Earth. Built in R and Tableau by Luis Pablo Martinez.
