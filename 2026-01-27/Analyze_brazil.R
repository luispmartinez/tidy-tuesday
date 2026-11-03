# Step 1: Load packages
library(tidyverse)

# Step 2: Settings
IN_FOLDER  <- path.expand("~/TableauData/Tidy Tuesday")
INPUT_PATH <- file.path(IN_FOLDER, "brazil_companies_clean.csv")
val <- function(tbl, size, col) tbl[[col]][tbl$company_size == size]

write_table <- function(df, name) {
  out <- file.path(IN_FOLDER, paste0(name, ".csv"))
  tmp <- paste0(out, ".tmp")
  write_csv(df, tmp)
  ok <- file.rename(tmp, out)
  stopifnot("rename failed, file not written" = ok)
  invisible(out)
}

# Step 3: Read the clean file
d <- read_csv(INPUT_PATH, show_col_types = FALSE)

# Step 4: Typical capital by size, all rows
# Q: why the median and not the mean here?
d |> group_by(company_size) |> summarise(n = n(), typical = median(capital_stock))

# Step 5: Same table without the flagged rows
# Q: before you run it, would you expect the medians to move much? Why?
d |> filter(!capital_flag) |> group_by(company_size) |> summarise(n = n(), typical = median(capital_stock))

# Step 6: Where do the flagged rows sit?
# Q: are the 233 spread evenly across the three sizes, or concentrated?
d |> count(company_size, capital_flag)

# Step 7: How spread out is capital within each size?
# Q: a median hides the spread. What do the 25th and 75th percentiles add?
d |> filter(!capital_flag) |> group_by(company_size) |>
  summarise(n = n(), p25 = quantile(capital_stock, .25), median = median(capital_stock), p75 = quantile(capital_stock, .75))

# Step 8: Is the "other" gap a shift of the whole group or just a few big companies?
# Q: what share of each size has capital above 1,000,000?
d |> filter(!capital_flag) |> group_by(company_size) |> summarise(pct_over_1m = mean(capital_stock > 1000000))

# Step 9: Export tables for Tableau
dir.create(IN_FOLDER, showWarnings = FALSE, recursive = TRUE)
size_summary <- d |> filter(!capital_flag) |> group_by(company_size) |>
  summarise(n = n(), p25 = quantile(capital_stock, .25), median = median(capital_stock),
            p75 = quantile(capital_stock, .75),
            pct_over_1m = mean(capital_stock > 1000000))
write_table(size_summary, "size_summary")

# Step 10: Checks
# Q: which numbers from the steps above should never change?
stopifnot(
  "row count is not 141332"    = nrow(d) == 141332,
  "flag count is not 233"      = sum(d$capital_flag) == 233,
  "size_summary is not 3 rows" = nrow(size_summary) == 3,
  "pct_over_1m outside 0 to 1" = all(size_summary$pct_over_1m >= 0 & size_summary$pct_over_1m <= 1),
  "over-1M share should rise: micro, small, other" =
    val(size_summary, "small-enterprise", "pct_over_1m") > val(size_summary, "micro-enterprise", "pct_over_1m") &
    val(size_summary, "other", "pct_over_1m") > val(size_summary, "small-enterprise", "pct_over_1m")
)

# Step 11: Rank tests, higher size class vs lower (placeholders removed)
# Needs: d (clean data), IN_FOLDER, tidyverse loaded

pairs <- tribble(~higher,            ~lower,
                 "other",            "micro-enterprise",
                 "other",            "small-enterprise",
                 "small-enterprise", "micro-enterprise")

stopifnot("pair name not in company_size" =
            all(c(pairs$higher, pairs$lower) %in% unique(d$company_size)))

shift_summary <- pairs |> pmap_dfr(function(higher, lower) {
  pair <- d |> filter(!capital_flag, company_size %in% c(higher, lower)) |>
    mutate(company_size = factor(company_size, levels = c(higher, lower)))
  # exact = FALSE: tied round amounts make exact p-values and intervals impossible,
  # and the exact method runs out of memory at this sample size.
  w <- wilcox.test(capital_stock ~ company_size, data = pair,
                   conf.int = TRUE, exact = FALSE)
  broom::tidy(w) |> mutate(higher   = higher,
                           lower    = lower,
                           n_higher = sum(pair$company_size == higher),
                           n_lower  = sum(pair$company_size == lower))
}) |>
  mutate(prob_higher = statistic / (as.numeric(n_higher) * n_lower))

# Checks
stopifnot(
  "estimates must all be positive"   = all(shift_summary$estimate > 0),
  "prob_higher outside 0.5 to 1"     = all(shift_summary$prob_higher > 0.5 &
                                             shift_summary$prob_higher <= 1),
  "shifts out of order"              = !is.unsorted(rev(shift_summary$estimate)),
  "group size does not match data"   = shift_summary$n_higher[1] ==
    sum(d$company_size == "other" & !d$capital_flag)
)

# Write (temp file then rename)
write_table(shift_summary, "shift_summary")

shift_summary |> select(higher, lower, estimate, n_higher, n_lower, prob_higher)

# Step 12: The 233 block (counts, capital, means by size and flag)
flag_summary <- d |>
  group_by(company_size, capital_flag) |>
  summarise(n = n_distinct(company_id), total_capital = sum(capital_stock), mean_capital = mean(capital_stock), .groups = "drop")

# Write (temp file then rename)
write_table(flag_summary, "flag_summary")

read_csv(file.path(IN_FOLDER, "flag_summary.csv"), show_col_types = FALSE) |> glimpse()


# Step 13: One row per size, with and without the 233
size_flag <- flag_summary |>
  group_by(company_size) |>
  summarise(n_total       = sum(n),
            n_flagged     = sum(n[capital_flag]),
            mean_with = sum(total_capital) / sum(n),
            mean_without  = sum(total_capital[!capital_flag]) / sum(n[!capital_flag]),
            .groups = "drop")
size_val <- function(size, col) size_flag[[col]][size_flag$company_size == size]

stopifnot(
  "small beats other with the 233"      = size_val("small-enterprise", "mean_with") > size_val("other", "mean_with"),
  "other leads without the 233"         = size_val("other", "mean_without") > size_val("small-enterprise", "mean_without"),
  "group totals match d"                = sum(size_flag$n_total) == nrow(d),
  "flagged count in size_flag matches d" = sum(size_flag$n_flagged) == sum(d$capital_flag)
)

# Write (temp file then rename)
write_table(size_flag, "size_flag")


# Step 14: The 233 block shares (from flag_summary only)
flag_share <- flag_summary |>
  summarise(n_flagged         = sum(n[capital_flag]),
            n_firms           = sum(n),
            pct_firms         = n_flagged / n_firms,
            pct_capital       = sum(total_capital[capital_flag]) / sum(total_capital),
            flagged_capital_t = sum(total_capital[capital_flag]) / 1e12,
            total_capital_t   = sum(total_capital) / 1e12)

stopifnot(
  "flag_share has the count columns" = all(c("n_flagged", "n_firms") %in% names(flag_share)),
  "pct_capital should be between 0.80 and 0.90" =
    flag_share$pct_capital > 0.80 & flag_share$pct_capital < 0.90,
  "flagged count in flag_share matches d" =
    flag_share$n_flagged == sum(d$capital_flag),
  "firm count in flag_share matches d" =
    flag_share$n_firms == nrow(d)
)

# Write (temp file then rename)
write_table(flag_share, "flag_share")

read_csv(file.path(IN_FOLDER, "flag_share.csv"), show_col_types = FALSE) |> glimpse()

# Step 15: The R$200,000 spike (base: all 141,332 firms; none of the 233 sit at 200,000)
round_share <- d |>
  summarise(n_200k   = sum(capital_stock == 200000),
            pct_200k = mean(capital_stock == 200000),
            one_in   = round(1 / pct_200k))
stopifnot(
  "none of the 233 sit at 200,000" = round_share$n_200k == sum(d$capital_stock == 200000 & !d$capital_flag),
  "pct_200k outside 0.15 to 0.25" = round_share$pct_200k > 0.15 & round_share$pct_200k < 0.25
)
write_table(round_share, "round_share")

# Step 16: Headline ratios (rounding rule: “whole numbers, nearest”.)
headline <- tibble(
  median_ratio_small = val(size_summary, "other", "median") / val(size_summary, "small-enterprise", "median"),
  median_ratio_micro = val(size_summary, "other", "median") / val(size_summary, "micro-enterprise", "median"),
  mean_ratio_small   = val(size_flag, "other", "mean_without") / val(size_flag, "small-enterprise", "mean_without")
) |>
  mutate(across(everything(), ~ round(.x), .names = "{.col}_label"))

stopifnot(
  "median ratio vs small outside 2.5 to 3.5" =
    headline$median_ratio_small > 2.5 & headline$median_ratio_small < 3.5,
  "all firms minus unflagged firms equals the flagged count" =
    sum(size_flag$n_total) - sum(size_summary$n) == sum(size_flag$n_flagged),
  "mean ratio vs small outside 4.5 to 5.5" =
    headline$mean_ratio_small > 4.5 & headline$mean_ratio_small < 5.5)

write_table(headline, "headline")