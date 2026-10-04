library(tidyverse)
library(janitor)
library(skimr)
library(naniar)
library(glue)
library(broom)

profile_shape <- function(df) {
  paste0(nrow(df), " rows x ", ncol(df), " columns")
}

profile_types <- function(df) {
  types <- sapply(df, class)
  paste(paste0(names(types), ": ", types), collapse = "\n")
  # last line: your nested paste call, without cat()
}
