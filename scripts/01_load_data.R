library(tidyverse)
library(patchwork)
library(here)

insurance_cost_raw <- read_csv(
  here("data", "insurance_costs.csv"),
  show_col_types = FALSE
)
