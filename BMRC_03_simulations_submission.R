#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Get SLURM task ID
task_id <- as.integer(args[1])

pop_assumptions <- c(
  "ONS_NHS_region_principal",
  "ONS_NHS_region_low_migration",
  "ONS_NHS_region_high_migration"
)

pop_years <- c(2047, 2042, 2037, 2032, 2027)

regions <- sircovid:::regions("england")

# Create all combinations
param_grid <- expand.grid(
  i = regions,
  j = pop_assumptions,
  k = pop_years,
  stringsAsFactors = FALSE
)

# Safety check
if (task_id > nrow(param_grid)) {
  stop("Task ID exceeds number of parameter combinations")
}

# Select this job's parameters
i <- param_grid$i[task_id]
j <- param_grid$j[task_id]
k <- param_grid$k[task_id]

library(orderly1)

orderly_run(
  "03_simulations",
  parameters = list(
    region = i,
    population_assumptions = j,
    population_year = k,
    vaccine_assumptions = "baseline_scaled_up",
    param_iterations = 50000
  ),
  use_draft = "newer"
)