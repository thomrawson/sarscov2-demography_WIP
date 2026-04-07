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

# Create all combinations
param_grid <- expand.grid(
  j = pop_assumptions,
  k = pop_years,
  stringsAsFactors = FALSE
)

param_grid$v <- "baseline_scaled_up"
#Add in the baseline
param_grid <- rbind(param_grid, data.frame(j = "rtm_baseline",
                                           k = 2019,
                                           v = "baseline"))

# Safety check
if (task_id > nrow(param_grid)) {
  stop("Task ID exceeds number of parameter combinations")
}

# Select this job's parameters
j <- param_grid$j[task_id]
k <- param_grid$k[task_id]
v <- param_grid$v[task_id]

library(orderly1)

orderly_run("04_combine_simulations",
            parameters = list(population_assumptions = j,
                              population_year = k,
                              vaccine_assumptions = v,
                              param_iterations = 50000),
            use_draft = "newer")