#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Get SLURM task ID
task_id <- as.integer(args[1])

pop_assumptions <- c(
  "ONS_NHS_region_principal",
  "ONS_NHS_region_low_migration",
  "ONS_NHS_region_high_migration"
)

pop_years <- c(2047, 
               #2042, 
               2037, 
               #2032, 
               2027)

vacc_assumptions <- c("baseline",
                      "baseline_scaled_up",
                      "baseline_scaled_up_and_reallocated",
                      "vaccinate_young_earlier")

#vacc_assumptions <- "1_month_earlier"

# Create all combinations
param_grid <- expand.grid(
  i = pop_assumptions,
  j = pop_years,
  k = vacc_assumptions,
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
  "02_parameters",
  parameters = list(
    population_assumptions = i,
    population_year = j,
    vaccine_assumptions = k
  ),
  use_draft = "newer"
)