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

# vacc_assumptions <- c("baseline",
#                       "baseline_scaled_up",
#                       "baseline_scaled_up_and_reallocated")

vacc_assumptions <- "1_month_earlier"

# Create all combinations
param_grid <- expand.grid(
  i = pop_assumptions,
  j = pop_years,
  k = vacc_assumptions,
  stringsAsFactors = FALSE
)

#Add in the baseline
param_grid <- rbind(param_grid, data.frame(i = "rtm_baseline",
                                           j = 2019,
                                           k = "baseline"))

# Safety check
if (task_id > nrow(param_grid)) {
  stop("Task ID exceeds number of parameter combinations")
}

# Select this job's parameters
i <- param_grid$i[task_id]
j <- param_grid$j[task_id]
k <- param_grid$k[task_id]

library(orderly1)

orderly_run("04_combine_simulations",
            parameters = list(population_assumptions = i,
                              population_year = j,
                              vaccine_assumptions = k,
                              param_iterations = 50000),
            use_draft = "newer")

orderly_run("06_parsed_simulations",
            parameters = list(population_assumptions = i,
                              population_year = j,
                              vaccine_assumptions = k,
                              param_iterations = 50000),
            use_draft = "newer")