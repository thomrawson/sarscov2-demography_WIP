#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Get SLURM task ID
task_id <- as.integer(args[1])

regions <- sircovid:::regions("england")

# Create all combinations
param_grid <- expand.grid(
  i = regions,
  stringsAsFactors = FALSE
)

# Safety check
if (task_id > nrow(param_grid)) {
  stop("Task ID exceeds number of parameter combinations")
}

# Select this job's parameters
i <- param_grid$i[task_id]

library(orderly1)

orderly_run(
  "02_parameters",
  parameters = list(
    population_assumptions = "rtm_baseline",
    population_year = 2019,
    #vaccine_assumptions = "baseline",
    vaccine_assumptions = "no_vaccines"
  ),
  use_draft = "newer"
)

orderly_run(
  "03_simulations",
  parameters = list(
    region = i,
    population_assumptions = "rtm_baseline",
    population_year = 2019,
    #vaccine_assumptions = "baseline",
    vaccine_assumptions = "no_vaccines",
    param_iterations = 50000
  ),
  use_draft = "newer"
)