#!/usr/bin/env Rscript

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check inputs
if (length(args) < 2) {
  stop("Usage: Rscript run_model.R <population_assumptions> <population_year>")
}

# Assign arguments
j <- args[1]
k <- args[2]

# (Optional) convert types if needed
# Example: if k should be numeric
k <- as.numeric(k)

# Load required library
library(orderly1)  # or whichever package provides orderly_run

# Run your function
orderly_run(
  "02_parameters",
  parameters = list(
    population_assumptions = j,
    population_year = k,
    vaccine_assumptions = "baseline_scaled_up"
  ),
  use_draft = "newer"
)