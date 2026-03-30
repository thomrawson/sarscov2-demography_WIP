library(orderly1)
# This takes takes the raw ONS population estimates, cleans it and outputs an .rds formatted for our downstream purposes
orderly_run("00_population_projections")
orderly_run("01_parsed_data")