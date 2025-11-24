library(orderly1)

orderly_run("00_population_projections")

parsed_data_task <- orderly_run("01_parsed_data")
orderly_commit(parsed_data_task)

################################################
# Parameters Task

orderly_run("02_parameters",
            parameters = list(population_assumptions = "ONS_NHS_region_principal",
                              vaccine_assumptions = "reallocate_same_number"),
            use_draft = "newer")

################################################
# Run simulations
regions <- sircovid::regions("england")
for(region in regions){
  # orderly_run("03_simulations",
  #                       parameters = list(population_assumptions = "ONS_NHS_region_principal",
  #                                         region = region),
  #                       use_draft = "newer")
  orderly_run("03_simulations",
              parameters = list(population_assumptions = "ONS_NHS_region_principal",
                                vaccine_assumptions = "reallocate_same_number",
                                region = region),
              use_draft = "newer")
}


################################################
# Combine simulations into one dataframe for plotting


