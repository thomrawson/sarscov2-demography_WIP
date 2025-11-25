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
                                region = region,
                                param_iterations = 10),
              use_draft = "newer")
}

orderly_develop_start("03_simulations",
            parameters = list(population_assumptions = "ONS_NHS_region_principal",
                              vaccine_assumptions = "reallocate_same_number",
                              region = "london"),
            use_draft = "newer")
################################################
# Combine simulations into one dataframe for plotting

orderly_run("04_combine_simulations",
                      parameters = list(population_assumptions = "ONS_NHS_region_principal",
                                        vaccine_assumptions = "reallocate_same_number",
                                        param_iterations = 10),
                      use_draft = "newer")


##################################################
#RTM BASELINE CHECK
orderly_run("02_parameters",
            parameters = list(population_assumptions = "rtm_baseline"),
            use_draft = "newer")
# Run simulations
regions <- sircovid::regions("england")
for(region in regions){
  # orderly_run("03_simulations",
  #                       parameters = list(population_assumptions = "ONS_NHS_region_principal",
  #                                         region = region),
  #                       use_draft = "newer")
  orderly_run("03_simulations",
              parameters = list(population_assumptions = "rtm_baseline",
                                region = region,
                                param_iterations = 1000),
              use_draft = "newer")
}

orderly_run("04_combine_simulations",
            parameters = list(population_assumptions = "rtm_baseline",
                              param_iterations = 10),
            use_draft = "newer")
