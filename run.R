library(orderly1)
# This takes takes the raw ONS population estimates, cleans it and outputs an .rds formatted for our downstream purposes
orderly_run("00_population_projections")

# This task simulates the role of what was once a live data pulling task. Now it just puts a load of model parameters into a trackable dependancy
parsed_data_task <- orderly_run("01_parsed_data")
orderly_commit(parsed_data_task)

################################################
# Parameters Task
# A task that sets all the "rules" that the model simulation will follow, such as what assumptions to use.
orderly_run("02_parameters",
            parameters = list(population_assumptions = "ONS_NHS_region_principal", # "ONS_NHS_region_principal" is the baseline ONS regional projections for 2047, 
                                                                                   #  "rtm_baseline" for your bog-standard normal model runs
                                                                                  # "ONS_NHS_region_low" is the low migration scenario ONS regional projections for 2047, 
                                                                                  # "ONS_NHS_region_high" is the low migration scenario ONS regional projections for 2047, 
                              
                              population_year = 2032,                             #2019 is the default which should only be used for "rtm_baseline" population_assumptions. Otherwise, 2027,2032,2037,2042,2047
                              
                              vaccine_assumptions = "baseline_scaled_up"),    #"reallocate_same_number" keeps exactly the same number of daily vaccines, but redistributes them to different ages according to priority list
                                                                                  #"baseline" to use exact real world vaccine data
                                                                                  #"baseline_scaled_up" to keep the distribution but just scale it to match population change
            use_draft = "newer")

################################################
# Run simulations
# This bit can take a while
regions <- sircovid::regions("england")
for(r in regions){
  # orderly_run("03_simulations",
  #                       parameters = list(population_assumptions = "ONS_NHS_region_principal",
  #                                         region = region),
  #                       use_draft = "newer")
  orderly_run("03_simulations",
              parameters = list(population_assumptions = "rtm_baseline",
                                vaccine_assumptions = "baseline",
                                region = r,
                                param_iterations = 100),
              use_draft = "newer")
  
  # orderly_run("03_simulations",
  #             parameters = list(population_assumptions = "ONS_NHS_region_principal",
  #                               population_year = 2032,
  #                               vaccine_assumptions = "baseline_scaled_up",
  #                               region = r,
  #                               param_iterations = 100),
  #             use_draft = "newer")
  
  # orderly_run("03_simulations",
  #             parameters = list(population_assumptions = "ONS_NHS_region_low_migration",
  #                               vaccine_assumptions = "baseline_scaled_up",
  #                               region = r,
  #                               param_iterations = 10000),
  #             use_draft = "newer")
  # 
  # orderly_run("03_simulations",
  #             parameters = list(population_assumptions = "ONS_NHS_region_high_migration",
  #                               vaccine_assumptions = "baseline_scaled_up",
  #                               region = r,
  #                               param_iterations = 10000),
  #             use_draft = "newer")
  
}



orderly_develop_start("03_simulations",
            parameters = list(population_assumptions = "ONS_NHS_region_principal",
                              vaccine_assumptions = "reallocate_same_number",
                              region = "london"),
            use_draft = "newer")

orderly_run("03_simulations",
                      parameters = list(population_assumptions = "ONS_NHS_region_principal",
                                        vaccine_assumptions = "reallocate_same_number",
                                        region = "london",
                                        param_iterations = 100),
                      use_draft = "newer")
################################################
# Combine simulations into one dataframe for plotting

orderly_run("04_combine_simulations",
                      parameters = list(population_assumptions = "ONS_NHS_region_principal",
                                        population_year = 2032,
                                        vaccine_assumptions = "baseline_scaled_up",
                                        param_iterations = 100),
                      use_draft = "newer")


##################################################
#RTM BASELINE CHECK
orderly_run("02_parameters",
            parameters = list(population_assumptions = "ONS_NHS_region_low_migration",
                              vaccine_assumptions = "baseline_scaled_up"),
            use_draft = "newer")
# Run simulations
regions <- sircovid::regions("england")
for(region in regions){
  orderly_run("03_simulations",
                        parameters = list(population_assumptions = "rtm_baseline",
                                          vaccine_assumptions = "baseline",
                                          region = region,
                                          param_iterations = 100),
                        use_draft = "newer")
  # orderly_run("03_simulations",
  #             parameters = list(population_assumptions = "rtm_baseline",
  #                               region = region,
  #                               param_iterations = 1000),
  #             use_draft = "newer")
}

orderly_run("04_combine_simulations",
            parameters = list(population_assumptions = "rtm_baseline",
                              vaccine_assumptions = "baseline",
                              param_iterations = 100),
            use_draft = "newer")

#########################
# Plotting
# Examine the difference between the population assumptions
orderly_run("0S1_population_plots",
            parameters = list(population_assumptions = "ONS_NHS_region_high_migration"),
            use_draft = "newer")

orderly_run("0S2_health_outcome_plots",
                      parameters = list(population_assumptions = "ONS_NHS_region_low_migration",
                                        vaccine_assumptions = "baseline_scaled_up",
                                        param_iterations = 100),
                      use_draft = "newer")


orderly_run("05_combine_vaccinations",
            parameters = list(population_assumptions = "ONS_NHS_region_principal",
                              vaccine_assumptions = "baseline_scaled_up",
                              param_iterations = 20),
            use_draft = "newer")


orderly_develop_start("0S3_vaccine_schedule_plots",
            parameters = list(population_assumptions = "ONS_NHS_region_principal",
                              vaccine_assumptions = "reallocate_same_number",
                              param_iterations = 100),
            use_draft = "newer")

orderly_run("0S3_vaccine_schedule_plots",
                      parameters = list(population_assumptions = "ONS_NHS_region_principal",
                                        vaccine_assumptions = "baseline_scaled_up",
                                        param_iterations = 20),
                      use_draft = "newer")
