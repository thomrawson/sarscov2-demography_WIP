library(orderly1)

# A data task to take the raw ONS population estimates, clean it, and output as an .rds formatted for downstream purposes
ONS_data_task <- orderly_run("00_population_projections")
orderly_commit(ONS_data_task)

# This task simulates the role of what was once a live data pulling task. Now it links pre-saved model parameters to a trackable dependancy
parsed_data_task <- orderly_run("01_parsed_data")
orderly_commit(parsed_data_task)

################################################
# Parameters Task
# A task that sets the "rules" that the model simulation will follow, such as what assumptions to use.
orderly_run("02_parameters",
            parameters = list(population_assumptions = "ONS_NHS_region_principal", # "ONS_NHS_region_principal" is the baseline ONS regional projections for 2047, 
                                                                                   #  "rtm_baseline" for the historic baseline population
                                                                                  # "ONS_NHS_region_low_migration" is the low migration scenario ONS regional projections for 2047, 
                                                                                  # "ONS_NHS_region_high_migration" is the low migration scenario ONS regional projections for 2047, 
                              
                              population_year = 2047,                             #2019 is the default which should only be used for "rtm_baseline" population_assumptions. Otherwise, 2027,2037,2047 were run
                              
                              vaccine_assumptions = "baseline_scaled_up_and_reallocated"),   #"baseline" to use exact real world vaccine data - the 'historic vaccinations' scenario
                                                                                             #"baseline_scaled_up" to keep the distribution by age, but scale daily doses to match age-specific population change - the 'scaled by age' scenario
                                                                                             #"baseline_scaled_up_and_reallocated" to scale the total daily vaccines given by the region's population difference, and then re-allocate - the 'scaled and reallocated' scenario
                                                                                             #"vaccinate_young_earlier" to swap when vaccines are opened up to 50-54s with 25-29s. 55-59s with 30-34s. 60-64s with 35-39s. - an exploratory scenario not mentioned in publication
                                                                                             #"reallocate_same_number" keeps exactly the same number of daily vaccines, but redistributes them to different ages according to priority list - an exploratory scenario not mentioned in publication
                                                                                             #"no_vaccines" for 0 vaccine doses - an exploratory scenario not mentioned in publication
                                                                                             #"1_month_earlier" is same as baseline_scaled_up_and_reallocated but doses 1 and 2 are shifted forward 30 days - an exploratory scenario not mentioned in publication
            use_draft = "newer")  

# Build grid of all parameter tasks to be run
pop_assumptions <- c("ONS_NHS_region_principal", "ONS_NHS_region_low_migration", "ONS_NHS_region_high_migration")
pop_years <- c(2047, 2037, 2027)
vacc_assumptions <- c("baseline", "baseline_scaled_up", "baseline_scaled_up_and_reallocated")
# Create all combinations
param_grid <- expand.grid(
  i = pop_assumptions,
  j = pop_years,
  k = vacc_assumptions,
  stringsAsFactors = FALSE
)
#And add the true baseline
param_grid <- rbind(param_grid, data.frame(i = "rtm_baseline",
                                           j = 2019,
                                           k = "baseline"))
#Run each task
for(task_id in 1:nrow(param_grid)){
  orderly_run("02_parameters",
              parameters = list(population_assumptions = param_grid$i[task_id],
                                population_year = param_grid$j[task_id],
                                vaccine_assumptions = param_grid$k[task_id]),
              use_draft = "newer")
}

################################################
# Simulations task
# This will take a while, as we run the model n_iterations times to sample
# In reality, this was done for 50,000 iterations across multiple nodes of a HPC
n_iterations <- 20

regions <- sircovid::regions("england")
# Create all combinations
param_grid <- expand.grid(
  i = pop_assumptions,
  j = pop_years,
  k = vacc_assumptions,
  r = regions,
  stringsAsFactors = FALSE
)
#And the true baseline
baseline_grid <- expand.grid(
  i = "rtm_baseline",
  j = 2019,
  k = "baseline",
  r = regions,
  stringsAsFactors = FALSE
)
param_grid <- rbind(param_grid, baseline_grid)

for(task_id in 1:nrow(param_grid)){
  orderly_run("03_simulations",
              parameters = list(population_assumptions = param_grid$i[task_id],
                                population_year = param_grid$j[task_id],
                                vaccine_assumptions = param_grid$k[task_id],
                                region = param_grid$r[task_id],
                                param_iterations = n_iterations),
              use_draft = "newer")
}

################################################
# Combine regional simulations into one dataframe
# Calculate the mean and 95%CrI for all output measures
pop_assumptions <- c("ONS_NHS_region_principal", "ONS_NHS_region_low_migration", "ONS_NHS_region_high_migration")
pop_years <- c(2047, 2037, 2027)
vacc_assumptions <- c("baseline", "baseline_scaled_up", "baseline_scaled_up_and_reallocated")
# Create all combinations
param_grid <- expand.grid(
  i = pop_assumptions,
  j = pop_years,
  k = vacc_assumptions,
  stringsAsFactors = FALSE
)
#And add the true baseline
param_grid <- rbind(param_grid, data.frame(i = "rtm_baseline",
                                           j = 2019,
                                           k = "baseline"))

for(task_id in 1:nrow(param_grid)){
  orderly_run("04_combine_simulations",
              parameters = list(population_assumptions = param_grid$i[task_id],
                                population_year = param_grid$j[task_id],
                                vaccine_assumptions = param_grid$k[task_id],
                                param_iterations = n_iterations),
              use_draft = "newer")
  ## The parsed_simulations task is a convenience, it ports over just the processed 
  ## data from combined_simulations to avoid having to transfer all the large 
  ## original simulation objects from the HPC.
  orderly_run("06_parsed_simulations",
              parameters = list(population_assumptions = param_grid$i[task_id],
                                population_year = param_grid$j[task_id],
                                vaccine_assumptions = param_grid$k[task_id],
                                param_iterations = n_iterations),
              use_draft = "newer")
}

##################################################
# Extract and combine the vaccine information from simulations task
for(task_id in 1:nrow(param_grid)){
  orderly_run("05_combine_vaccinations",
              parameters = list(population_assumptions = param_grid$i[task_id],
                                population_year = param_grid$j[task_id],
                                vaccine_assumptions = param_grid$k[task_id],
                                param_iterations = n_iterations),
              use_draft = "newer")
}

##################################################
# Plotting
##################################################

orderly_run("Fig1_Population_Change",
            parameters = list(population_assumptions = "ONS_NHS_region_principal",
                              vaccine_assumptions = "baseline_scaled_up_and_reallocated",
                              param_iterations = n_iterations,
                              population_year = 2047),
            use_draft = "newer")

orderly_run("Fig2_Transmission_matrices",
            use_draft = "newer")

orderly_run("Fig3_Vaccine_Illustrative",
            parameters = list(population_assumptions = "ONS_NHS_region_principal",
                              param_iterations = n_iterations),
            use_draft = "newer")

orderly_run("Fig4_Health_Outcomes",
            parameters = list(vaccine_assumptions = "baseline_scaled_up_and_reallocated",
                              param_iterations = n_iterations),
            use_draft = "newer")

orderly_run("Fig5_Transmission_Differences",
            parameters = list(vaccine_assumptions = "baseline_scaled_up_and_reallocated",
                              population_year = 2047,
                              param_iterations = n_iterations),
            use_draft = "newer")

orderly_run("Fig6_Facetting",
            parameters = list(param_iterations = n_iterations),
            use_draft = "newer")
