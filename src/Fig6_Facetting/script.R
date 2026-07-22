
source("global_util.R")
## Load simulation data
########################
#Base params
######## p1 ##########
base_params_p1v1_2047 <- readRDS("dependencies/baseline_p1v1_2047.rds")
base_params_p1v1_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v1_2047, `[[`, "population"))
)
base_params_p1v1_2037 <- readRDS("dependencies/baseline_p1v1_2037.rds")
base_params_p1v1_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v1_2037, `[[`, "population"))
)
base_params_p1v1_2027 <- readRDS("dependencies/baseline_p1v1_2027.rds")
base_params_p1v1_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v1_2027, `[[`, "population"))
)
base_params_p1v2_2047 <- readRDS("dependencies/baseline_p1v2_2047.rds")
base_params_p1v2_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v2_2047, `[[`, "population"))
)
base_params_p1v2_2037 <- readRDS("dependencies/baseline_p1v2_2037.rds")
base_params_p1v2_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v2_2037, `[[`, "population"))
)
base_params_p1v2_2027 <- readRDS("dependencies/baseline_p1v2_2027.rds")
base_params_p1v2_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v2_2027, `[[`, "population"))
)
base_params_p1v3_2047 <- readRDS("dependencies/baseline_p1v3_2047.rds")
base_params_p1v3_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v3_2047, `[[`, "population"))
)
base_params_p1v3_2037 <- readRDS("dependencies/baseline_p1v3_2037.rds")
base_params_p1v3_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v3_2037, `[[`, "population"))
)
base_params_p1v3_2027 <- readRDS("dependencies/baseline_p1v3_2027.rds")
base_params_p1v3_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v3_2027, `[[`, "population"))
)
base_params_p1v4_2047 <- readRDS("dependencies/baseline_p1v4_2047.rds")
base_params_p1v4_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v4_2047, `[[`, "population"))
)
base_params_p1v4_2037 <- readRDS("dependencies/baseline_p1v4_2037.rds")
base_params_p1v4_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v4_2037, `[[`, "population"))
)
base_params_p1v4_2027 <- readRDS("dependencies/baseline_p1v4_2027.rds")
base_params_p1v4_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p1v4_2027, `[[`, "population"))
)
###### p2 ################
base_params_p2v1_2047 <- readRDS("dependencies/baseline_p2v1_2047.rds")
base_params_p2v1_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v1_2047, `[[`, "population"))
)
base_params_p2v1_2037 <- readRDS("dependencies/baseline_p2v1_2037.rds")
base_params_p2v1_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v1_2037, `[[`, "population"))
)
base_params_p2v1_2027 <- readRDS("dependencies/baseline_p2v1_2027.rds")
base_params_p2v1_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v1_2027, `[[`, "population"))
)
base_params_p2v2_2047 <- readRDS("dependencies/baseline_p2v2_2047.rds")
base_params_p2v2_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v2_2047, `[[`, "population"))
)
base_params_p2v2_2037 <- readRDS("dependencies/baseline_p2v2_2037.rds")
base_params_p2v2_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v2_2037, `[[`, "population"))
)
base_params_p2v2_2027 <- readRDS("dependencies/baseline_p2v2_2027.rds")
base_params_p2v2_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v2_2027, `[[`, "population"))
)
base_params_p2v3_2047 <- readRDS("dependencies/baseline_p2v3_2047.rds")
base_params_p2v3_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v3_2047, `[[`, "population"))
)
base_params_p2v3_2037 <- readRDS("dependencies/baseline_p2v3_2037.rds")
base_params_p2v3_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v3_2037, `[[`, "population"))
)
base_params_p2v3_2027 <- readRDS("dependencies/baseline_p2v3_2027.rds")
base_params_p2v3_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v3_2027, `[[`, "population"))
)
base_params_p2v4_2047 <- readRDS("dependencies/baseline_p2v4_2047.rds")
base_params_p2v4_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v4_2047, `[[`, "population"))
)
base_params_p2v4_2037 <- readRDS("dependencies/baseline_p2v4_2037.rds")
base_params_p2v4_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v4_2037, `[[`, "population"))
)
base_params_p2v4_2027 <- readRDS("dependencies/baseline_p2v4_2027.rds")
base_params_p2v4_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p2v4_2027, `[[`, "population"))
)
###### p3 ################
base_params_p3v1_2047 <- readRDS("dependencies/baseline_p3v1_2047.rds")
base_params_p3v1_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v1_2047, `[[`, "population"))
)
base_params_p3v1_2037 <- readRDS("dependencies/baseline_p3v1_2037.rds")
base_params_p3v1_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v1_2037, `[[`, "population"))
)
base_params_p3v1_2027 <- readRDS("dependencies/baseline_p3v1_2027.rds")
base_params_p3v1_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v1_2027, `[[`, "population"))
)
base_params_p3v2_2047 <- readRDS("dependencies/baseline_p3v2_2047.rds")
base_params_p3v2_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v2_2047, `[[`, "population"))
)
base_params_p3v2_2037 <- readRDS("dependencies/baseline_p3v2_2037.rds")
base_params_p3v2_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v2_2037, `[[`, "population"))
)
base_params_p3v2_2027 <- readRDS("dependencies/baseline_p3v2_2027.rds")
base_params_p3v2_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v2_2027, `[[`, "population"))
)
base_params_p3v3_2047 <- readRDS("dependencies/baseline_p3v3_2047.rds")
base_params_p3v3_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v3_2047, `[[`, "population"))
)
base_params_p3v3_2037 <- readRDS("dependencies/baseline_p3v3_2037.rds")
base_params_p3v3_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v3_2037, `[[`, "population"))
)
base_params_p3v3_2027 <- readRDS("dependencies/baseline_p3v3_2027.rds")
base_params_p3v3_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v3_2027, `[[`, "population"))
)
base_params_p3v4_2047 <- readRDS("dependencies/baseline_p3v4_2047.rds")
base_params_p3v4_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v4_2047, `[[`, "population"))
)
base_params_p3v4_2037 <- readRDS("dependencies/baseline_p3v4_2037.rds")
base_params_p3v4_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v4_2037, `[[`, "population"))
)
base_params_p3v4_2027 <- readRDS("dependencies/baseline_p3v4_2027.rds")
base_params_p3v4_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_p3v4_2027, `[[`, "population"))
)

##############################################################################
# Simulation output and index names
baseline_df <- readRDS("dependencies/baseline_combined_output_dataframe.rds")
baseline_index_names <- readRDS("dependencies/baseline_index_names.rds")
p1v1_2047_df <- readRDS("dependencies/p1v1_2047_output_dataframe.rds")
p1v1_2047_index_names <- readRDS("dependencies/p1v1_2047_index_names.rds")
p1v1_2037_df <- readRDS("dependencies/p1v1_2037_output_dataframe.rds")
p1v1_2037_index_names <- readRDS("dependencies/p1v1_2037_index_names.rds")
p1v1_2027_df <- readRDS("dependencies/p1v1_2027_output_dataframe.rds")
p1v1_2027_index_names <- readRDS("dependencies/p1v1_2027_index_names.rds")
p1v2_2047_df <- readRDS("dependencies/p1v2_2047_output_dataframe.rds")
p1v2_2047_index_names <- readRDS("dependencies/p1v2_2047_index_names.rds")
p1v2_2037_df <- readRDS("dependencies/p1v2_2037_output_dataframe.rds")
p1v2_2037_index_names <- readRDS("dependencies/p1v2_2037_index_names.rds")
p1v2_2027_df <- readRDS("dependencies/p1v2_2027_output_dataframe.rds")
p1v2_2027_index_names <- readRDS("dependencies/p1v2_2027_index_names.rds")
p1v3_2047_df <- readRDS("dependencies/p1v3_2047_output_dataframe.rds")
p1v3_2047_index_names <- readRDS("dependencies/p1v3_2047_index_names.rds")
p1v3_2037_df <- readRDS("dependencies/p1v3_2037_output_dataframe.rds")
p1v3_2037_index_names <- readRDS("dependencies/p1v3_2037_index_names.rds")
p1v3_2027_df <- readRDS("dependencies/p1v3_2027_output_dataframe.rds")
p1v3_2027_index_names <- readRDS("dependencies/p1v3_2027_index_names.rds")
p1v4_2047_df <- readRDS("dependencies/p1v4_2047_output_dataframe.rds")
p1v4_2047_index_names <- readRDS("dependencies/p1v4_2047_index_names.rds")
p1v4_2037_df <- readRDS("dependencies/p1v4_2037_output_dataframe.rds")
p1v4_2037_index_names <- readRDS("dependencies/p1v4_2037_index_names.rds")
p1v4_2027_df <- readRDS("dependencies/p1v4_2027_output_dataframe.rds")
p1v4_2027_index_names <- readRDS("dependencies/p1v4_2027_index_names.rds")
###########################################################################
p2v1_2047_df <- readRDS("dependencies/p2v1_2047_output_dataframe.rds")
p2v1_2047_index_names <- readRDS("dependencies/p2v1_2047_index_names.rds")
p2v1_2037_df <- readRDS("dependencies/p2v1_2037_output_dataframe.rds")
p2v1_2037_index_names <- readRDS("dependencies/p2v1_2037_index_names.rds")
p2v1_2027_df <- readRDS("dependencies/p2v1_2027_output_dataframe.rds")
p2v1_2027_index_names <- readRDS("dependencies/p2v1_2027_index_names.rds")
p2v2_2047_df <- readRDS("dependencies/p2v2_2047_output_dataframe.rds")
p2v2_2047_index_names <- readRDS("dependencies/p2v2_2047_index_names.rds")
p2v2_2037_df <- readRDS("dependencies/p2v2_2037_output_dataframe.rds")
p2v2_2037_index_names <- readRDS("dependencies/p2v2_2037_index_names.rds")
p2v2_2027_df <- readRDS("dependencies/p2v2_2027_output_dataframe.rds")
p2v2_2027_index_names <- readRDS("dependencies/p2v2_2027_index_names.rds")
p2v3_2047_df <- readRDS("dependencies/p2v3_2047_output_dataframe.rds")
p2v3_2047_index_names <- readRDS("dependencies/p2v3_2047_index_names.rds")
p2v3_2037_df <- readRDS("dependencies/p2v3_2037_output_dataframe.rds")
p2v3_2037_index_names <- readRDS("dependencies/p2v3_2037_index_names.rds")
p2v3_2027_df <- readRDS("dependencies/p2v3_2027_output_dataframe.rds")
p2v3_2027_index_names <- readRDS("dependencies/p2v3_2027_index_names.rds")
p2v4_2047_df <- readRDS("dependencies/p2v4_2047_output_dataframe.rds")
p2v4_2047_index_names <- readRDS("dependencies/p2v4_2047_index_names.rds")
p2v4_2037_df <- readRDS("dependencies/p2v4_2037_output_dataframe.rds")
p2v4_2037_index_names <- readRDS("dependencies/p2v4_2037_index_names.rds")
p2v4_2027_df <- readRDS("dependencies/p2v4_2027_output_dataframe.rds")
p2v4_2027_index_names <- readRDS("dependencies/p2v4_2027_index_names.rds")
###########################################################################
p3v1_2047_df <- readRDS("dependencies/p3v1_2047_output_dataframe.rds")
p3v1_2047_index_names <- readRDS("dependencies/p3v1_2047_index_names.rds")
p3v1_2037_df <- readRDS("dependencies/p3v1_2037_output_dataframe.rds")
p3v1_2037_index_names <- readRDS("dependencies/p3v1_2037_index_names.rds")
p3v1_2027_df <- readRDS("dependencies/p3v1_2027_output_dataframe.rds")
p3v1_2027_index_names <- readRDS("dependencies/p3v1_2027_index_names.rds")
p3v2_2047_df <- readRDS("dependencies/p3v2_2047_output_dataframe.rds")
p3v2_2047_index_names <- readRDS("dependencies/p3v2_2047_index_names.rds")
p3v2_2037_df <- readRDS("dependencies/p3v2_2037_output_dataframe.rds")
p3v2_2037_index_names <- readRDS("dependencies/p3v2_2037_index_names.rds")
p3v2_2027_df <- readRDS("dependencies/p3v2_2027_output_dataframe.rds")
p3v2_2027_index_names <- readRDS("dependencies/p3v2_2027_index_names.rds")
p3v3_2047_df <- readRDS("dependencies/p3v3_2047_output_dataframe.rds")
p3v3_2047_index_names <- readRDS("dependencies/p3v3_2047_index_names.rds")
p3v3_2037_df <- readRDS("dependencies/p3v3_2037_output_dataframe.rds")
p3v3_2037_index_names <- readRDS("dependencies/p3v3_2037_index_names.rds")
p3v3_2027_df <- readRDS("dependencies/p3v3_2027_output_dataframe.rds")
p3v3_2027_index_names <- readRDS("dependencies/p3v3_2027_index_names.rds")
p3v4_2047_df <- readRDS("dependencies/p3v4_2047_output_dataframe.rds")
p3v4_2047_index_names <- readRDS("dependencies/p3v4_2047_index_names.rds")
p3v4_2037_df <- readRDS("dependencies/p3v4_2037_output_dataframe.rds")
p3v4_2037_index_names <- readRDS("dependencies/p3v4_2037_index_names.rds")
p3v4_2027_df <- readRDS("dependencies/p3v4_2027_output_dataframe.rds")
p3v4_2027_index_names <- readRDS("dependencies/p3v4_2027_index_names.rds")
###########################################################################

#Check all the index names align
if (!(identical(baseline_index_names, p1v1_2047_index_names) && identical(baseline_index_names, p1v1_2037_index_names) && identical(baseline_index_names, p1v1_2027_index_names))) {
  stop("Index names are not identical")
}

#Add population to the simulation output dataframes
# Make 12 versions of baseline to appear across all facets
p1v1_baseline_df <- baseline_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_principal") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p1v2_baseline_df <- baseline_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_principal") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p1v3_baseline_df <- baseline_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_principal") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p1v4_baseline_df <- baseline_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_principal") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p2v1_baseline_df <- baseline_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_low_migration") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p2v2_baseline_df <- baseline_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_low_migration") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p2v3_baseline_df <- baseline_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_low_migration") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p2v4_baseline_df <- baseline_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_low_migration") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p3v1_baseline_df <- baseline_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_high_migration") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p3v2_baseline_df <- baseline_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_high_migration") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p3v3_baseline_df <- baseline_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_high_migration") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
p3v4_baseline_df <- baseline_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_high_migration") %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
baseline_df <- rbind(p1v1_baseline_df, p1v2_baseline_df, p1v3_baseline_df, p1v4_baseline_df,
                     p2v1_baseline_df, p2v2_baseline_df, p2v3_baseline_df, p2v4_baseline_df,
                     p3v1_baseline_df, p3v2_baseline_df, p3v3_baseline_df, p3v4_baseline_df)
rm(p1v1_baseline_df, p1v2_baseline_df, p1v3_baseline_df, p1v4_baseline_df,
   p2v1_baseline_df, p2v2_baseline_df, p2v3_baseline_df, p2v4_baseline_df,
   p3v1_baseline_df, p3v2_baseline_df, p3v3_baseline_df, p3v4_baseline_df)
baseline_df$version <- "2019"
####################################################################
p1v1_2047_df <- p1v1_2047_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v1_2047[[region]]$population)) %>%
  ungroup()
p1v1_2037_df <- p1v1_2037_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v1_2037[[region]]$population)) %>%
  ungroup()
p1v1_2027_df <- p1v1_2027_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v1_2027[[region]]$population)) %>%
  ungroup()

p1v2_2047_df <- p1v2_2047_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v2_2047[[region]]$population)) %>%
  ungroup()
p1v2_2037_df <- p1v2_2037_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v2_2037[[region]]$population)) %>%
  ungroup()
p1v2_2027_df <- p1v2_2027_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v2_2027[[region]]$population)) %>%
  ungroup()

p1v3_2047_df <- p1v3_2047_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v3_2047[[region]]$population)) %>%
  ungroup()
p1v3_2037_df <- p1v3_2037_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v3_2037[[region]]$population)) %>%
  ungroup()
p1v3_2027_df <- p1v3_2027_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v3_2027[[region]]$population)) %>%
  ungroup()

p1v4_2047_df <- p1v4_2047_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v4_2047[[region]]$population)) %>%
  ungroup()
p1v4_2037_df <- p1v4_2037_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v4_2037[[region]]$population)) %>%
  ungroup()
p1v4_2027_df <- p1v4_2027_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_principal",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p1v4_2027[[region]]$population)) %>%
  ungroup()
###############################################################################
p2v1_2047_df <- p2v1_2047_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v1_2047[[region]]$population)) %>%
  ungroup()
p2v1_2037_df <- p2v1_2037_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v1_2037[[region]]$population)) %>%
  ungroup()
p2v1_2027_df <- p2v1_2027_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v1_2027[[region]]$population)) %>%
  ungroup()

p2v2_2047_df <- p2v2_2047_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v2_2047[[region]]$population)) %>%
  ungroup()
p2v2_2037_df <- p2v2_2037_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v2_2037[[region]]$population)) %>%
  ungroup()
p2v2_2027_df <- p2v2_2027_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v2_2027[[region]]$population)) %>%
  ungroup()

p2v3_2047_df <- p2v3_2047_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v3_2047[[region]]$population)) %>%
  ungroup()
p2v3_2037_df <- p2v3_2037_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v3_2037[[region]]$population)) %>%
  ungroup()
p2v3_2027_df <- p2v3_2027_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v3_2027[[region]]$population)) %>%
  ungroup()

p2v4_2047_df <- p2v4_2047_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v4_2047[[region]]$population)) %>%
  ungroup()
p2v4_2037_df <- p2v4_2037_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v4_2037[[region]]$population)) %>%
  ungroup()
p2v4_2027_df <- p2v4_2027_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_low_migration",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p2v4_2027[[region]]$population)) %>%
  ungroup()
###############################################################################
p3v1_2047_df <- p3v1_2047_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v1_2047[[region]]$population)) %>%
  ungroup()
p3v1_2037_df <- p3v1_2037_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v1_2037[[region]]$population)) %>%
  ungroup()
p3v1_2027_df <- p3v1_2027_df %>%
  mutate(vaccine = "baseline",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v1_2027[[region]]$population)) %>%
  ungroup()

p3v2_2047_df <- p3v2_2047_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v2_2047[[region]]$population)) %>%
  ungroup()
p3v2_2037_df <- p3v2_2037_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v2_2037[[region]]$population)) %>%
  ungroup()
p3v2_2027_df <- p3v2_2027_df %>%
  mutate(vaccine = "baseline_scaled_up",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v2_2027[[region]]$population)) %>%
  ungroup()

p3v3_2047_df <- p3v3_2047_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v3_2047[[region]]$population)) %>%
  ungroup()
p3v3_2037_df <- p3v3_2037_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v3_2037[[region]]$population)) %>%
  ungroup()
p3v3_2027_df <- p3v3_2027_df %>%
  mutate(vaccine = "baseline_scaled_up_and_reallocated",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v3_2027[[region]]$population)) %>%
  ungroup()

p3v4_2047_df <- p3v4_2047_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2047") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v4_2047[[region]]$population)) %>%
  ungroup()
p3v4_2037_df <- p3v4_2037_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2037") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v4_2037[[region]]$population)) %>%
  ungroup()
p3v4_2027_df <- p3v4_2027_df %>%
  mutate(vaccine = "1_month_earlier",
         population_assumptions = "ONS_NHS_region_high_migration",
         version = "2027") %>%
  rowwise() %>%
  mutate(population = sum(base_params_p3v4_2027[[region]]$population)) %>%
  ungroup()

###############################################################################
###############################################################################

# Stick all simulation data together:
simulation_data <- rbind(baseline_df,
                         p1v1_2047_df, p1v1_2037_df, p1v1_2027_df,
                         p1v2_2047_df, p1v2_2037_df, p1v2_2027_df,
                         p1v3_2047_df, p1v3_2037_df, p1v3_2027_df,
                         p1v4_2047_df, p1v4_2037_df, p1v4_2027_df,
                         p2v1_2047_df, p2v1_2037_df, p2v1_2027_df,
                         p2v2_2047_df, p2v2_2037_df, p2v2_2027_df,
                         p2v3_2047_df, p2v3_2037_df, p2v3_2027_df,
                         p2v4_2047_df, p2v4_2037_df, p2v4_2027_df,
                         p3v1_2047_df, p3v1_2037_df, p3v1_2027_df,
                         p3v2_2047_df, p3v2_2037_df, p3v2_2027_df,
                         p3v3_2047_df, p3v3_2037_df, p3v3_2027_df,
                         p3v4_2047_df, p3v4_2037_df, p3v4_2027_df
                         )

#Remove all the stuff we don't need anymore:
rm(baseline_df,
   p1v1_2047_df, p1v1_2037_df, p1v1_2027_df,
   p1v2_2047_df, p1v2_2037_df, p1v2_2027_df,
   p1v3_2047_df, p1v3_2037_df, p1v3_2027_df,
   p1v4_2047_df, p1v4_2037_df, p1v4_2027_df,
   p2v1_2047_df, p2v1_2037_df, p2v1_2027_df,
   p2v2_2047_df, p2v2_2037_df, p2v2_2027_df,
   p2v3_2047_df, p2v3_2037_df, p2v3_2027_df,
   p2v4_2047_df, p2v4_2037_df, p2v4_2027_df,
   p3v1_2047_df, p3v1_2037_df, p3v1_2027_df,
   p3v2_2047_df, p3v2_2037_df, p3v2_2027_df,
   p3v3_2047_df, p3v3_2037_df, p3v3_2027_df,
   p3v4_2047_df, p3v4_2037_df, p3v4_2027_df)

rm(baseline_index_names,
   p1v1_2027_index_names, p1v1_2037_index_names, p1v1_2047_index_names,
   p1v2_2027_index_names, p1v2_2037_index_names, p1v2_2047_index_names,
   p1v3_2027_index_names, p1v3_2037_index_names, p1v3_2047_index_names,
   p1v4_2027_index_names, p1v4_2037_index_names, p1v4_2047_index_names,
   p2v1_2027_index_names, p2v1_2037_index_names, p2v1_2047_index_names,
   p2v2_2027_index_names, p2v2_2037_index_names, p2v2_2047_index_names,
   p2v3_2027_index_names, p2v3_2037_index_names, p2v3_2047_index_names,
   p2v4_2027_index_names, p2v4_2037_index_names, p2v4_2047_index_names,
   p3v1_2027_index_names, p3v1_2037_index_names, p3v1_2047_index_names,
   p3v2_2027_index_names, p3v2_2037_index_names, p3v2_2047_index_names,
   p3v3_2027_index_names, p3v3_2037_index_names, p3v3_2047_index_names,
   p3v4_2027_index_names, p3v4_2037_index_names, p3v4_2047_index_names)

rm(base_params_p1v1_2027, base_params_p1v1_2037, base_params_p1v1_2047,
   base_params_p1v2_2027, base_params_p1v2_2037, base_params_p1v2_2047,
   base_params_p1v3_2027, base_params_p1v3_2037, base_params_p1v3_2047,
   base_params_p1v4_2027, base_params_p1v4_2037, base_params_p1v4_2047,
   base_params_p2v1_2027, base_params_p2v1_2037, base_params_p2v1_2047,
   base_params_p2v2_2027, base_params_p2v2_2037, base_params_p2v2_2047,
   base_params_p2v3_2027, base_params_p2v3_2037, base_params_p2v3_2047,
   base_params_p2v4_2027, base_params_p2v4_2037, base_params_p2v4_2047,
   base_params_p3v1_2027, base_params_p3v1_2037, base_params_p3v1_2047,
   base_params_p3v2_2027, base_params_p3v2_2037, base_params_p3v2_2047,
   base_params_p3v3_2027, base_params_p3v3_2037, base_params_p3v3_2047,
   base_params_p3v4_2027, base_params_p3v4_2037, base_params_p3v4_2047)
#########################################################################
#Define general plotting functions
# Lancet colours for reference:
#"#00468BFF" "#ED0000FF" "#42B540FF" "#0099B4FF" "#925E9FFF" "#FDAF91FF" "#AD002AFF" "#ADB6B6FF"
source("plotting_functions.R")
####################################################################
# Prep folders
dir.create("Figures")
dir.create("Figures/individual_panels")
# Save the data we make plots with
dir.create("Figure_dataframes")
saveRDS(simulation_data, "Figure_dataframes/simulation_data.rds")
####################################################################
############################################################################
#Cumulative hospitalisations by scenario
figure_data <- simulation_data %>%
  filter(output_type == "total_hospitalisations")
saveRDS(simulation_data, "Figure_dataframes/figure_data.rds")

f4 <- plot_cumulative_facet(figure_data, "Hospitalisations \n(thousands)", "england", -150)
f4 <- f4 + ggtitle("Cumulative Daily Confirmed Hospitalisations by Vaccination and Migration Assumptions")
ggsave(
  filename = "Fig6.png",
  plot = f4,
  width = 13, height = 10.4, dpi = 320        
)
ggsave(
  filename = "Fig6.pdf",
  plot = f4,
  width = 13, height = 10.4, dpi = 320        
)

f4 <- plot_cumulative_facet(filter(figure_data,
                                   vaccine != "1_month_earlier"), 
                            "Hospitalisations \n(thousands)", "england", -150)
f4 <- f4 + ggtitle("Cumulative Daily Confirmed Hospitalisations by Vaccination and Migration Assumptions")
ggsave(
  filename = "Fig6_3row.png",
  plot = f4,
  width = 13, height = 7.8, dpi = 320        
)
ggsave(
  filename = "Fig6_3row.pdf",
  plot = f4,
  width = 13, height = 7.8, dpi = 320        
)

# f4_per_capita <- plot_cumulative_facet_per_capita(figure_data, "Hospitalisations", "england")
# p2_per_capita <- p2_per_capita + ggtitle("Cumulative Daily Confirmed Hospitalisations Per Capita")
# ggsave(
#   filename = "Figures/individual_panels/Fig2_p2_per_capita.png",
#   plot = p2_per_capita,
#   width = 10, height = 6, dpi = 320        
# )
