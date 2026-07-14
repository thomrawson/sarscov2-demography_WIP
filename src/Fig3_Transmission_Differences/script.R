
source("global_util.R")
source("plotting_functions.R")
## Load simulation data
########################
simulation_data <- readRDS("simulation_data.rds")
# Because time is a double we need to just make sure they're all within machine-precision:
simulation_data$time <- round(simulation_data$time)
dir.create("Figures")
dir.create("Figures/individual_panels")
########################
baseline_principal <- readRDS("dependencies/baseline_central.rds")
baseline_principal$england <- list(
  population = Reduce(`+`, lapply(baseline_principal, `[[`, "population"))
)
baseline_low_migration <- readRDS("dependencies/baseline_low_migration.rds")
baseline_low_migration$england <- list(
  population = Reduce(`+`, lapply(baseline_low_migration, `[[`, "population"))
)
baseline_high_migration <- readRDS("dependencies/baseline_high_migration.rds")
baseline_high_migration$england <- list(
  population = Reduce(`+`, lapply(baseline_high_migration, `[[`, "population"))
)
##################################################################
#Need to redefine the population for certain output types
regions <- c("england", sircovid:::regions("england"))
outputs <- c("sympt_cases_under15_inc", "sympt_cases_15_24_inc",
             "sympt_cases_25_49_inc", "sympt_cases_50_64_inc",
             "sympt_cases_65_79_inc", "sympt_cases_80_plus_inc")
versions <- c("Factual", "Counterfactual_2047", "Counterfactual_2037", 
              "Counterfactual_2027")

lookup <- expand.grid(
  region = regions,
  output_type = outputs,
  version = versions,
  stringsAsFactors = FALSE
)
lookup$new_population <- NA
for(i in 1:nrow(lookup)){
  if(lookup$version[i] == "Factual"){
    if(lookup$output_type[i] == "sympt_cases_under15_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[1:3])
    } else if(lookup$output_type[i] == "sympt_cases_15_24_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[4:5])
    } else if(lookup$output_type[i] == "sympt_cases_25_49_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[6:10])
    } else if(lookup$output_type[i] == "sympt_cases_50_64_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[11:13])
    } else if(lookup$output_type[i] == "sympt_cases_65_79_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[14:16])
    } else if(lookup$output_type[i] == "sympt_cases_80_plus_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[17])
    }
  } else if(lookup$version[i] == "Counterfactual_2047"){
    if(lookup$output_type[i] == "sympt_cases_under15_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[1:3])
    } else if(lookup$output_type[i] == "sympt_cases_15_24_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[4:5])
    } else if(lookup$output_type[i] == "sympt_cases_25_49_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[6:10])
    } else if(lookup$output_type[i] == "sympt_cases_50_64_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[11:13])
    } else if(lookup$output_type[i] == "sympt_cases_65_79_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[14:16])
    } else if(lookup$output_type[i] == "sympt_cases_80_plus_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[17])
    }
  } else if(lookup$version[i] == "Counterfactual_low_migration"){
    if(lookup$output_type[i] == "sympt_cases_under15_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[1:3])
    } else if(lookup$output_type[i] == "sympt_cases_15_24_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[4:5])
    } else if(lookup$output_type[i] == "sympt_cases_25_49_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[6:10])
    } else if(lookup$output_type[i] == "sympt_cases_50_64_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[11:13])
    } else if(lookup$output_type[i] == "sympt_cases_65_79_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[14:16])
    } else if(lookup$output_type[i] == "sympt_cases_80_plus_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[17])
    }
  } else if(lookup$version[i] == "Counterfactual_high_migration"){
    if(lookup$output_type[i] == "sympt_cases_under15_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[1:3])
    } else if(lookup$output_type[i] == "sympt_cases_15_24_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[4:5])
    } else if(lookup$output_type[i] == "sympt_cases_25_49_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[6:10])
    } else if(lookup$output_type[i] == "sympt_cases_50_64_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[11:13])
    } else if(lookup$output_type[i] == "sympt_cases_65_79_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[14:16])
    } else if(lookup$output_type[i] == "sympt_cases_80_plus_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[17])
    }
  }

}

#Now use the lookup to overwrite the simulation_data population entries:
simulation_data <- simulation_data %>%
  left_join(lookup, by = c("region", "output_type", "version")) %>%
  mutate(population = ifelse(!is.na(new_population), new_population, population)) %>%
  select(-new_population)

##################################
#Repeat but for deaths
outputs <- c("deaths_hosp_0_49_inc","deaths_hosp_50_54_inc",           
"deaths_hosp_55_59_inc","deaths_hosp_60_64_inc","deaths_hosp_65_69_inc",          
"deaths_hosp_70_74_inc","deaths_hosp_75_79_inc","deaths_hosp_80_plus_inc")

lookup <- expand.grid(
  region = regions,
  output_type = outputs,
  version = versions,
  stringsAsFactors = FALSE
)
lookup$new_population <- NA
for(i in 1:nrow(lookup)){
  if(lookup$version[i] == "Factual"){
    if(lookup$output_type[i] == "deaths_hosp_0_49_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[1:10])
    } else if(lookup$output_type[i] == "deaths_hosp_50_54_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[11])
    } else if(lookup$output_type[i] == "deaths_hosp_55_59_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[12])
    } else if(lookup$output_type[i] == "deaths_hosp_60_64_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[13])
    } else if(lookup$output_type[i] == "deaths_hosp_65_69_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[14])
    } else if(lookup$output_type[i] == "deaths_hosp_70_74_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[15])
    } else if(lookup$output_type[i] == "deaths_hosp_75_79_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[16])
    } else if(lookup$output_type[i] == "deaths_hosp_80_plus_inc"){
      lookup$new_population[i] <- sum(sircovid:::sircovid_population(lookup$region[i])[17])
    }
  } else if(lookup$version[i] == "Counterfactual_2047"){
    if(lookup$output_type[i] == "deaths_hosp_0_49_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[1:10])
    } else if(lookup$output_type[i] == "deaths_hosp_50_54_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[11])
    } else if(lookup$output_type[i] == "deaths_hosp_55_59_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[12])
    } else if(lookup$output_type[i] == "deaths_hosp_60_64_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[13])
    } else if(lookup$output_type[i] == "deaths_hosp_65_69_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[14])
    } else if(lookup$output_type[i] == "deaths_hosp_70_74_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[15])
    } else if(lookup$output_type[i] == "deaths_hosp_75_79_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[16])
    } else if(lookup$output_type[i] == "deaths_hosp_80_plus_inc"){
      lookup$new_population[i] <- sum(baseline_principal[[lookup$region[i]]]$population[17])
    } 
  } else if(lookup$version[i] == "Counterfactual_low_migration"){
    if(lookup$output_type[i] == "deaths_hosp_0_49_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[1:10])
    } else if(lookup$output_type[i] == "deaths_hosp_50_54_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[11])
    } else if(lookup$output_type[i] == "deaths_hosp_55_59_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[12])
    } else if(lookup$output_type[i] == "deaths_hosp_60_64_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[13])
    } else if(lookup$output_type[i] == "deaths_hosp_65_69_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[14])
    } else if(lookup$output_type[i] == "deaths_hosp_70_74_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[15])
    } else if(lookup$output_type[i] == "deaths_hosp_75_79_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[16])
    } else if(lookup$output_type[i] == "deaths_hosp_80_plus_inc"){
      lookup$new_population[i] <- sum(baseline_low_migration[[lookup$region[i]]]$population[17])
    }
  } else if(lookup$version[i] == "Counterfactual_high_migration"){
    if(lookup$output_type[i] == "deaths_hosp_0_49_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[1:10])
    } else if(lookup$output_type[i] == "deaths_hosp_50_54_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[11])
    } else if(lookup$output_type[i] == "deaths_hosp_55_59_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[12])
    } else if(lookup$output_type[i] == "deaths_hosp_60_64_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[13])
    } else if(lookup$output_type[i] == "deaths_hosp_65_69_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[14])
    } else if(lookup$output_type[i] == "deaths_hosp_70_74_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[15])
    } else if(lookup$output_type[i] == "deaths_hosp_75_79_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[16])
    } else if(lookup$output_type[i] == "deaths_hosp_80_plus_inc"){
      lookup$new_population[i] <- sum(baseline_high_migration[[lookup$region[i]]]$population[17])
    }
  }
  
}
#Now use the lookup to overwrite the simulation_data population entries:
simulation_data <- simulation_data %>%
  left_join(lookup, by = c("region", "output_type", "version")) %>%
  mutate(population = ifelse(!is.na(new_population), new_population, population)) %>%
  select(-new_population)
################################################
#Define some new entries for aggregated ages
#0-49, 50-64, 65-79, 80+
new_rows <- simulation_data %>%
  filter(output_type %in% c(
    "sympt_cases_under15_inc",
    "sympt_cases_15_24_inc",
    "sympt_cases_25_49_inc"
  )) %>%
  group_by(region, time, version) %>%
  summarise(
    output_type = "sympt_cases_0_49_inc",
    mean = sum(mean, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    cri_lower = NA,
    cri_upper = NA,
    .groups = "drop"
  )
simulation_data <- bind_rows(simulation_data, new_rows)
###
#And now for deaths
outputs <- c("deaths_hosp_0_49_inc","deaths_hosp_50_54_inc",           
             "deaths_hosp_55_59_inc","deaths_hosp_60_64_inc","deaths_hosp_65_69_inc",          
             "deaths_hosp_70_74_inc","deaths_hosp_75_79_inc","deaths_hosp_80_plus_inc")
#50 -64
new_rows <- simulation_data %>%
  filter(output_type %in% c(
    "deaths_hosp_50_54_inc",
    "deaths_hosp_55_59_inc",
    "deaths_hosp_60_64_inc"
  )) %>%
  group_by(region, time, version) %>%
  summarise(
    output_type = "deaths_hosp_50_64_inc",
    mean = sum(mean, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    cri_lower = NA,
    cri_upper = NA,
    .groups = "drop"
  )
simulation_data <- bind_rows(simulation_data, new_rows)
#65 - 79
new_rows <- simulation_data %>%
  filter(output_type %in% c(
    "deaths_hosp_65_69_inc",
    "deaths_hosp_70_74_inc",
    "deaths_hosp_75_79_inc"
  )) %>%
  group_by(region, time, version) %>%
  summarise(
    output_type = "deaths_hosp_65_79_inc",
    mean = sum(mean, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    cri_lower = NA,
    cri_upper = NA,
    .groups = "drop"
  )
simulation_data <- bind_rows(simulation_data, new_rows)

########################
# Panel 2 - eff_Rt
p1_sim_data <- simulation_data %>%
  filter(output_type == "eff_Rt_all") %>%
  filter(version %in% c("Factual", "Counterfactual_2047"))
p1_real_data <- data.frame(time = NA, region = NA, population = NA, value = NA)
p2 <- plot_time_series(p1_sim_data, p1_real_data,
                       "Effective Rt", "england")
p2 <- p2 + ggtitle("Effective Reproduction Number") + guides(colour = guide_legend(override.aes = list(shape = NA)))
ggsave(
  filename = "Figures/individual_panels/Fig3_p2.png",
  plot = p2,
  width = 10, height = 6, dpi = 320 
)

#Panel 2b - Rt
p1b_sim_data <- simulation_data %>%
  filter(output_type == "Rt_all") %>%
  filter(version %in% c("Factual", "Counterfactual_2047"))
p1b_real_data <- data.frame(time = NA, region = NA, population = NA, value = NA)
p2b <- plot_time_series(p1b_sim_data, p1b_real_data,
                       "Rt", "england")
p2b <- p2b + ggtitle("Reproduction Number") + guides(colour = guide_legend(override.aes = list(shape = NA)))
ggsave(
  filename = "Figures/individual_panels/Fig3_p2b.png",
  plot = p2b,
  width = 10, height = 6, dpi = 320 
)
########################
# Panel 1 - Cases
p1_sim_data <- simulation_data %>%
  filter(output_type == "sympt_cases_inc") %>%
  filter(version %in% c("Factual", "Counterfactual_2047"))
p1_real_data <- data.frame(time = NA, region = NA, population = NA, value = NA)
p1 <- plot_time_series(p1_sim_data, p1_real_data,
                       "Symptomatic Cases", "england")
p1 <- p1 + ggtitle("Pillar 2 Symptomatic Cases") + guides(colour = guide_legend(override.aes = list(shape = NA)))
ggsave(
  filename = "Figures/individual_panels/Fig3_p1.png",
  plot = p1,
  width = 10, height = 6, dpi = 320 
)

######################################
# Supplementary 1 - Symptomatic Cases over time, but split up by age, 2019 Factual
#"#00468BFF" "#ED0000FF" "#42B540FF" "#0099B4FF" "#925E9FFF" "#FDAF91FF" "#AD002AFF" "#ADB6B6FF"
s1 <- plot_sympt_cases_age_stack(simulation_data, "england", "Factual")
s1 <- s1 + ggtitle("Pillar 2 Symptomatic Cases by Age (2020 Baseline)")

######################################
# Supplementary 2 - Symptomatic Cases over time, but split up by age, 2047 Counterfactual
s2 <- plot_sympt_cases_age_stack(simulation_data, "england", "Counterfactual_2047")
s2 <- s2 + ggtitle("Pillar 2 Symptomatic Cases by Age (2047 Central Projection)")

##Define a function to extract y-axis range, so we can force them to the same y-axis range
get_y_range <- function(p) {
  pp <- ggplot_build(p)$layout$panel_params[[1]]
  
  # Works across recent ggplot2 versions
  if (!is.null(pp$y.range)) {
    pp$y.range
  } else if (!is.null(pp$y$range$range)) {
    pp$y$range$range
  } else {
    stop("Couldn't find y-axis range in this ggplot object.")
  }
}

y1 <- get_y_range(s1)
y2 <- get_y_range(s2)
upper_max <- max(y1[2], y2[2])
lower_min <- min(y1[1], y2[1]) 
s1 <- s1 + coord_cartesian(ylim = c(lower_min, upper_max))
s2 <- s2 + coord_cartesian(ylim = c(lower_min, upper_max))
ggsave(
  filename = "Figures/individual_panels/Fig3_s1.png",
  plot = s1,
  width = 10, height = 6, dpi = 320 
)
ggsave(
  filename = "Figures/individual_panels/Fig3_s2.png",
  plot = s2,
  width = 10, height = 6, dpi = 320 
)

#####################################
# S3 - Do the same plot as above, but make it be max proportion split by ages
s3 <- plot_sympt_cases_age_proportion(
  sim_df = simulation_data,
  region_selected = "england",
  version_selected = "Factual"
) + ggtitle("Proportion of Symptomatic Cases by Age (2020 Baseline)")
ggsave(
  filename = "Figures/individual_panels/Fig3_s3.png",
  plot = s3,
  width = 10, height = 6, dpi = 320
)

#####################################
# S4 - Counterfactual
s4 <- plot_sympt_cases_age_proportion(
  sim_df = simulation_data,
  region_selected = "england",
  version_selected = "Counterfactual_2047"
) + ggtitle("Proportion of Symptomatic Cases by Age (2047 Central Projection)")
ggsave(
  filename = "Figures/individual_panels/Fig3_s4.png",
  plot = s4,
  width = 10, height = 6, dpi = 320
)

#####################################
# S4 - The ratio difference plot
simulation_data <- simulation_data %>%
  left_join(lookup, by = c("region", "output_type", "version")) %>%
  mutate(population = ifelse(!is.na(new_population), new_population, population)) %>%
  select(-new_population)

ratio_sim_data <- simulation_data |>
  dplyr::left_join(
    simulation_data |>
      dplyr::filter(version == "Factual") |>
      dplyr::distinct(region, time, output_type, factual_population = population),
    by = c("region", "time", "output_type")
  ) |>
  dplyr::mutate(
    population_ratio_vs_factual = population / factual_population
  )
ratio_sim_data <- ratio_sim_data |>
  dplyr::left_join(
    ratio_sim_data |>
      dplyr::filter(version == "Factual") |>
      dplyr::distinct(region, time, output_type, factual_mean = mean),
    by = c("region", "time", "output_type")
  ) |>
  dplyr::mutate(
    mean_ratio_vs_factual = mean / factual_mean
  )

s4_sim_data <- ratio_sim_data |>
  filter(version == "Counterfactual_2047") |>
  filter(output_type %in% c(#"sympt_cases_under15_inc", "sympt_cases_15_24_inc",
                            "sympt_cases_0_49_inc", "sympt_cases_50_64_inc",
                            "sympt_cases_65_79_inc", "sympt_cases_80_plus_inc" ))


s4 <- plot_population_adjusted_case_ratio(
  sim_df = s4_sim_data,
  region_selected = "england"
) + ggtitle("Population-adjusted Daily Symptomatic Cases Ratio (2020 Baseline vs 2047 Central Projection)")
ggsave(
  filename = "Figures/individual_panels/Fig3_s4.png",
  plot = s4,
  width = 10, height = 6, dpi = 320
)

#######################
# S5, same as above but for cumulative cases
s5_cum_sim_data <- s4_sim_data |>
  dplyr::arrange(region, output_type, version, time) |>
  dplyr::group_by(region, output_type, version) |>
  dplyr::mutate(
    cum_mean = cumsum(mean),
    cum_factual_mean = cumsum(factual_mean),
    cum_mean_ratio_vs_factual = dplyr::if_else(
      cum_factual_mean == 0,
      NA_real_,
      cum_mean / cum_factual_mean
    ),
    cum_pop_adjusted_case_ratio = cum_mean_ratio_vs_factual / population_ratio_vs_factual
  ) |>
  dplyr::ungroup()

s5 <- plot_population_adjusted_cumulative_case_ratio(
  sim_df = s5_cum_sim_data,
  region_selected = "england"
) + ggtitle("Population-adjusted Cumulative Symptomatic Cases Ratio \n(2020 Baseline vs 2047 Central Projection)")
ggsave(
  filename = "Figures/individual_panels/Fig3_s5.png",
  plot = s5,
  width = 10, height = 6, dpi = 320
)

##############################################
#P3 - Do the above but for a rolling cumulative, rather than a total cumulative.
rolling_sum <- function(x, k) {
  as.numeric(stats::filter(x, rep(1, k), sides = 1))
}
make_rolling_sim_data_dplyr <- function(sim_df, window_days = 90) {
  
  sim_df |>
    dplyr::arrange(region, output_type, version, time) |>
    dplyr::group_by(region, output_type, version) |>
    dplyr::mutate(
      roll_mean = rolling_sum(mean, window_days),
      roll_factual_mean = rolling_sum(factual_mean, window_days),
      roll_mean_ratio_vs_factual = dplyr::if_else(
        roll_factual_mean == 0,
        NA_real_,
        roll_mean / roll_factual_mean
      ),
      roll_pop_adjusted_case_ratio =
        roll_mean_ratio_vs_factual / population_ratio_vs_factual
    ) |>
    dplyr::ungroup()
}

p3_roll_sim_data <- make_rolling_sim_data_dplyr(s4_sim_data, window_days = 180)
p3 <- plot_population_adjusted_rolling_cumulative_case_ratio(
  sim_df = p3_roll_sim_data,
  region_selected = "england"
) + ggtitle("Population-adjusted 6-month Rolling Cumulative \nSymptomatic Cases Ratio \n(2020 Baseline vs 2047 Central Projection)")
ggsave(
  filename = "Figures/individual_panels/Fig3_p3.png",
  plot = p3,
  width = 10, height = 6, dpi = 320
)

#########################################
#Repeat p3 but for hospitalisations instead of cases.
p4_sim_data <- ratio_sim_data |>
  filter(version == "Counterfactual_2047") |>
  filter(output_type %in% c("all_admission_0_9_inc","all_admission_10_19_inc",           
                            "all_admission_20_29_inc","all_admission_30_39_inc","all_admission_40_49_inc",          
                            "all_admission_50_59_inc","all_admission_60_69_inc","all_admission_70_79_inc",           
                            "all_admission_80_plus_inc"))
p4_roll_sim_data <- make_rolling_sim_data_dplyr(p4_sim_data, window_days = 180)
p4 <- plot_population_adjusted_rolling_cumulative_case_ratio_hosps(
  sim_df = p4_roll_sim_data,
  region_selected = "england"
) + ggtitle("Population-adjusted 6-month Rolling Cumulative \nHospitalisations Ratio \n(2020 Baseline vs 2047 Central Projection)")
ggsave(
  filename = "Figures/individual_panels/Fig3_p3b.png",
  plot = p4,
  width = 10, height = 6, dpi = 320
)

#########################################
#Repeat p3 but for deaths instead of cases.
p4_sim_data <- ratio_sim_data |>
  filter(version == "Counterfactual_2047") |>
  filter(output_type %in% c("deaths_hosp_0_49_inc","deaths_hosp_50_64_inc",           
                            "deaths_hosp_65_79_inc", "deaths_hosp_80_plus_inc"))
p4_roll_sim_data <- make_rolling_sim_data_dplyr(p4_sim_data, window_days = 180)
p4 <- plot_population_adjusted_rolling_cumulative_case_ratio_deaths(
  sim_df = p4_roll_sim_data,
  region_selected = "england"
) + ggtitle("Population-adjusted 6-month Rolling \nCumulative Deaths Ratio \n(2020 Baseline vs 2047 Central Projection)")
ggsave(
  filename = "Figures/individual_panels/Fig3_p4.png",
  plot = p4,
  width = 10, height = 6, dpi = 320
)

y1 <- get_y_range(p3)
y2 <- get_y_range(p4)
upper_max <- max(y1[2], y2[2])
lower_min <- min(y1[1], y2[1]) 
p3 <- p3 + coord_cartesian(ylim = c(lower_min, upper_max))
p4 <- p4 + coord_cartesian(ylim = c(lower_min, upper_max))

#############################################################
# P5 - Vaccine proportion uptake in each of the 4 age groups on June 1st
## Load data:
base_params <- readRDS("vaccine_dependencies/base.rds")
base_params$england <- list(
  population = Reduce(`+`, lapply(base_params, `[[`, "population"))
)
baseline_df <- readRDS("vaccine_dependencies/baseline_vaccine_simulations.rds")
counterfactual_df <- readRDS("vaccine_dependencies/vaccine_simulations.rds")
baseline_vacc_schedules <- readRDS("vaccine_dependencies/baseline_vaccine_schedules.rds")
counterfactual_vacc_schedules <- readRDS("vaccine_dependencies/vaccine_schedules.rds")

baseline_df$version <- "Factual"
counterfactual_df$version <- "Counterfactual"
vaccine_df <- rbind(baseline_df, counterfactual_df)
regions <- unique(vaccine_df$region)
vaccination_strata <- unique(vaccine_df$vaccine_strata)

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64",
  "65-69", "70-74", "75-79", "80+", "CHR", "CHW")

# Set age levels:
vaccine_df <- mutate(vaccine_df, age_group = factor(age_group, levels = age_levels))

## Next, we want plots of bar graphs over time, 
## showing the proportion of the population at each step in each vaccine class
## Each bar will represent the first of the respective month.

## First, we have to convert the dataframe to instead be capturing the number in that vaccine class currently
## That means subtracting the # in the onward class, from the # in the current class
# Let's cut the credible intervals for this
vaccine_current_df <- select(vaccine_df, region, time, age_group, population, vaccine_strata, mean_cum_doses, version)

vaccine_current_df <- vaccine_current_df %>%
  mutate(vaccine_strata = factor(vaccine_strata, levels = vaccination_strata)) %>%
  arrange(region, time, age_group, version, vaccine_strata) %>%
  group_by(region, time, age_group, version) %>%
  mutate(
    mean_in_stage = mean_cum_doses - lead(mean_cum_doses),    # current minus next cumulative
    mean_in_stage = if_else(is.na(mean_in_stage), mean_cum_doses, mean_in_stage), # last strata keeps its cum count
    mean_in_stage = pmax(mean_in_stage, 0)  # protect against tiny negative numbers from rounding
  ) %>%
  ungroup()

times_to_keep <- c(#"2020-12-01", 
  "2021-06-01")

vaccine_current_df_reduced <- filter(vaccine_current_df, time %in% sircovid:::sircovid_date(times_to_keep))

## Sum over all age groups too:
# 2) create totals across age groups (drop any pre-existing "total" rows first)
totals_df <- vaccine_current_df_reduced %>%
  group_by(region, time, version, vaccine_strata) %>%
  summarise(
    population      = sum(population,      na.rm = TRUE),
    mean_cum_doses  = sum(mean_cum_doses,  na.rm = TRUE),
    mean_in_stage   = sum(mean_in_stage,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(age_group = "total") %>%
  #keep column order same as original
  select(region, time, age_group, population, vaccine_strata, mean_cum_doses, version, mean_in_stage, everything())

# 3) bind totals back on
vaccine_current_df_reduced <- bind_rows(
  vaccine_current_df_reduced,
  totals_df
) 

#Repeat that for our specific age groups of interest:
#0 - 49
totals_df <- vaccine_current_df_reduced %>%
  filter(age_group %in% c("0-4", "5-9", "10-14", "15-19", "20-24",
                          "25-29", "30-34", "35-39", "40-44", "45-49")) %>%
  group_by(region, time, version, vaccine_strata) %>%
  summarise(
    population      = sum(population,      na.rm = TRUE),
    mean_cum_doses  = sum(mean_cum_doses,  na.rm = TRUE),
    mean_in_stage   = sum(mean_in_stage,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(age_group = "0-49") %>%
  #keep column order same as original
  select(region, time, age_group, population, vaccine_strata, mean_cum_doses, version, mean_in_stage, everything())
vaccine_current_df_reduced <- bind_rows(
  vaccine_current_df_reduced,
  totals_df
) 
#50 - 64
totals_df <- vaccine_current_df_reduced %>%
  filter(age_group %in% c("50-54", "55-59", "60-64")) %>%
  group_by(region, time, version, vaccine_strata) %>%
  summarise(
    population      = sum(population,      na.rm = TRUE),
    mean_cum_doses  = sum(mean_cum_doses,  na.rm = TRUE),
    mean_in_stage   = sum(mean_in_stage,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(age_group = "50-64") %>%
  #keep column order same as original
  select(region, time, age_group, population, vaccine_strata, mean_cum_doses, version, mean_in_stage, everything())
vaccine_current_df_reduced <- bind_rows(
  vaccine_current_df_reduced,
  totals_df
) 
#65-79
totals_df <- vaccine_current_df_reduced %>%
  filter(age_group %in% c("65-69", "70-74", "75-79")) %>%
  group_by(region, time, version, vaccine_strata) %>%
  summarise(
    population      = sum(population,      na.rm = TRUE),
    mean_cum_doses  = sum(mean_cum_doses,  na.rm = TRUE),
    mean_in_stage   = sum(mean_in_stage,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(age_group = "65-79") %>%
  #keep column order same as original
  select(region, time, age_group, population, vaccine_strata, mean_cum_doses, version, mean_in_stage, everything())
vaccine_current_df_reduced <- bind_rows(
  vaccine_current_df_reduced,
  totals_df
) 

# Convert time to date and factors:
vaccine_current_df_reduced$time <- sircovid:::sircovid_date_as_date(vaccine_current_df_reduced$time)

#Create "no vaccination" rows.
###############################
# 1) Make sure the factor has the new level (so the new rows keep the same factor type)
vaccine_current_df_reduced <- vaccine_current_df_reduced %>%
  mutate(vaccine_strata = fct_expand(vaccine_strata, "No vaccinations"))
# levels(vaccine_current_df_reduced$vaccine_strata) <- c("No vaccinations",
#                                                       "Had 1st dose",
#                                                       "Full dose 1 protection",
#                                                       "Full dose 2 protection",
#                                                       "Waned dose 2 protection",
#                                                       "Full dose 3 protection",
#                                                       "Waned dose 3 protect",
#                                                       "SHOULD BE EMPTY")

# 2) Extract mean_cum_doses for "Had 1st dose"
had1 <- vaccine_current_df_reduced %>%
  filter(vaccine_strata == "Had 1st dose") %>%
  select(region, time, age_group, version, population, had1 = mean_cum_doses)

# 3) Build one new row per region/time/age_group/version (join had1 if present)
new_rows <- vaccine_current_df_reduced %>%
  #keep one row per region/time/age/version/population
  distinct(region, time, age_group, version, population) %>%
  #add the matching "had1" column
  left_join(had1,
            by = c("region", "time", "age_group", "version", "population")) %>%
  mutate(
    had1 = coalesce(had1, 0),                        # if no Had1 row, assume 0
    mean_in_stage = pmax(population - had1, 0),      # population - had1, clamp at 0
    mean_cum_doses = NA_real_,                       # 0 cumulative for this row
    vaccine_strata = factor("No vaccinations", levels = levels(vaccine_current_df_reduced$vaccine_strata))
  ) %>%
  # order columns to match original df
  select(names(vaccine_current_df_reduced))    

# 4) Bind and re-arrange
vaccine_current_df_extended <- bind_rows(vaccine_current_df_reduced, new_rows) %>%
  arrange(region, time, age_group, version, vaccine_strata)

# 5) Rename and relevel
vaccine_current_df_extended <- vaccine_current_df_extended %>%
  mutate(
    vaccine_strata = vaccine_strata %>%
      fct_recode(
        "Waned dose 3 protection" = "Waned dose 3 protect"
      ) %>%
      fct_relevel(
        "No vaccinations",
        "Had 1st dose",
        "Full dose 1 protection",
        "Full dose 2 protection",
        "Waned dose 2 protection",
        "Full dose 3 protection",
        "Waned dose 3 protection",
        "SHOULD BE EMPTY"
      )
  )

## Pluck out a subset to plot:
plot_df <- vaccine_current_df_extended %>% 
  # choose which age_group to plot
  filter(age_group %in% c("0-49", "50-64", "65-79", "80+")) %>% 
  filter(region == "england") %>%
  mutate(time = as.Date(time)) %>%
  # compute proportion in each stage (mean_in_stage must already exist)
  mutate(prop_in_stage = mean_in_stage / population) %>%
  arrange(time, version, vaccine_strata)

# ---- plotting ----
# fill palette:
my_palette <- c(
  "No vaccinations"        = "#B45F5F",
  "Had 1st dose"           = "#FAD675",
  "Full dose 1 protection" = "#6BAED6",
  "Full dose 2 protection" = "#3182BD",
  "Full dose 3 protection" = "#08519C",
  "Waned dose 2 protection"= "#D4A017",
  "Waned dose 3 protection"= "#A6761D"
)

# Map version -> alpha & linetype
alpha_map <- c("Factual" = 1.0, "Counterfactual" = 0.65)
linetype_map <- c("Factual" = "solid", "Counterfactual" = "dashed")

nudge_days <- 6  # how far to separate the two bars (in days)
plot_df_nudged <- plot_df %>%
  mutate(
    time_nudge = case_when(
      version == "Factual"       ~ time - days(nudge_days),
      version == "Counterfactual"~ time + days(nudge_days),
      TRUE                       ~ time  # fallback if other labels exist
    )
  )

plot_df_nudged <- plot_df_nudged |>
  dplyr::mutate(
    age_num = as.numeric(factor(age_group, levels = c("0-49", "50-64", "65-79", "80+"))),
    age_nudge = dplyr::case_when(
      version == "Factual" ~ age_num - 0.18,
      version == "Counterfactual" ~ age_num + 0.18
    )
  )

#We don't have boosters yet, so no need to include them
plot_df_nudged <- filter(plot_df_nudged,
                         vaccine_strata %in% c("No vaccinations", "Had 1st dose", 
                                               "Full dose 1 protection", "Full dose 2 protection",
                                               "Waned dose 2 protection"))

label_df <- plot_df_nudged %>%
  filter(age_nudge < 1.5) %>%
  group_by(age_nudge, version) %>%
  summarise(y_label = max(prop_in_stage) - 0.2, .groups = "drop") %>%
  mutate(version_label = recode(version,
                                "Factual" = "2020 Baseline",
                                "Counterfactual_2047" = "Counterfactual"))

ggplot(plot_df_nudged,
       aes(x = age_nudge,   # <-- use nudged numeric x
           y = prop_in_stage,
           fill = vaccine_strata,
           alpha = version,
           linetype = version)) +
  geom_col(
    colour = "grey20",
    linewidth = 0.3,
    width = 0.35   # narrower bars to match spacing
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4),
    labels = c("0-49", "50-64", "65-79", "80+"),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = c(0, 0)
                     ) +
  scale_fill_manual(values = my_palette) +
  scale_alpha_manual(name = "Model", values = alpha_map,
                     #guide = "none" #remove to return to legend
                     ) +
  scale_linetype_manual(
    name = "Model",
    values = linetype_map,
    guide = guide_legend(override.aes = list(fill = "grey70"))
  ) +
  labs(
    title = "Population proportion in each vaccine strata \n(June 1st 2021)",
    x = "Age group",
    y = "Proportion of population",
    fill = "Vaccine strata"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) + 
  guides(
    fill = guide_legend(nrow = 3),
    alpha = "none",#guide_legend(nrow = 3),
    linetype = "none"#guide_legend(nrow = 3)
  ) -> p5

p5 <- p5 +
  geom_text(
    data = label_df,
    aes(x = age_nudge, y = y_label, label = version_label),
    inherit.aes = FALSE,
    angle = 90,
    hjust = 0,          # text grows upward from the bar top
    vjust = 0.5,
    size = 3.5,
    fontface = "italic",
    colour = "grey20"
  )

ggsave(
  filename = "Figures/individual_panels/Fig3_p5.png",
  plot = p5,
  width = 16,      # inches
  height = 10,      # inches
  dpi = 320        # high resolution
)

#############################################################
# P6 - Level of average vaccine protection against infection in each of the 4 age groups
#Pull out the protection against infection data for each strata
ve_values <- baseline_principal$london$rel_severity_alpha_delta$rel_susceptibility
ve_infection <- ve_values[,2,]
# 19 rows by age (capturing difference in vaccine manufacturer)
#7 columns by vaccine strata

v_strata <- c("No vaccinations", "Had 1st dose", "Full dose 1 protection",
              "Full dose 2 protection", "Waned dose 2 protection")
plot_df_nudged$v_protection <- NA
for(i in 1:nrow(plot_df_nudged)){
  if(plot_df_nudged$version[i] == "Factual"){
    if(plot_df_nudged$age_group[i] == "0-49"){
      plot_df_nudged$v_protection[i] <- sum(sircovid:::sircovid_population("england")[1:10]*ve_infection[1:10,which(v_strata == plot_df_nudged$vaccine_strata[i])])/sum(sircovid:::sircovid_population("england")[1:10])
    } else if(plot_df_nudged$age_group[i] == "50-64"){
      plot_df_nudged$v_protection[i] <- sum(sircovid:::sircovid_population("england")[11:13]*ve_infection[11:13,which(v_strata == plot_df_nudged$vaccine_strata[i])])/sum(sircovid:::sircovid_population("england")[11:13])
    } else if(plot_df_nudged$age_group[i] == "65-79"){
      plot_df_nudged$v_protection[i] <- sum(sircovid:::sircovid_population("england")[14:16]*ve_infection[14:16,which(v_strata == plot_df_nudged$vaccine_strata[i])])/sum(sircovid:::sircovid_population("england")[14:16])
    } else if(plot_df_nudged$age_group[i] == "80+"){
      plot_df_nudged$v_protection[i] <- sum(sircovid:::sircovid_population("england")[17]*ve_infection[17,which(v_strata == plot_df_nudged$vaccine_strata[i])])/sum(sircovid:::sircovid_population("england")[17])
    }
  } else if(plot_df_nudged$version[i] == "Counterfactual"){
    if(plot_df_nudged$age_group[i] == "0-49"){
      plot_df_nudged$v_protection[i] <- sum(baseline_principal$england$population[1:10]*ve_infection[1:10,which(v_strata == plot_df_nudged$vaccine_strata[i])])/sum(baseline_principal$england$population[1:10])
    } else if(plot_df_nudged$age_group[i] == "50-64"){
      plot_df_nudged$v_protection[i] <- sum(baseline_principal$england$population[11:13]*ve_infection[11:13,which(v_strata == plot_df_nudged$vaccine_strata[i])])/sum(baseline_principal$england$population[11:13])
    } else if(plot_df_nudged$age_group[i] == "65-79"){
      plot_df_nudged$v_protection[i] <- sum(baseline_principal$england$population[14:16]*ve_infection[14:16,which(v_strata == plot_df_nudged$vaccine_strata[i])])/sum(baseline_principal$england$population[14:16])
    } else if(plot_df_nudged$age_group[i] == "80+"){
      plot_df_nudged$v_protection[i] <- sum(baseline_principal$england$population[17]*ve_infection[17,which(v_strata == plot_df_nudged$vaccine_strata[i])])/sum(baseline_principal$england$population[17])
    }
  }
}
#Change from susceptibility to protection:
plot_df_nudged$v_protection <- 1 - plot_df_nudged$v_protection
#Scale by proportion in that group:
plot_df_nudged$v_protection_average <- plot_df_nudged$v_protection*plot_df_nudged$prop_in_stage
#Sum over all the vaccine strata:
plot_df_sum <- plot_df_nudged |>
  dplyr::group_by(age_group, version) |>
  dplyr::summarise(
    v_protection_average = sum(v_protection_average, na.rm = TRUE)
  )

plot_df_sum <- plot_df_sum |>
  dplyr::mutate(
    age_num = as.numeric(factor(age_group, levels = c("0-49", "50-64", "65-79", "80+"))),
    age_nudge = dplyr::case_when(
      version == "Factual" ~ age_num - 0.18,
      version == "Counterfactual" ~ age_num + 0.18
    )
  )

ggplot(plot_df_sum,
       aes(x = age_nudge,
           y = v_protection_average,
           fill = version)) +   # <-- colour by version now
  geom_col(
    colour = "grey20",
    linewidth = 0.3,
    width = 0.35
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4),
    labels = c("0-49", "50-64", "65-79", "80+"),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c(
      "Factual" = "#1b9e77",
      "Counterfactual" = "#d95f02"
    ),
    breaks = c("Factual", "Counterfactual"),
    labels = c("2019 baseline", "2047 central \nprojection")
  ) +
  labs(
    x = "Age group",
    y = "Average vaccine effectiveness",
    fill = "Model",
    title = "Average population vaccine effectiveness \nvs. infection by age group (June 1st 2021)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) -> p6

ggsave(
  filename = "Figures/individual_panels/Fig3_p6.png",
  plot = p6,
  width = 16,      # inches
  height = 10,      # inches
  dpi = 320        # high resolution
)
#########################################
# p7 - Total average protection in the population
grey_lines <- c(
  "2020-03-25", ## First full lockdown
  "2020-05-11", ## Initial easing
  "2020-11-05", ## Lockdown 2 starts
  "2020-12-02", ## Lockdown 2 ends
  "2021-01-05", ## 16. Lockdown 3 starts
  "2021-03-08", ## 17. Step 1 of roadmap: schools reopen
  "2021-04-19", ## 19. Step 2 of roadmap: outdoors hospitality (04-12) 
  ##     and schools return (04-19)
  "2021-05-17", ## 20. Step 3 of roadmap: indoors hospitality
  "2021-07-19") ## 24. Step 4
grey_labels <- as.Date(grey_lines)
label_cols <- c(
  "red",
  "grey",
  "red",
  "grey",
  "red",
  "grey",
  "grey",
  "grey",
  "grey"
)
#We give some label positions a nudge to aid plotting
grey_labels[5] <- grey_labels[5] 
grey_labels[4] <- grey_labels[4] + 10


protection_data <- filter(simulation_data,
                          output_type %in% c("protected_S_vaccinated_weighted",       
                                             "protected_R_vaccinated_weighted",
                                             "protected_R_unvaccinated_weighted"),
                          version %in% c("Factual", "Counterfactual_2047"),
                          region == "england")
#write.csv(protection_data, "protection_data.csv", row.names = FALSE)

ggplot(
  protection_data,
  aes(
    x = sircovid:::sircovid_date_as_date(time),
    y = mean/population,
    colour = output_type,
    linetype = version
  )
) +
  geom_ribbon(
    aes(
      ymin = cri_lower/population,
      ymax = cri_upper/population,
      fill = output_type,
      group = interaction(output_type, version)
    ),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(
      "protected_S_vaccinated_weighted" = "#7570b3",
      "protected_R_vaccinated_weighted" = '#9C755F',
      "protected_R_unvaccinated_weighted" = '#CC3311'
    ),
    labels = c(
      "protected_S_vaccinated_weighted" = "Vaccinated \n(susceptible)",
      "protected_R_vaccinated_weighted" = "Vaccinated \n(recovered)",
      "protected_R_unvaccinated_weighted" = "Unvaccinated \n(recovered)"
    ),
    name = "Protection type"
  ) +
  geom_vline(xintercept = as.Date('2021-03-31'), alpha = 0.8, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = label_cols, lty = 'dashed') +
  scale_fill_manual(
    values = c(
      "protected_S_vaccinated_weighted" = "#7570b3",
      "protected_R_vaccinated_weighted" = '#9C755F',
      "protected_R_unvaccinated_weighted" = '#CC3311'
    ),
    labels = c(
      "protected_S_vaccinated_weighted" = "Vaccinated \n(susceptible)",
      "protected_R_vaccinated_weighted" = "Vaccinated \n(recovered)",
      "protected_R_unvaccinated_weighted" = "Unvaccinated \n(recovered)"
    ),
    name = "Protection type"#,
    #guide = "none"
  ) +
  scale_linetype_manual(
    values = c(
      "Factual" = "solid",
      "Counterfactual_2047" = "dashed"
    ),
    breaks = c("Factual", "Counterfactual_2047"),
    labels = c(
      "Factual" = "2020 baseline",
      "Counterfactual_2047" = "2047 central projection"
    ),
    name = "Model"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  labs(
    x = "Time",
    y = "Population protected",
    title = "Population protection against infection over time"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_x_date(date_labels = "%b %Y") +
  guides(
    linetype = guide_legend(
      keywidth = unit(2, "cm")
  ) )-> p7

ggsave(
  filename = "Figures/individual_panels/Fig3_p7.png",
  plot = p7,
  width = 16,      # inches
  height = 10,      # inches
  dpi = 320        # high resolution
)
#########################################
#p8 - Absolute difference in levels of protection
protection_data <- protection_data %>%
  mutate(
    date              = sircovid:::sircovid_date_as_date(time),
    proportion        = mean / population,
    proportion_lower  = cri_lower / population,
    proportion_upper  = cri_upper / population
  )

protection_diff <- protection_data %>%
  select(date, output_type, version, proportion, proportion_lower, proportion_upper) %>%
  pivot_wider(
    names_from  = version,
    values_from = c(proportion, proportion_lower, proportion_upper)
  ) %>%
  mutate(
    diff       = proportion_Counterfactual_2047 - proportion_Factual,
    diff_lower = proportion_lower_Counterfactual_2047 - proportion_lower_Factual,
    diff_upper = proportion_upper_Counterfactual_2047 - proportion_upper_Factual
  )

ggplot(
  protection_diff,
  aes(x = date, y = diff, colour = output_type)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = as.Date('2021-03-31'), alpha = 0.8, colour = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = label_cols, lty = 'dashed') +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(
    values = c(
      "protected_S_vaccinated_weighted"    = "#7570b3",
      "protected_R_vaccinated_weighted"    = '#9C755F',
      "protected_R_unvaccinated_weighted"  = '#CC3311'
    ),
    labels = c(
      "protected_S_vaccinated_weighted"    = "Vaccinated \n(susceptible)",
      "protected_R_vaccinated_weighted"    = "Vaccinated \n(recovered)",
      "protected_R_unvaccinated_weighted"  = "Unvaccinated \n(recovered)"
    ),
    name = "Protection type"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0), limits = c(-0.04,0.03)
  ) +
  scale_x_date(date_labels = "%b %Y") +
  labs(
    x = "Time",
    y = "Difference in population protected\n(2047 counterfactual minus baseline)",
    title = "Difference in population protection over time"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position   = "top",
    legend.box        = "vertical",
    panel.grid.minor  = element_blank(),
    axis.line         = element_line(colour = "black")
  ) -> p8

ggsave(
  filename = "Figures/individual_panels/Fig3_p8.png",
  plot = p8,
  width = 16,      # inches
  height = 10,      # inches
  dpi = 320        # high resolution
)
#########################################
## Stick it all together into a possible figure 3
Fig3 <- plot_grid(p1, p2, 
                  #p3, p4,
                  
                  p5, p6,
                  p7, p8,
                  #p4 + theme(legend.position = "none"), 
                  nrow = 3, ncol = 2, 
                  labels = "AUTO",
                  #rel_widths = c(1.5, 1), 
                  align = "v")
Fig3 <- Fig3 + theme(plot.background = element_rect(fill = "white", colour = NA))
#final_patch <- p_england | p_map + plot_layout(widths = c(5, 2))

ggsave("Fig3.png", Fig3, width = 13, height = 16, dpi = 320)
ggsave("Fig3.pdf", Fig3, width = 16, height = 12, dpi = 320)
