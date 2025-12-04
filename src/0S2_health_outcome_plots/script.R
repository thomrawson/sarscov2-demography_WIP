
source("global_util.R")
## Load data:
base_params <- readRDS("base.rds")
base_params$england <- list(
  population = Reduce(`+`, lapply(base_params, `[[`, "population"))
)
baseline_df <- readRDS("baseline_combined_output_dataframe.rds")
baseline_index_names <- readRDS("baseline_index_names.rds")
counterfactual_df <- readRDS("combined_output_dataframe.rds")
counterfactual_index_names <- readRDS("index_names.rds")

baseline_df$version <- "Factual"
baseline_df <- baseline_df %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
counterfactual_df$version <- "Counterfactual"
counterfactual_df <- counterfactual_df %>%
  rowwise() %>%
  mutate(population = sum(base_params[[region]]$population)) %>%
  ungroup()


# Load the real-world data
england_data <- read_csv("england_region_data.csv")
# Sum them together to make the england totals:
england_totals <- england_data %>%
  group_by(date) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))
england_totals$region <- "england"
england_data <- bind_rows(england_data, england_totals)
england_data$Nothing <- NA
england_data <- england_data %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()
# NOTE: These cols below won't appear in england totals because can't share the data, they're all NAs
# It's all the pillar 2 data
colnames(england_data)[which(!(colnames(england_data) %in% colnames(england_totals)))]

# We need to select which indices we want to plot, and if there is assoc. date with it
plot_columns <- data.frame(index_names = c("icu",
                                           "deaths_hosp_inc",
                                           "deaths_hosp_0_49_inc",
                                           "deaths_hosp_50_54_inc",
                                           "deaths_hosp_55_59_inc",
                                           "deaths_hosp_60_64_inc",
                                           "deaths_hosp_65_69_inc",
                                           "deaths_hosp_70_74_inc",
                                           "deaths_hosp_75_79_inc",
                                           "deaths_hosp_80_plus_inc",
                                           "deaths_comm_inc",
                                           "deaths_comm_0_49_inc",
                                           "deaths_comm_50_54_inc",
                                           "deaths_comm_55_59_inc",
                                           "deaths_comm_60_64_inc",
                                           "deaths_comm_65_69_inc",
                                           "deaths_comm_70_74_inc",
                                           "deaths_comm_75_79_inc",
                                           "deaths_comm_80_plus_inc",
                                           "admitted_inc", # general is phe_patients - mv_beds #"general",
                                           "all_admission_0_9_inc",
                                           "all_admission_10_19_inc", 
                                           "all_admission_20_29_inc",
                                           "all_admission_30_39_inc",
                                           "all_admission_40_49_inc",  
                                           "all_admission_50_59_inc",
                                           "all_admission_60_69_inc",
                                           "all_admission_70_79_inc",  
                                           "all_admission_80_plus_inc",
                                           # "react_pos",
                                           # "react_5_24_pos",                    
                                           # "react_25_34_pos",
                                           # "react_35_44_pos",                   
                                           # "react_45_54_pos",
                                           # "react_55_64_pos",                   
                                           # "react_65_plus_pos",
                                           "sympt_cases_inc",
                                           "sympt_cases_over25_inc",
                                           "sympt_cases_15_24_inc",
                                           "sympt_cases_25_49_inc",
                                           "sympt_cases_50_64_inc",
                                           "sympt_cases_65_79_inc",
                                           "sympt_cases_80_plus_inc",
                                           "ihr",
                                           "ifr",
                                           "hfr",
                                           "eff_Rt_all",
                                           "eff_Rt_general",
                                           "Rt_all",
                                           "Rt_general"
                                           ),
                           data_names = c("phe_occupied_mv_beds",
                                          "ons_death_hospital",
                                          "ons_death_hosp_0_49",
                                          "ons_death_hosp_50_54",
                                          "ons_death_hosp_55_59",
                                          "ons_death_hosp_60_64",
                                          "ons_death_hosp_65_69",
                                          "ons_death_hosp_70_74",
                                          "ons_death_hosp_75_79",
                                          "ons_death_hosp_80_plus",
                                          "ons_death_noncarehome",
                                          "ons_death_non_hosp_0_49",
                                          "ons_death_non_hosp_50_54",
                                          "ons_death_non_hosp_55_59",
                                          "ons_death_non_hosp_60_64",
                                          "ons_death_non_hosp_65_69",
                                          "ons_death_non_hosp_70_74",
                                          "ons_death_non_hosp_75_79",
                                          "ons_death_non_hosp_80_plus",
                                          "phe_patients",
                                          "admissions_0_9",
                                          "admissions_10_19",
                                          "admissions_20_29",
                                          "admissions_30_39",
                                          "admissions_40_49",
                                          "admissions_50_59",
                                          "admissions_60_69",
                                          "admissions_70_79",
                                          "admissions_80_plus",
                                          # "react_positive",
                                          # "react_positive_5_24",
                                          # "react_positive_25_34",
                                          # "react_positive_35_44",
                                          # "react_positive_45_54",
                                          # "react_positive_55_64",
                                          # "react_positive_65_plus",
                                          "pillar2_positives_symp_pcr_only",
                                          "pillar2_positives_symp_pcr_only_over25",
                                          #"pillar2_positives_symp_pcr_only_under15",
                                          "pillar2_positives_symp_pcr_only_15_24",
                                          "pillar2_positives_symp_pcr_only_25_49",
                                          "pillar2_positives_symp_pcr_only_50_64",
                                          "pillar2_positives_symp_pcr_only_65_79",
                                          "pillar2_positives_symp_pcr_only_80_plus",
                                          "Nothing",
                                          "Nothing",
                                          "Nothing",
                                          "Nothing",
                                          "Nothing",
                                          "Nothing",
                                          "Nothing"
                                          ),
                           titles = c("ICU beds",
                                      "Deaths in hospital",
                                      "Deaths in hospital (age 0-49)",
                                      "Deaths in hospital (age 50-54)",
                                      "Deaths in hospital (age 55-59)",
                                      "Deaths in hospital (age 60-64)",
                                      "Deaths in hospital (age 65-69)",
                                      "Deaths in hospital (age 70-74)",
                                      "Deaths in hospital (age 75-79)",
                                      "Deaths in hospital (age 80+)",
                                      "Deaths in community",
                                      "Deaths in community (age 0-49)",
                                      "Deaths in community (age 50-54)",
                                      "Deaths in community (age 55-59)",
                                      "Deaths in community (age 60-64)",
                                      "Deaths in community (age 65-69)",
                                      "Deaths in community (age 70-74)",
                                      "Deaths in community (age 75-79)",
                                      "Deaths in community (age 80+)",
                                      "PHE - general patients",
                                      "Hospital admissions (age 0-9)",
                                      "Hospital admissions (age 10-19)", 
                                      "Hospital admissions (age 20-29)",
                                      "Hospital admissions (age 30-39)",
                                      "Hospital admissions (age 40-49)",  
                                      "Hospital admissions (age 50-59)",
                                      "Hospital admissions (age 60-69)",
                                      "Hospital admissions (age 70-79)",  
                                      "Hospital admissions (age 80-89)",
                                      # "Positive REACT samples",
                                      # "Positive REACT samples (age 5-24)",                    
                                      # "Positive REACT samples (age 25-34)",
                                      # "Positive REACT samples (age 35-44)",                   
                                      # "Positive REACT samples (age 45-54)",
                                      # "Positive REACT samples (age 55-64)",                   
                                      # "Positive REACT samples (age 65+)",
                                      "New pillar 2 symptomatic cases",
                                      "New pillar 2 symptomatic cases (age 25+)",
                                      "New pillar 2 symptomatic cases (age 15-24)",
                                      "New pillar 2 symptomatic cases (age 25-49)",
                                      "New pillar 2 symptomatic cases (age 50-64)",
                                      "New pillar 2 symptomatic cases (age 65-79)",
                                      "New pillar 2 symptomatic cases (age 80+)",
                                      "IHR",
                                      "IFR",
                                      "HFR",
                                      "eff_Rt_all",
                                      "eff_Rt_general",
                                      "Rt_all",
                                      "Rt_general"
                           )
                           )


#Now plot one by one:

plot_time_series <- function(sim_df, data_df, number, region) {
  #Key dates here:
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
  
  ggplot(sim_df, aes(x = sircovid::sircovid_date_as_date(time), y = mean, colour = version, fill = version)) +
    geom_ribbon(aes(ymin = cri_lower, ymax = cri_upper), alpha = 0.2, colour = NA) +
    geom_line(size = 1) +
    geom_point(data = data_df, aes(x=as.Date(time), y = value, color = "Data", fill = "Data"), alpha = 0.3) +
    geom_vline(xintercept = as.Date('2021-03-31'), alpha = 0.8, color = 'black') +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = label_cols, lty = 'dashed') +
    scale_colour_manual(values = c("Factual" = "#1b9e77",
                                   "Counterfactual" = "#d95f02",
                                   "Data" = "firebrick"),
                        breaks = c("Factual", "Counterfactual", "Data"),
                        labels = c("2019 baseline", "2047 projection", "Data")) +
    scale_fill_manual(values = c("Factual" = "#1b9e77",
                                 "Counterfactual" = "#d95f02",
                                 "Data" = "firebrick"),
                      breaks = c("Factual", "Counterfactual", "Data"),
                      labels = c("2019 baseline", "2047 projection", "Data")) +
    labs(
      x = "Time",
      y = plot_columns$titles[number],
      colour = "Scenario:",
      fill = "Scenario:",
      title = sprintf("%s - %s",plot_columns$titles[number], region)
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%b %Y")
}

plot_time_series_per_capita <- function(sim_df, data_df, number, region) {
  #Key dates here:
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
  
  ggplot(sim_df, aes(x = sircovid::sircovid_date_as_date(time), y = (mean/population)*1000, colour = version, fill = version)) +
    geom_ribbon(aes(ymin = (cri_lower/population)*1000, ymax = (cri_upper/population)*1000), alpha = 0.2, colour = NA) +
    geom_line(size = 1) +
    geom_point(data = data_df, aes(x=as.Date(time), y = (value/population)*1000, color = "Data", fill = "Data"), alpha = 0.3) +
    geom_vline(xintercept = as.Date('2021-03-31'), alpha = 0.8, color = 'black') +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = label_cols, lty = 'dashed') +
    scale_colour_manual(values = c("Factual" = "#1b9e77",
                                   "Counterfactual" = "#d95f02",
                                   "Data" = "firebrick"),
                        breaks = c("Factual", "Counterfactual", "Data"),
                        labels = c("2019 baseline", "2047 projection", "Data")) +
    scale_fill_manual(values = c("Factual" = "#1b9e77",
                                 "Counterfactual" = "#d95f02",
                                 "Data" = "firebrick"),
                      breaks = c("Factual", "Counterfactual", "Data"),
                      labels = c("2019 baseline", "2047 projection", "Data")) +
    labs(
      x = "Time",
      y = sprintf("%s (per 1000 people)", plot_columns$titles[number]),
      colour = "Scenario:",
      fill = "Scenario:",
      title = sprintf("%s - %s",plot_columns$titles[number], region)
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%b %Y")
}


per_capita_plots <- 1:36
regions <- c("england", sircovid::regions("england"))
dir.create("regional_plots")
dir.create("england_plots")
dir.create("per_capita_plots")
dir.create("per_capita_plots/regional_plots")
dir.create("per_capita_plots/england_plots")
for(i in 1:nrow(plot_columns)){
  for(r in regions){
    plot_baseline <- filter(baseline_df, output_type == plot_columns$index_names[i]) %>%
      filter(region == r)
    
    plot_counterfactual <- filter(counterfactual_df, output_type == plot_columns$index_names[i]) %>%
      filter(region == r)
    plot_df <- rbind(plot_baseline, plot_counterfactual)
    
    plot_data <- england_data %>%
      select(date, region, population, plot_columns$data_names[i]) %>%
      filter(region == r)
    colnames(plot_data) <- c("time", "region", "population", "value")
    p <- plot_time_series(plot_df, plot_data, i, r)
    if(r == "england"){
      ggsave(
        filename = sprintf("england_plots/%s.png", plot_columns$index_names[i]),
        plot = p,
        width = 10,      # inches
        height = 6,      # inches
        dpi = 320        # high resolution
      )
    }else{
      ggsave(
        filename = sprintf("regional_plots/%s_%s.png", plot_columns$index_names[i], r),
        plot = p,
        width = 10,      # inches
        height = 6,      # inches
        dpi = 320        # high resolution
      )
    }
    
    # And the "per capita" plots
    if(i %in% per_capita_plots){
      p <- plot_time_series_per_capita(plot_df, plot_data, i, r)
      if(r == "england"){
        ggsave(
          filename = sprintf("per_capita_plots/england_plots/%s.png", plot_columns$index_names[i]),
          plot = p,
          width = 10,      # inches
          height = 6,      # inches
          dpi = 320        # high resolution
        )
      }else{
        ggsave(
          filename = sprintf("per_capita_plots/regional_plots/%s_%s.png", plot_columns$index_names[i], r),
          plot = p,
          width = 10,      # inches
          height = 6,      # inches
          dpi = 320        # high resolution
        )
      }
    }
    
    
  }  
}
