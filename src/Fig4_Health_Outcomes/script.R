
source("global_util.R")
## Load simulation data
########################
#Base params
base_params_2047 <- readRDS("dependencies/baseline_2047.rds")
base_params_2047$england <- list(
  population = Reduce(`+`, lapply(base_params_2047, `[[`, "population"))
)
base_params_2037 <- readRDS("dependencies/baseline_2037.rds")
base_params_2037$england <- list(
  population = Reduce(`+`, lapply(base_params_2037, `[[`, "population"))
)
base_params_2027 <- readRDS("dependencies/baseline_2027.rds")
base_params_2027$england <- list(
  population = Reduce(`+`, lapply(base_params_2027, `[[`, "population"))
)

# Simulation output and index names
baseline_df <- readRDS("dependencies/baseline_combined_output_dataframe.rds")
baseline_index_names <- readRDS("dependencies/baseline_index_names.rds")
counterfactual_2047_df <- readRDS("dependencies/2047_output_dataframe.rds")
counterfactual_2047_index_names <- readRDS("dependencies/2047_index_names.rds")
counterfactual_2037_df <- readRDS("dependencies/2037_output_dataframe.rds")
counterfactual_2037_index_names <- readRDS("dependencies/2037_index_names.rds")
counterfactual_2027_df <- readRDS("dependencies/2027_output_dataframe.rds")
counterfactual_2027_index_names <- readRDS("dependencies/2027_index_names.rds")

#Check all the index names align
if (!(identical(baseline_index_names, counterfactual_2047_index_names) && identical(baseline_index_names, counterfactual_2037_index_names) && identical(baseline_index_names, counterfactual_2027_index_names))) {
  stop("Index names are not identical")
}

#Add population to the simulation output dataframes
baseline_df$version <- "Factual"
baseline_df <- baseline_df %>%
  rowwise() %>%
  mutate(population = sum(sircovid:::sircovid_population(region))) %>%
  ungroup()

counterfactual_2047_df$version <- "Counterfactual_2047"
counterfactual_2047_df <- counterfactual_2047_df %>%
  rowwise() %>%
  mutate(population = sum(base_params_2047[[region]]$population)) %>%
  ungroup()

counterfactual_2037_df$version <- "Counterfactual_2037"
counterfactual_2037_df <- counterfactual_2037_df %>%
  rowwise() %>%
  mutate(population = sum(base_params_2037[[region]]$population)) %>%
  ungroup()

counterfactual_2027_df$version <- "Counterfactual_2027"
counterfactual_2027_df <- counterfactual_2027_df %>%
  rowwise() %>%
  mutate(population = sum(base_params_2027[[region]]$population)) %>%
  ungroup()

# Stick all simulation data together:
simulation_data <- rbind(baseline_df, counterfactual_2047_df, 
                         counterfactual_2037_df, counterfactual_2027_df)


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
# NOTE: The cols named below won't appear in england totals because we can't share the data, they're all NAs
# It's all the pillar 2 data
colnames(england_data)[which(!(colnames(england_data) %in% colnames(england_totals)))]

#Add together the age_specific hospitalisations so I can focus on totals:
england_data <- england_data %>%
  mutate(total_confirmed_admissions = rowSums(across(c("admissions_0_9",                         
                                       "admissions_10_19",  "admissions_20_29",                       
                                       "admissions_30_39",  "admissions_40_49",                       
                                       "admissions_50_59",  "admissions_60_69",                       
                                       "admissions_70_79",  "admissions_80_plus")),
                              na.rm = TRUE))

#Remove all the stuff we don't need anymore:
rm(baseline_df, counterfactual_2047_df,
   counterfactual_2037_df, counterfactual_2027_df,
   england_totals,
   base_params_2047, base_params_2037, base_params_2027,
   counterfactual_2047_index_names, counterfactual_2037_index_names, counterfactual_2027_index_names)

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
saveRDS(england_data, "Figure_dataframes/england_data.rds")
####################################################################
#Plot 1 - confirmed hospital admissions
p1_sim_data <- simulation_data %>%
  filter(output_type == "total_hospitalisations") %>%
  filter(version %in% c("Factual", "Counterfactual_2047"))
p1_real_data <- england_data %>%
  select(date, region, population, total_confirmed_admissions)
colnames(p1_real_data) <- c("time", "region", "population", "value")

p1 <- plot_time_series(p1_sim_data, p1_real_data,
                       "Daily Hospitalisations", "england")
p1 <- p1 + ggtitle("Daily Confirmed Hospitalisations")
ggsave(
  filename = "Figures/individual_panels/Fig2_p1.png",
  plot = p1,
  width = 10,      # inches
  height = 6,      # inches
  dpi = 320        # high resolution
)

p1_per_capita <- plot_time_series_per_capita(p1_sim_data, p1_real_data,
                       "Daily Hospitalisations", "england")
p1_per_capita <- p1_per_capita + ggtitle("Daily new confirmed hospitalisations per capita - England")
ggsave(
  filename = "Figures/individual_panels/Fig2_p1_per_capita.png",
  plot = p1_per_capita,
  width = 10,      # inches
  height = 6,      # inches
  dpi = 320        # high resolution
)

#Do for each separate region
dir.create("Figures/individual_panels/by_region")
regions <- sircovid:::regions("england")
for(r in regions){
  p <- plot_time_series(p1_sim_data, p1_real_data,
                        "Daily Hospitalisations", r)
  ggsave(
    filename = sprintf("Figures/individual_panels/by_region/Fig2_p1_%s.png", r),
    plot = p,
    width = 10,      # inches
    height = 6,      # inches
    dpi = 320        # high resolution
  )
}

############################################################################
#Panel 2 - Cumulative hospitalisations by scenario
p2_sim_data <- simulation_data %>%
  filter(output_type == "total_hospitalisations")
p2 <- plot_cumulative(p2_sim_data, "Hospitalisations \n(thousands)", "england")
p2 <- p2 + ggtitle("Cumulative Daily Confirmed Hospitalisations")
ggsave(
  filename = "Figures/individual_panels/Fig2_p2.png",
  plot = p2,
  width = 10, height = 6, dpi = 320        
)

p2_per_capita <- plot_cumulative_per_capita(p2_sim_data, "Hospitalisations", "england")
p2_per_capita <- p2_per_capita + ggtitle("Cumulative Daily Confirmed Hospitalisations Per Capita")
ggsave(
  filename = "Figures/individual_panels/Fig2_p2_per_capita.png",
  plot = p2_per_capita,
  width = 10, height = 6, dpi = 320        
)

# Plot panel 1 with all scenarios
p <- plot_time_series(p2_sim_data, p1_real_data, "Daily hosps", "england")
ggsave(
  filename = "Figures/individual_panels/Fig2_p1_all_scenarios.png",
  plot = p,
  width = 10, height = 6, dpi = 320        
)
#########################################################################
#Panel 3 - Hospital deaths

p3_sim_data <- simulation_data %>%
  filter(output_type == "deaths_hosp_inc") %>%
  filter(version %in% c("Factual", "Counterfactual_2047"))
p3_real_data <- england_data %>%
  select(date, region, population, ons_death_hospital)
colnames(p3_real_data) <- c("time", "region", "population", "value")

p3 <- plot_time_series(p3_sim_data, p3_real_data,
                       "Daily Hospital Deaths", "england")
p3 <- p3 + ggtitle("Daily Hospital Deaths")
ggsave(
  filename = "Figures/individual_panels/Fig2_p3.png",
  plot = p3,
  width = 10, height = 6, dpi = 320 
)

p3_per_capita <- plot_time_series_per_capita(p3_sim_data, p3_real_data,
                       "Daily Hospital Deaths", "england")
p3_per_capita <- p3_per_capita + ggtitle("Daily Hospital Deaths Per Capita")
ggsave(
  filename = "Figures/individual_panels/Fig2_p3_per_capita.png",
  plot = p3_per_capita,
  width = 10, height = 6, dpi = 320 
)

#############################################
# Panel 4 ICU beds
p4_sim_data <- simulation_data %>%
  filter(output_type == "icu") %>%
  filter(version %in% c("Factual", "Counterfactual_2047"))
p4_real_data <- england_data %>%
  select(date, region, population, phe_occupied_mv_beds) #mech_vent_covid) #phe_occupied_mv_beds 
colnames(p4_real_data) <- c("time", "region", "population", "value")

p4 <- plot_time_series(p4_sim_data, p4_real_data,
                       "ICU beds", "england")
p4 <- p4 + ggtitle("Occupied ICU beds - England")
ggsave(
  filename = "Figures/individual_panels/Fig2_p4.png",
  plot = p4,
  width = 10, height = 6, dpi = 320 
)

############################################
# Panel 5 Hospital deaths cumulative

p5_sim_data <- simulation_data %>%
  filter(output_type == "deaths_hosp_inc")
p5 <- plot_cumulative(p5_sim_data, "Hospital \nDeaths (thousands)", "england", -20)
p5 <- p5 + ggtitle("Cumulative Hospital Deaths")
ggsave(
  filename = "Figures/individual_panels/Fig2_p5.png",
  plot = p5,
  width = 10, height = 6, dpi = 320        
)

p5_per_capita <- plot_cumulative_per_capita(p5_sim_data, "Hospitalisations", "england", -0.0003)
p5_per_capita <- p5_per_capita + ggtitle("Cumulative Hospital Deaths Per Capita")
ggsave(
  filename = "Figures/individual_panels/Fig2_p5_per_capita.png",
  plot = p5_per_capita,
  width = 10, height = 6, dpi = 320        
)

# Plot panel 3 with all scenarios
p <- plot_time_series(p5_sim_data, p3_real_data, "Daily Hospital Deaths", "england")
ggsave(
  filename = "Figures/individual_panels/Fig2_p3_all_scenarios.png",
  plot = p,
  width = 10, height = 6, dpi = 320        
)

#################################################
# Panel 6 - IHR
p6_sim_data <- simulation_data %>%
  filter(output_type == "ihr") %>%
  filter(version %in% c("Factual", "Counterfactual_2047"))
p6_real_data <- data.frame(time = NA, region = NA, population = NA, value = NA)

p6 <- plot_time_series(p6_sim_data, p6_real_data,
                       "IHR", "england")
p6 <- p6 + ggtitle("IHR")
ggsave(
  filename = "Figures/individual_panels/Fig2_p6.png",
  plot = p6,
  width = 10, height = 6, dpi = 320 
)

#################################################
# Panel 7 - IFR
p7_sim_data <- simulation_data %>%
  filter(output_type == "ifr") %>%
  filter(version %in% c("Factual", "Counterfactual_2047"))
p7_real_data <- data.frame(time = NA, region = NA, population = NA, value = NA)

p7 <- plot_time_series(p7_sim_data, p7_real_data,
                       "IFR", "england")
p7 <- p7 + ggtitle("IFR")
ggsave(
  filename = "Figures/individual_panels/Fig2_p7.png",
  plot = p7,
  width = 10, height = 6, dpi = 320 
)

#################################################
# Put together a final figure

Fig2 <- plot_grid(p1, p2, 
                  p3 + theme(legend.position = "none"), 
                  p5 + theme(legend.position = "none"), 
                  p6 + theme(legend.position = "none"), 
                  p7 + theme(legend.position = "none"), 
                  nrow = 3, ncol = 2, 
                  labels = "AUTO",
                  #rel_widths = c(1.5, 1), 
                  align = "v")
Fig2 <- Fig2 + theme(plot.background = element_rect(fill = "white", colour = NA))
#final_patch <- p_england | p_map + plot_layout(widths = c(5, 2))

ggsave("Fig2.png", Fig2, width = 16, height = 12, dpi = 320)
ggsave("Fig2.pdf", Fig2, width = 16, height = 12, dpi = 320)
