
source("global_util.R")
source("plotting_functions.R")
## Load simulation data
########################
simulation_data <- readRDS("simulation_data.rds")
dir.create("Figures")
dir.create("Figures/individual_panels")
########################
# Panel 1 - eff_Rt
p1_sim_data <- simulation_data %>%
  filter(output_type == "eff_Rt_all") %>%
  filter(version %in% c("Factual", "Counterfactual_principal"))
p1_real_data <- data.frame(time = NA, region = NA, population = NA, value = NA)
p1 <- plot_time_series(p1_sim_data, p1_real_data,
                       "Effective Rt", "england")
p1 <- p1 + ggtitle("Effective Reproduction Number") + guides(colour = guide_legend(override.aes = list(shape = NA)))
ggsave(
  filename = "Figures/individual_panels/Fig3_p1.png",
  plot = p1,
  width = 10, height = 6, dpi = 320 
)

#Panel 1b - Rt
p1b_sim_data <- simulation_data %>%
  filter(output_type == "Rt_all") %>%
  filter(version %in% c("Factual", "Counterfactual_principal"))
p1b_real_data <- data.frame(time = NA, region = NA, population = NA, value = NA)
p1b <- plot_time_series(p1b_sim_data, p1b_real_data,
                       "Rt", "england")
p1b <- p1b + ggtitle("Reproduction Number") + guides(colour = guide_legend(override.aes = list(shape = NA)))
ggsave(
  filename = "Figures/individual_panels/Fig3_p1b.png",
  plot = p1b,
  width = 10, height = 6, dpi = 320 
)

######################################
# Panel 2 - Symptomatic Cases over time, but split up by age, 2019 Factual
#"#00468BFF" "#ED0000FF" "#42B540FF" "#0099B4FF" "#925E9FFF" "#FDAF91FF" "#AD002AFF" "#ADB6B6FF"
p2 <- plot_sympt_cases_age_stack(simulation_data, "england", "Factual")
p2 <- p2 + ggtitle("Pillar 2 Symptomatic Cases by Age (2019 Baseline)")

######################################
# Panel 3 - Symptomatic Cases over time, but split up by age, 2047 Counterfactual
p3 <- plot_sympt_cases_age_stack(simulation_data, "england", "Counterfactual_principal")
p3 <- p3 + ggtitle("Pillar 2 Symptomatic Cases by Age (2047 Central Projection)")

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

y2 <- get_y_range(p2)
y3 <- get_y_range(p3)
upper_max <- max(y2[2], y3[2])
lower_min <- min(y2[1], y3[1]) 
p2 <- p2 + coord_cartesian(ylim = c(lower_min, upper_max))
p3 <- p3 + coord_cartesian(ylim = c(lower_min, upper_max))
ggsave(
  filename = "Figures/individual_panels/Fig3_p2.png",
  plot = p2,
  width = 10, height = 6, dpi = 320 
)
ggsave(
  filename = "Figures/individual_panels/Fig3_p3.png",
  plot = p3,
  width = 10, height = 6, dpi = 320 
)

#####################################
# Panel 4 - Do the same plot as above, but make it be max proportion split by ages
p4 <- plot_sympt_cases_age_proportion(
  sim_df = simulation_data,
  region_selected = "england",
  version_selected = "Factual"
) + ggtitle("Proportion of Symptomatic Cases by Age (2019 Baseline)")
ggsave(
  filename = "Figures/individual_panels/Fig3_p4.png",
  plot = p4,
  width = 10, height = 6, dpi = 320
)

#####################################
# Panel 5 - Counterfactual
p5 <- plot_sympt_cases_age_proportion(
  sim_df = simulation_data,
  region_selected = "england",
  version_selected = "Counterfactual_principal"
) + ggtitle("Proportion of Symptomatic Cases by Age (2047 Central Projection)")
ggsave(
  filename = "Figures/individual_panels/Fig3_p5.png",
  plot = p5,
  width = 10, height = 6, dpi = 320
)

#####################################
# Panel 6 - The ratio difference plot
# Because time is a double we need to just make sure they're all within machine-precision:
simulation_data$time <- round(simulation_data$time)

p6_sim_data <- simulation_data |>
  dplyr::left_join(
    simulation_data |>
      dplyr::filter(version == "Factual") |>
      dplyr::distinct(region, time, factual_population = population),
    by = c("region", "time")
  ) |>
  dplyr::mutate(
    population_ratio_vs_factual = population / factual_population
  )
p6_sim_data <- p6_sim_data |>
  dplyr::left_join(
    p6_sim_data |>
      dplyr::filter(version == "Factual") |>
      dplyr::distinct(region, time, output_type, factual_mean = mean),
    by = c("region", "time", "output_type")
  ) |>
  dplyr::mutate(
    mean_ratio_vs_factual = mean / factual_mean
  )

p6_sim_data <- p6_sim_data |>
  filter(version == "Counterfactual_principal") |>
  filter(output_type %in% c("sympt_cases_under15_inc", "sympt_cases_15_24_inc",
                            "sympt_cases_25_49_inc", "sympt_cases_50_64_inc",
                            "sympt_cases_65_79_inc", "sympt_cases_80_plus_inc" ))


p6 <- plot_population_adjusted_case_ratio(
  sim_df = p6_sim_data,
  region_selected = "england"
) + ggtitle("Population-adjusted Daily Symptomatic Cases Ratio (2019 Baseline vs 2047 Central Projection)")
ggsave(
  filename = "Figures/individual_panels/Fig3_p6.png",
  plot = p6,
  width = 10, height = 6, dpi = 320
)

#######################
# Panel 7, same as above but for cumulative cases
p7_cum_sim_data <- p6_sim_data |>
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

p7 <- plot_population_adjusted_cumulative_case_ratio(
  sim_df = p7_cum_sim_data,
  region_selected = "england"
) + ggtitle("Population-adjusted Cumulative Symptomatic Cases Ratio \n(2019 Baseline vs 2047 Central Projection)")
ggsave(
  filename = "Figures/individual_panels/Fig3_p7.png",
  plot = p7,
  width = 10, height = 6, dpi = 320
)


#########################################
## Stick it all together into a possible figure 3

Fig3 <- plot_grid(p2, p3, 
                  p4 + theme(legend.position = "none"), 
                  p5 + theme(legend.position = "none"), 
                  p1 ,#+ theme(legend.position = "none"), 
                  p7 + theme(legend.position = "none"), 
                  nrow = 3, ncol = 2, 
                  labels = "AUTO",
                  #rel_widths = c(1.5, 1), 
                  align = "v")
Fig3 <- Fig3 + theme(plot.background = element_rect(fill = "white", colour = NA))
#final_patch <- p_england | p_map + plot_layout(widths = c(5, 2))

ggsave("Fig3.png", Fig3, width = 16, height = 12, dpi = 320)
ggsave("Fig3.pdf", Fig3, width = 16, height = 12, dpi = 320)
