### All Fig2 plotting functions
######################################
#Panel 1
plot_time_series <- function(sim_df, data_df, outcome_label, region_selected) {
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
  
  ggplot(filter(sim_df, region == region_selected), 
         aes(x = sircovid::sircovid_date_as_date(time), y = mean, colour = version, fill = version)) +
    geom_ribbon(aes(ymin = cri_lower, ymax = cri_upper), alpha = 0.2, colour = NA) +
    geom_line(size = 1) +
    geom_point(data = filter(data_df, region == region_selected), 
               aes(x=as.Date(time), y = value, color = "Data", fill = "Data"), alpha = 0.3) +
    geom_vline(xintercept = as.Date('2021-03-31'), alpha = 0.8, color = 'black') +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = label_cols, lty = 'dashed') +
    scale_colour_manual(values = c("Factual" = "#1b9e77",
                                   "Counterfactual_principal" = "#d95f02",
                                   "Counterfactual_low_migration" = "#0099B4FF",
                                   "Counterfactual_high_migration" = "#925E9FFF",
                                   "Data" = "#AD002AFF"),
                        breaks = c("Factual", "Counterfactual_principal",
                                   "Counterfactual_low_migration",
                                   "Counterfactual_high_migration", "Data"),
                        labels = c("2019 baseline", "2047 central \nprojection", 
                                   "2047 low migration \nprojection",
                                   "2047 high migration \nprojection", "Data")) +
    scale_fill_manual(values = c("Factual" = "#1b9e77",
                                 "Counterfactual_principal" = "#d95f02",
                                 "Counterfactual_low_migration" = "#0099B4FF",
                                 "Counterfactual_high_migration" = "#925E9FFF",
                                 "Data" = "#AD002AFF"),
                      breaks = c("Factual", "Counterfactual_principal",
                                 "Counterfactual_low_migration",
                                 "Counterfactual_high_migration", "Data"),
                      labels = c("2019 baseline", "2047 central \nprojection", 
                                 "2047 low migration \nprojection",
                                 "2047 high migration \nprojection", "Data")) +
    labs(
      x = "Time",
      y = outcome_label,
      colour = "Scenario:",
      fill = "Scenario:",
      title = sprintf("%s - %s",outcome_label, region_selected)
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%b %Y")
}

plot_time_series_per_capita <- function(sim_df, data_df, outcome_label, region_selected) {
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
  
  ggplot(filter(sim_df, region == region_selected), 
         aes(x = sircovid::sircovid_date_as_date(time), y = (mean/population)*1000, colour = version, fill = version)) +
    geom_ribbon(aes(ymin = (cri_lower/population)*1000, ymax = (cri_upper/population)*1000), alpha = 0.2, colour = NA) +
    geom_line(size = 1) +
    geom_point(data = filter(data_df, region == region_selected), 
               aes(x=as.Date(time), y = (value/population)*1000, color = "Data", fill = "Data"), alpha = 0.3) +
    geom_vline(xintercept = as.Date('2021-03-31'), alpha = 0.8, color = 'black') +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = label_cols, lty = 'dashed') +
    scale_colour_manual(values = c("Factual" = "#1b9e77",
                                   "Counterfactual_principal" = "#d95f02",
                                   "Counterfactual_low_migration" = "#0099B4FF",
                                   "Counterfactual_high_migration" = "#925E9FFF",
                                   "Data" = "#AD002AFF"),
                        breaks = c("Factual", "Counterfactual_principal",
                                   "Counterfactual_low_migration",
                                   "Counterfactual_high_migration", "Data"),
                        labels = c("2019 baseline", "2047 central \nprojection", 
                                   "2047 low migration \nprojection",
                                   "2047 high migration \nprojection", "Data")) +
    scale_fill_manual(values = c("Factual" = "#1b9e77",
                                 "Counterfactual_principal" = "#d95f02",
                                 "Counterfactual_low_migration" = "#0099B4FF",
                                 "Counterfactual_high_migration" = "#925E9FFF",
                                 "Data" = "#AD002AFF"),
                      breaks = c("Factual", "Counterfactual_principal",
                                 "Counterfactual_low_migration",
                                 "Counterfactual_high_migration", "Data"),
                      labels = c("2019 baseline", "2047 central \nprojection", 
                                 "2047 low migration \nprojection",
                                 "2047 high migration \nprojection", "Data")) +
    labs(
      x = "Time",
      y = sprintf("%s (per 1000 people)", outcome_label),
      colour = "Scenario:",
      fill = "Scenario:",
      title = sprintf("%s - %s", outcome_label, region_selected)
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%b %Y")
}


#######
# Panel 2, cumulative
plot_cumulative <- function(sim_df, outcome_label, region_selected, y_nudge_value = -50) {
  
  plot_df <- sim_df |>
    dplyr::filter(region == region_selected) |>
    dplyr::group_by(version) |>
    dplyr::arrange(time, .by_group = TRUE) |>
    dplyr::mutate(cum_mean = cumsum(mean)) |>
    dplyr::ungroup()
  
  # Order versions so the largest cumulative curve is drawn first (back),
  # and the smallest is drawn last (front)
  version_order <- plot_df |>
    dplyr::group_by(version) |>
    dplyr::summarise(final_cum = max(cum_mean), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(final_cum)) |>
    dplyr::pull(version)
  
  plot_df$version <- factor(plot_df$version, levels = version_order)
  
  label_df <- plot_df |>
    dplyr::group_by(version) |>
    dplyr::filter(time == max(time)) |>
    dplyr::ungroup()
  
  ggplot(
    plot_df,
    aes(
      x = sircovid::sircovid_date_as_date(time),
      y = cum_mean/1000,
      fill = version,
      colour = version,
      group = version
    )
  ) +
    geom_area(position = "identity") +
    geom_label(
      data = label_df,
      aes(
        x = sircovid::sircovid_date_as_date(time),
        y = cum_mean/1000,
        label = round(cum_mean, 0),
        colour = version
      ),
      fill = "white",
      size = 3,
      #label.size = NA,   # removes border
      show.legend = FALSE,
      hjust = -0.1,
      nudge_y = y_nudge_value,
      nudge_x = -50
    ) +
    #geom_line(linewidth = 1) +
    scale_colour_manual(
      values = c(
        "Factual" = "#1b9e77",
        "Counterfactual_principal" = "#d95f02",
        "Counterfactual_low_migration" = "#0099B4FF",
        "Counterfactual_high_migration" = "#925E9FFF"
      ),
      breaks = c(
        "Factual",
        "Counterfactual_principal",
        "Counterfactual_low_migration",
        "Counterfactual_high_migration"
      ),
      labels = c(
        "2019 baseline",
        "2047 central \nprojection",
        "2047 low migration \nprojection",
        "2047 high migration \nprojection"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Factual" = "#1b9e77",
        "Counterfactual_principal" = "#d95f02",
        "Counterfactual_low_migration" = "#0099B4FF",
        "Counterfactual_high_migration" = "#925E9FFF"
      ),
      breaks = c(
        "Factual",
        "Counterfactual_principal",
        "Counterfactual_low_migration",
        "Counterfactual_high_migration"
      ),
      labels = c(
        "2019 baseline",
        "2047 central \nprojection",
        "2047 low migration \nprojection",
        "2047 high migration \nprojection"
      )
    ) +
    labs(
      x = "Time",
      y = paste0("Cumulative ", outcome_label),
      colour = "Scenario:",
      fill = "Scenario:",
      title = sprintf("%s - %s", outcome_label, region_selected)
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%b %Y")
}

plot_cumulative_per_capita <- function(sim_df, outcome_label, region_selected, y_nudge_value = 0) {
  
  plot_df <- sim_df |>
    dplyr::filter(region == region_selected) |>
    dplyr::group_by(version) |>
    dplyr::arrange(time, .by_group = TRUE) |>
    dplyr::mutate(cum_mean = cumsum(mean)) |>
    dplyr::ungroup()
  
  # Order versions so the largest cumulative curve is drawn first (back),
  # and the smallest is drawn last (front)
  version_order <- plot_df |>
    dplyr::group_by(version) |>
    dplyr::summarise(final_cum = max(cum_mean), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(final_cum)) |>
    dplyr::pull(version)
  
  plot_df$version <- factor(plot_df$version, levels = version_order)
  
  label_df <- plot_df |>
    dplyr::group_by(version) |>
    dplyr::filter(time == max(time)) |>
    dplyr::ungroup()
  
  ggplot(
    plot_df,
    aes(
      x = sircovid::sircovid_date_as_date(time),
      y = cum_mean/population,
      fill = version,
      colour = version,
      group = version
    )
  ) +
    geom_area(position = "identity") +
    geom_label(
      data = label_df,
      aes(
        x = sircovid::sircovid_date_as_date(time),
        y = cum_mean/population,
        label = round(cum_mean, 0),
        colour = version
      ),
      fill = "white",
      size = 3,
      #label.size = NA,   # removes border
      show.legend = FALSE,
      hjust = -0.1,
      nudge_y = y_nudge_value,
      nudge_x = -50
    ) +
    #geom_line(linewidth = 1) +
    scale_colour_manual(
      values = c(
        "Factual" = "#1b9e77",
        "Counterfactual_principal" = "#d95f02",
        "Counterfactual_low_migration" = "#0099B4FF",
        "Counterfactual_high_migration" = "#925E9FFF"
      ),
      breaks = c(
        "Factual",
        "Counterfactual_principal",
        "Counterfactual_low_migration",
        "Counterfactual_high_migration"
      ),
      labels = c(
        "2019 baseline",
        "2047 central \nprojection",
        "2047 low migration \nprojection",
        "2047 high migration \nprojection"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Factual" = "#1b9e77",
        "Counterfactual_principal" = "#d95f02",
        "Counterfactual_low_migration" = "#0099B4FF",
        "Counterfactual_high_migration" = "#925E9FFF"
      ),
      breaks = c(
        "Factual",
        "Counterfactual_principal",
        "Counterfactual_low_migration",
        "Counterfactual_high_migration"
      ),
      labels = c(
        "2019 baseline",
        "2047 central \nprojection",
        "2047 low migration \nprojection",
        "2047 high migration \nprojection"
      )
    ) +
    labs(
      x = "Time",
      y = paste0("Cumulative ", outcome_label, " (per capita)"),
      colour = "Scenario:",
      fill = "Scenario:",
      title = sprintf("%s - %s", outcome_label, region_selected)
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%b %Y")
}


##############
#The stacked symptomatic cases plot:
plot_sympt_cases_age_stack <- function(sim_df, region_selected, version_selected = NULL, per_capita = FALSE) {
  
  age_levels <- c(
    "sympt_cases_under15_inc",
    "sympt_cases_15_24_inc",
    "sympt_cases_25_49_inc",
    "sympt_cases_50_64_inc",
    "sympt_cases_65_79_inc",
    "sympt_cases_80_plus_inc"
  )
  
  age_labels <- c(
    "sympt_cases_under15_inc" = "Under 15",
    "sympt_cases_15_24_inc"   = "15-24",
    "sympt_cases_25_49_inc"   = "25-49",
    "sympt_cases_50_64_inc"   = "50-64",
    "sympt_cases_65_79_inc"   = "65-79",
    "sympt_cases_80_plus_inc" = "80+"
  )
  
  plot_df <- sim_df |>
    dplyr::filter(
      region == region_selected,
      output_type %in% age_levels
    ) |>
    dplyr::mutate(
      age_group = factor(age_labels[output_type], levels = age_labels[age_levels]),
      value = if (per_capita) mean / population else mean/1000
    )
  
  if (!is.null(version_selected)) {
    plot_df <- plot_df |>
      dplyr::filter(version == version_selected)
  }
  
  ggplot(
    plot_df,
    aes(
      x = sircovid::sircovid_date_as_date(time),
      y = value,
      fill = age_group,
      group = age_group
    )
  ) +
    geom_area() +
    scale_fill_manual(
      values = c(
        #"#00468BFF" "#ED0000FF" "#42B540FF" "#0099B4FF" "#925E9FFF" "#FDAF91FF" "#AD002AFF" "#ADB6B6FF"
        "Under 15" = "#00468BFF",#"#1b9e77",
        "15-24"    = "#ED0000FF",#"#d95f02",
        "25-49"    = "#42B540FF",#"#7570b3",
        "50-64"    = "#0099B4FF",#"#e7298a",
        "65-79"    = "#925E9FFF",#"#66a61e",
        "80+"      = "#FDAF91FF" #"#e6ab02"
      ),
      breaks = c("Under 15", "15-24", "25-49", "50-64", "65-79", "80+"),
      labels = c("Under 15", "15-24", "25-49", "50-64", "65-79", "80+")
    ) +
    labs(
      x = "Time",
      y = if (per_capita) {
        "Symptomatic COVID-19 Cases (Per Capita)"
      } else {
        "Symptomatic COVID-19 Cases (Thousands)"
      },
      fill = "Age Group:",
      title = if (!is.null(version_selected)) {
        sprintf("Symptomatic COVID-19 cases by age - %s (%s)", region_selected, version_selected)
      } else {
        sprintf("Symptomatic COVID-19 cases by age - %s", region_selected)
      }
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%b %Y")
}


######################
#Ratio plot
plot_population_adjusted_case_ratio <- function(sim_df, region_selected) {
  
  age_labels <- c(
    "sympt_cases_under15_inc" = "Under 15",
    "sympt_cases_15_24_inc"   = "15-24",
    "sympt_cases_25_49_inc"   = "25-49",
    "sympt_cases_50_64_inc"   = "50-64",
    "sympt_cases_65_79_inc"   = "65-79",
    "sympt_cases_80_plus_inc" = "80+"
  )
  
  plot_df <- sim_df |>
    dplyr::filter(region == region_selected) |>
    dplyr::mutate(
      age_group = factor(
        age_labels[output_type],
        levels = c("Under 15", "15-24", "25-49", "50-64", "65-79", "80+")
      ),
      pop_adjusted_case_ratio = mean_ratio_vs_factual / population_ratio_vs_factual
    )
  
  ggplot(
    plot_df,
    aes(
      x = sircovid::sircovid_date_as_date(time),
      y = pop_adjusted_case_ratio,
      colour = age_group,
      group = age_group
    )
  ) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1) +
    scale_colour_manual(
      values = c(
        "Under 15" = "#00468BFF",#"#1b9e77",
        "15-24"    = "#ED0000FF",#"#d95f02",
        "25-49"    = "#42B540FF",#"#7570b3",
        "50-64"    = "#0099B4FF",#"#e7298a",
        "65-79"    = "#925E9FFF",#"#66a61e",
        "80+"      = "#FDAF91FF" #"#e6ab02"
      ),
      breaks = c("Under 15", "15-24", "25-49", "50-64", "65-79", "80+"),
      labels = c("Under 15", "15-24", "25-49", "50-64", "65-79", "80+")
    ) +
    labs(
      x = "Time",
      y = "Cases ratio relative to factual,\nadjusted for population ratio",
      colour = "Age group:",
      title = sprintf(
        "Population-adjusted symptomatic case ratio - %s",
        region_selected
      )
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%b %Y")
}

# plot_population_adjusted_case_ratio(
#   sim_df = p4_sim_data,
#   region_selected = "england"
# )
