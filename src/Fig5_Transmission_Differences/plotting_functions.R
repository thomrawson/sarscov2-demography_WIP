### All Fig5 plotting functions
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
                                   "Counterfactual_2047" = "#d95f02",
                                   "Counterfactual_low_migration" = "#0099B4FF",
                                   "Counterfactual_high_migration" = "#925E9FFF",
                                   "Data" = "#AD002AFF"),
                        breaks = c("Factual", "Counterfactual_2047",
                                   "Counterfactual_low_migration",
                                   "Counterfactual_high_migration", "Data"),
                        labels = c("2020 baseline", "2047 central \nprojection", 
                                   "2047 low migration \nprojection",
                                   "2047 high migration \nprojection", "Data")) +
    scale_fill_manual(values = c("Factual" = "#1b9e77",
                                 "Counterfactual_2047" = "#d95f02",
                                 "Counterfactual_low_migration" = "#0099B4FF",
                                 "Counterfactual_high_migration" = "#925E9FFF",
                                 "Data" = "#AD002AFF"),
                      breaks = c("Factual", "Counterfactual_2047",
                                 "Counterfactual_low_migration",
                                 "Counterfactual_high_migration", "Data"),
                      labels = c("2020 baseline", "2047 central \nprojection", 
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
                                   "Counterfactual_2047" = "#d95f02",
                                   "Counterfactual_low_migration" = "#0099B4FF",
                                   "Counterfactual_high_migration" = "#925E9FFF",
                                   "Data" = "#AD002AFF"),
                        breaks = c("Factual", "Counterfactual_2047",
                                   "Counterfactual_low_migration",
                                   "Counterfactual_high_migration", "Data"),
                        labels = c("2020 baseline", "2047 central \nprojection", 
                                   "2047 low migration \nprojection",
                                   "2047 high migration \nprojection", "Data")) +
    scale_fill_manual(values = c("Factual" = "#1b9e77",
                                 "Counterfactual_2047" = "#d95f02",
                                 "Counterfactual_low_migration" = "#0099B4FF",
                                 "Counterfactual_high_migration" = "#925E9FFF",
                                 "Data" = "#AD002AFF"),
                      breaks = c("Factual", "Counterfactual_2047",
                                 "Counterfactual_low_migration",
                                 "Counterfactual_high_migration", "Data"),
                      labels = c("2020 baseline", "2047 central \nprojection", 
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
        "Counterfactual_2047" = "#d95f02",
        "Counterfactual_low_migration" = "#0099B4FF",
        "Counterfactual_high_migration" = "#925E9FFF"
      ),
      breaks = c(
        "Factual",
        "Counterfactual_2047",
        "Counterfactual_low_migration",
        "Counterfactual_high_migration"
      ),
      labels = c(
        "2020 baseline",
        "2047 central \nprojection",
        "2047 low migration \nprojection",
        "2047 high migration \nprojection"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Factual" = "#1b9e77",
        "Counterfactual_2047" = "#d95f02",
        "Counterfactual_low_migration" = "#0099B4FF",
        "Counterfactual_high_migration" = "#925E9FFF"
      ),
      breaks = c(
        "Factual",
        "Counterfactual_2047",
        "Counterfactual_low_migration",
        "Counterfactual_high_migration"
      ),
      labels = c(
        "2020 baseline",
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
        "Counterfactual_2047" = "#d95f02",
        "Counterfactual_low_migration" = "#0099B4FF",
        "Counterfactual_high_migration" = "#925E9FFF"
      ),
      breaks = c(
        "Factual",
        "Counterfactual_2047",
        "Counterfactual_low_migration",
        "Counterfactual_high_migration"
      ),
      labels = c(
        "2020 baseline",
        "2047 central \nprojection",
        "2047 low migration \nprojection",
        "2047 high migration \nprojection"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Factual" = "#1b9e77",
        "Counterfactual_2047" = "#d95f02",
        "Counterfactual_low_migration" = "#0099B4FF",
        "Counterfactual_high_migration" = "#925E9FFF"
      ),
      breaks = c(
        "Factual",
        "Counterfactual_2047",
        "Counterfactual_low_migration",
        "Counterfactual_high_migration"
      ),
      labels = c(
        "2020 baseline",
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
        "Symptomatic COVID-19 Cases \n(Thousands)"
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
#Stacked cases proportion plot
plot_sympt_cases_age_proportion <- function(sim_df, region_selected, version_selected = NULL) {
  
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
    )
  
  if (!is.null(version_selected)) {
    plot_df <- plot_df |>
      dplyr::filter(version == version_selected)
  }
  
  if (dplyr::n_distinct(plot_df$version) > 1) {
    stop("Multiple versions found. Please supply `version_selected = ...`.")
  }
  
  plot_df <- plot_df |>
    dplyr::mutate(
      age_group = factor(age_labels[output_type], levels = age_labels[age_levels])
    ) |>
    dplyr::group_by(time) |>
    dplyr::mutate(
      total_cases = sum(mean, na.rm = TRUE),
      proportion = dplyr::if_else(total_cases > 0, mean / total_cases, 0)
    ) |>
    dplyr::ungroup()
  
  ggplot(
    plot_df,
    aes(
      x = sircovid::sircovid_date_as_date(time),
      y = proportion,
      fill = age_group,
      group = age_group
    )
  ) +
    geom_area(colour = NA, stat = "identity") +
    scale_fill_manual(
      values = c(
        "Under 15" = "#00468BFF",
        "15-24"    = "#ED0000FF",
        "25-49"    = "#42B540FF",
        "50-64"    = "#0099B4FF",
        "65-79"    = "#925E9FFF",
        "80+"      = "#FDAF91FF"
      ),
      breaks = c("Under 15", "15-24", "25-49", "50-64", "65-79", "80+"),
      labels = c("Under 15", "15-24", "25-49", "50-64", "65-79", "80+")
    ) +
    scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      limits = c(0, 1.01),
      expand = c(0,0)
    ) +
    labs(
      x = "Time",
      y = "Proportion of New Daily Cases",
      fill = "Age Group:",
      title = if (!is.null(version_selected)) {
        sprintf("Proportion of symptomatic COVID-19 cases by age - %s (%s)",
                region_selected, version_selected)
      } else {
        sprintf("Proportion of symptomatic COVID-19 cases by age - %s",
                region_selected)
      }
    ) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%b %Y",
                 expand = c(0,0))
}


################

######################
#Ratio plot
plot_population_adjusted_case_ratio <- function(sim_df, region_selected) {
  
  age_labels <- c(
    "sympt_cases_0_49_inc"   = "0-49",
    "sympt_cases_50_64_inc"   = "50-64",
    "sympt_cases_65_79_inc"   = "65-79",
    "sympt_cases_80_plus_inc" = "80+"
  )
  
  plot_df <- sim_df |>
    dplyr::filter(region == region_selected) |>
    dplyr::mutate(
      age_group = factor(
        age_labels[output_type],
        levels = c("0-49", "50-64", "65-79", "80+")
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
        "0-49" = "#00468BFF",#"#1b9e77",
        "50-64"    = "#ED0000FF",#"#d95f02",
        "65-79"    = "#42B540FF",#"#7570b3",
        #"50-64"    = "#0099B4FF",#"#e7298a",
        "80+"    = "#925E9FFF"#"#66a61e",
        #"80+"      = "#FDAF91FF" #"#e6ab02"
      ),
      breaks = c("0-49", "50-64", "65-79", "80+"),
      labels = c("0-49", "50-64", "65-79", "80+")
    ) +
    labs(
      x = "Time",
      y = "Ratio of difference in daily cases to \n difference in population size",
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

########################################
#Cumulative version of the above:
plot_population_adjusted_cumulative_case_ratio <- function(sim_df, region_selected) {
  
  age_labels <- c(
    "sympt_cases_0_49_inc"   = "0-49",
    "sympt_cases_50_64_inc"   = "50-64",
    "sympt_cases_65_79_inc"   = "65-79",
    "sympt_cases_80_plus_inc" = "80+"
  )
  
  plot_df <- sim_df |>
    dplyr::filter(region == region_selected) |>
    dplyr::mutate(
      age_group = factor(
        age_labels[output_type],
        levels = c("0-49", "50-64", "65-79", "80+")
      )
    )
  
  ggplot(
    plot_df,
    aes(
      x = sircovid::sircovid_date_as_date(time),
      y = cum_pop_adjusted_case_ratio,
      colour = age_group,
      group = age_group
    )
  ) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1) +
    scale_colour_manual(
      values = c(
        "0-49" = "#00468BFF",#"#1b9e77",
        "50-64"    = "#ED0000FF",#"#d95f02",
        "65-79"    = "#42B540FF",#"#7570b3",
        #"50-64"    = "#0099B4FF",#"#e7298a",
        "80+"    = "#925E9FFF"#"#66a61e",
        #"80+"      = "#FDAF91FF" #"#e6ab02"
      ),
      breaks = c("0-49", "50-64", "65-79", "80+"),
      labels = c("0-49", "50-64", "65-79", "80+")
    ) +
    labs(
      x = "Time",
      y = "Ratio of difference in cumulative \ncases to difference in population size",
      colour = "Age group:",
      title = sprintf(
        "Population-adjusted cumulative symptomatic case ratio - %s",
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
########################################
#Rolling cumulative version of the above:
plot_population_adjusted_rolling_cumulative_case_ratio <- function(sim_df, region_selected) {
  
  age_labels <- c(
    #"sympt_cases_under15_inc" = "Under 15",
    #"sympt_cases_15_24_inc"   = "15-24",
    "sympt_cases_0_49_inc"   = "0-49",
    "sympt_cases_50_64_inc"   = "50-64",
    "sympt_cases_65_79_inc"   = "65-79",
    "sympt_cases_80_plus_inc" = "80+"
  )
  
  plot_df <- sim_df |>
    dplyr::filter(region == region_selected) |>
    dplyr::mutate(
      age_group = factor(
        age_labels[output_type],
        levels = c("0-49", "50-64", "65-79", "80+")
      )
    )
  
  ggplot(
    plot_df,
    aes(
      x = sircovid::sircovid_date_as_date(time),
      y = roll_pop_adjusted_case_ratio,
      colour = age_group,
      group = age_group
    )
  ) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1) +
    scale_colour_manual(
      values = c(
        "0-49" = "#00468BFF",#"#1b9e77",
        "50-64"    = "#ED0000FF",#"#d95f02",
        "65-79"    = "#42B540FF",#"#7570b3",
        #"50-64"    = "#0099B4FF",#"#e7298a",
        "80+"    = "#925E9FFF"#"#66a61e",
        #"80+"      = "#FDAF91FF" #"#e6ab02"
      ),
      breaks = c("0-49", "50-64", "65-79", "80+"),
      labels = c("0-49", "50-64", "65-79", "80+")
    ) +
    labs(
      x = "Time",
      y = "Ratio of difference in rolling cumulative \ncases to difference in population size",
      colour = "Age group:",
      title = sprintf(
        "Population-adjusted 3-month rolling cumulative symptomatic case ratio - %s",
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

########################################
#Rolling cumulative version for hospitalisations:
plot_population_adjusted_rolling_cumulative_case_ratio_hosps <- function(sim_df, region_selected) {

  age_labels <- c(
    "all_admission_0_9_inc" = "0-9",
    "all_admission_10_19_inc"   = "10-19",
    "all_admission_20_29_inc"   = "20-29",
    "all_admission_30_39_inc"   = "30-39",
    "all_admission_40_49_inc"   = "40-49",
    "all_admission_50_59_inc"   = "50-59",
    "all_admission_60_69_inc"   = "60-69",
    "all_admission_70_79_inc"   = "70-79",
    "all_admission_80_plus_inc" = "80+"
  )
  
  plot_df <- sim_df |>
    dplyr::filter(region == region_selected) |>
    dplyr::mutate(
      age_group = factor(
        age_labels[output_type],
        levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
      )
    )
  
  ggplot(
    plot_df,
    aes(
      x = sircovid::sircovid_date_as_date(time),
      y = roll_pop_adjusted_case_ratio,
      colour = age_group,
      group = age_group
    )
  ) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1) +
    scale_colour_manual(
      values = c(
        "0-9" = "#00468BFF",
        "10-19"    = "#ED0000FF",
        "20-29"    = "#42B540FF",
        "30-39"    = "#0099B4FF",
        "40-49"    = "#925E9FFF",
        "50-59"    = "#FDAF91FF",
        "60-69"    = "yellow",
        "70-79"    = "hotpink",
        "80+"    = "black"
      ),
      breaks = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
      labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
    ) +
    labs(
      x = "Time",
      y = "Ratio of difference in rolling cumulative \nhospitalisations to difference in population size",
      colour = "Age group:",
      title = sprintf(
        "Population-adjusted 3-month rolling cumulative hospitalisation ratio - %s",
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

########################################
#Rolling cumulative version for deaths:
plot_population_adjusted_rolling_cumulative_case_ratio_deaths <- function(sim_df, region_selected) {
  
  age_labels <- c(
    "deaths_hosp_0_49_inc"    = "0-49",
    "deaths_hosp_50_64_inc"   = "50-64",
    #"deaths_hosp_55_59_inc"   = "55-59",
    #"deaths_hosp_60_64_inc"   = "60-64",
    "deaths_hosp_65_79_inc"   = "65-79",
    #"deaths_hosp_70_74_inc"   = "70-74",
    #"deaths_hosp_75_79_inc"   = "75-79",
    "deaths_hosp_80_plus_inc" = "80+"
  )
  
  plot_df <- sim_df |>
    dplyr::filter(region == region_selected) |>
    dplyr::mutate(
      age_group = factor(
        age_labels[output_type],
        levels = c("0-49", "50-64", "65-79", "80+")
      )
    )
  
  ggplot(
    plot_df,
    aes(
      x = sircovid::sircovid_date_as_date(time),
      y = roll_pop_adjusted_case_ratio,
      colour = age_group,
      group = age_group
    )
  ) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1) +
    scale_colour_manual(
      values = c(
        # "0-49" = "#00468BFF",
        # "50-54"    = "#ED0000FF",
        # "55-59"    = "#42B540FF",
        # "60-64"    = "#0099B4FF",
        # "65-69"    = "#925E9FFF",
        # "70-74"    = "#FDAF91FF",
        # "75-79"    = "yellow",
        # "80+"    = "black"
        "0-49" = "#00468BFF",#"#1b9e77",
        "50-64"    = "#ED0000FF",#"#d95f02",
        "65-79"    = "#42B540FF",#"#7570b3",
        #"50-64"    = "#0099B4FF",#"#e7298a",
        "80+"    = "#925E9FFF"#"#66a61e",
        #"80+"      = "#FDAF91FF" #"#e6ab02"
      ),
      breaks = c("0-49", "50-64", "65-79", "80+"),
      labels = c("0-49", "50-64", "65-79", "80+")
    ) +
    labs(
      x = "Time",
      y = "Ratio of difference in rolling cumulative \ndeaths to difference in population size",
      colour = "Age group:",
      title = sprintf(
        "Population-adjusted 3-month rolling cumulative hospitalisation ratio - %s",
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
