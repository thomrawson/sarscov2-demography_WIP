### All Fig4 plotting functions
######################################
#######
#cumulative
plot_cumulative_facet <- function(sim_df, outcome_label, region_selected, y_nudge_value = -50) {
  
  plot_df <- sim_df |>
    dplyr::filter(region == region_selected) |>
    dplyr::group_by(version, vaccine, population_assumptions) |>
    dplyr::arrange(time, .by_group = TRUE) |>
    dplyr::mutate(cum_mean = cumsum(mean)) |>
    dplyr::ungroup()
  
  #Change factor levels:
  plot_df$population_assumptions <- factor(
    plot_df$population_assumptions,
    levels = c("ONS_NHS_region_low_migration", "ONS_NHS_region_principal", "ONS_NHS_region_high_migration"),
    labels = c("Low migration", "Central migration", "High migration")
  )
  plot_df$vaccine <- factor(
    plot_df$vaccine,
    levels = c("baseline", "baseline_scaled_up_and_reallocated", "baseline_scaled_up", "1_month_earlier"),
    labels = c("2019 vaccinations", "Scaled and reallocated", "Scaled by age", "1 month earlier")
  )
  
  # Order versions so the largest cumulative curve is drawn first (back),
  # and the smallest is drawn last (front)
  version_order <- plot_df |>
    dplyr::group_by(version) |>
    dplyr::summarise(final_cum = max(cum_mean), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(final_cum)) |>
    dplyr::pull(version)
  
  plot_df$version <- factor(plot_df$version, levels = version_order)
  
  label_df <- plot_df |>
    dplyr::group_by(version, vaccine, population_assumptions) |>
    dplyr::filter(time == max(time)) |>
    dplyr::ungroup()
  label_df <- label_df |>
    dplyr::group_by(vaccine, population_assumptions) |>
    dplyr::arrange(cum_mean) |>
    dplyr::mutate(y_offset = seq(-110, 110, length.out = dplyr::n()),
                  y_offset = ifelse(vaccine == "baseline", 0, y_offset))
  
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
        y = cum_mean/1000 + y_offset,
        label = round(cum_mean, 0),
        colour = version
      ),
      fill = "white",
      size = 3,
      #label.size = NA,   # removes border
      show.legend = FALSE,
      hjust = -0.1,
      nudge_y = y_nudge_value,
      nudge_x = -100
    ) +
    #geom_line(linewidth = 1) +
    scale_colour_manual(values = c("2019" = "#1b9e77",
                                   "2047" = "#d95f02",
                                   "2037" = "#0099B4FF",
                                   "2027" = "#925E9FFF",
                                   "Data" = "#AD002AFF"),
                        breaks = c("2019", "2047",
                                   "2037",
                                   "2027", "Data"),
                        labels = c("2019 baseline", "2047 \nprojection", 
                                   "2037 \nprojection",
                                   "2027 \nprojection", "Data")) +
    scale_fill_manual(values = c("2019" = "#1b9e77",
                                 "2047" = "#d95f02",
                                 "2037" = "#0099B4FF",
                                 "2027" = "#925E9FFF",
                                 "Data" = "#AD002AFF"),
                      breaks = c("2019", "2047",
                                 "2037",
                                 "2027", "Data"),
                      labels = c("2019 baseline", "2047 \nprojection", 
                                 "2037 \nprojection",
                                 "2027 \nprojection", "Data")) +
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
    scale_x_date(date_labels = "%b %Y") +
    facet_grid(vaccine ~ population_assumptions)
}

plot_cumulative_facet_per_capita <- function(sim_df, outcome_label, region_selected, y_nudge_value = 0) {
  
  plot_df <- sim_df |>
    dplyr::filter(region == region_selected) |>
    dplyr::group_by(version, vaccine, population_assumptions) |>
    dplyr::arrange(time, .by_group = TRUE) |>
    dplyr::mutate(cum_mean = cumsum(mean)) |>
    dplyr::ungroup()
  
  plot_df$population_assumptions <- factor(
    plot_df$population_assumptions,
    levels = c("ONS_NHS_region_low_migration", "ONS_NHS_region_principal", "ONS_NHS_region_high_migration"),
    labels = c("Low migration", "Central migration", "High migration")
  )
  
  plot_df$vaccine <- factor(
    plot_df$vaccine,
    levels = c("baseline", "baseline_scaled_up_and_reallocated", "baseline_scaled_up"),
    labels = c("2019 vaccinations", "Scaled and reallocated", "Scaled by age")
  )
  
  version_order <- plot_df |>
    dplyr::group_by(version) |>
    dplyr::summarise(final_cum = max(cum_mean), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(final_cum)) |>
    dplyr::pull(version)
  
  plot_df$version <- factor(plot_df$version, levels = version_order)
  
  
  label_df <- plot_df |>
    dplyr::group_by(version, vaccine, population_assumptions) |>
    dplyr::filter(time == max(time)) |>
    dplyr::ungroup()
  # offset
  label_df <- label_df |>
    dplyr::group_by(vaccine, population_assumptions) |>
    dplyr::arrange(cum_mean) |>
    dplyr::mutate(
      y_offset = seq(-0.003, 0.003, length.out = dplyr::n()),  #  scaled for per-capita
      y_offset = ifelse(vaccine == "baseline", 0, y_offset)
    ) |>
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
        y = cum_mean/population + y_offset,
        label = round(cum_mean, 0),
        colour = version
      ),
      fill = "white",
      size = 3,
      #label.size = NA,   # removes border
      show.legend = FALSE,
      hjust = -0.1,
      nudge_y = y_nudge_value,
      nudge_x = -90
    ) +
    #geom_line(linewidth = 1) +
    scale_colour_manual(values = c("2019" = "#1b9e77",
                                   "2047" = "#d95f02",
                                   "2037" = "#0099B4FF",
                                   "2027" = "#925E9FFF",
                                   "Data" = "#AD002AFF"),
                        breaks = c("2019", "2047",
                                   "2037",
                                   "2027", "Data"),
                        labels = c("2019 baseline", "2047 \nprojection", 
                                   "2037 \nprojection",
                                   "2027 \nprojection", "Data")) +
    scale_fill_manual(values = c("2019" = "#1b9e77",
                                 "2047" = "#d95f02",
                                 "2037" = "#0099B4FF",
                                 "2027" = "#925E9FFF",
                                 "Data" = "#AD002AFF"),
                      breaks = c("2019", "2047",
                                 "2037",
                                 "2027", "Data"),
                      labels = c("2019 baseline", "2047 \nprojection", 
                                 "2037 \nprojection",
                                 "2027 \nprojection", "Data")) +
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
    scale_x_date(date_labels = "%b %Y") +
    facet_grid(vaccine ~ population_assumptions)
}

