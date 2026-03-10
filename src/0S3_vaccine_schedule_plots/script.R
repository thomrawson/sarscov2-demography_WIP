
source("global_util.R")
## Load data:
base_params <- readRDS("base.rds")
base_params$england <- list(
  population = Reduce(`+`, lapply(base_params, `[[`, "population"))
)
baseline_df <- readRDS("baseline_vaccine_simulations.rds")
counterfactual_df <- readRDS("vaccine_simulations.rds")
baseline_vacc_schedules <- readRDS("baseline_vaccine_schedules.rds")
counterfactual_vacc_schedules <- readRDS("vaccine_schedules.rds")

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
###########################################################################
## First plot will be the final uptake of each vaccine dose on final day of analysis
## By age group, and comparing baseline with counterfactual
final_date_vaccine_df <- filter(vaccine_df, time == 785)

scenario_cols <- c(
  "Factual" = "#2B83BA",       # softened blue
  "Counterfactual" = "#E69F00"  # orange
)

# common theme
my_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 6)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),     # remove vertical gridlines
    panel.grid.major.y = element_line(color = "gray90"),
    plot.margin = margin(t = 6, r = 8, b = 6, l = 8)
  )

dir.create("final_vaccine_plots")
dir.create("final_vaccine_proportion_plots")
for(reg in regions){
  for(vacc in vaccination_strata){
    p1 <- 
      filter(final_date_vaccine_df, region %in% reg) %>%
      filter(vaccine_strata %in% vacc) %>%
      filter(! age_group %in% c("CHW", "CHR")) %>%
      ggplot(aes(x = age_group, y = mean_cum_doses/1000, fill = version)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.86, colour = NA) +
      scale_fill_manual(values = scenario_cols,
                        labels = c("Factual" = "2019 baseline", 
                                   "Counterfactual" = "2047 vaccine\n deployment")) +
      #scale_y_continuous(labels = label_number(accuracy = 1, suffix = " T"), expand = c(0,0)) +
      scale_x_discrete(expand = c(0,0)) +
      labs(title = sprintf("Number of vaccine doses - %s - %s", vacc, reg),
           x = "Age group", y = "Doses (thousands)", fill = "Population Scenario:") +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "left")) +
      my_theme  +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1 ),    # hide x labels on top plot
        # axis.ticks.x = element_blank(),
        legend.position = "bottom")
    
    p2 <- 
      filter(final_date_vaccine_df, region %in% reg) %>%
      filter(vaccine_strata %in% vacc) %>%
      filter(! age_group %in% c("CHW", "CHR")) %>%
      ggplot(aes(x = age_group, y = mean_cum_doses/population, fill = version)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.86, colour = NA) +
      scale_fill_manual(values = scenario_cols,
                        labels = c("Factual" = "2019 baseline", 
                                   "Counterfactual" = "2047 vaccine\n deployment")) +
      #scale_y_continuous(labels = label_number(accuracy = 1, suffix = " T"), expand = c(0,0)) +
      scale_x_discrete(expand = c(0,0)) +
      labs(title = sprintf("Proportion of people vaccinated - %s - %s", vacc, reg),
           x = "Age group", y = "People vaccinated (% of age group)", fill = "Population Scenario:") +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "left")) +
      my_theme  +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1 ),    # hide x labels on top plot
        # axis.ticks.x = element_blank(),
        legend.position = "bottom")
    
    ggsave(
      filename = sprintf("final_vaccine_plots/%s_%s.png", reg, vacc),
      plot = p1,
      width = 10,      # inches
      height = 8,      # inches
      dpi = 320        # high resolution
    )
    ggsave(
      filename = sprintf("final_vaccine_proportion_plots/%s_%s.png", reg, vacc),
      plot = p2,
      width = 10,      # inches
      height = 8,      # inches
      dpi = 320        # high resolution
    )
  }
}

# Now, we plot the uptake over time for each region, vaccine strata, AND age group
dir.create("vaccines_over_time")
dir.create("vaccines_proportion_over_time")
for(reg in regions){
  dir.create(sprintf("vaccines_over_time/vaccines_over_time_%s", reg))
  dir.create(sprintf("vaccines_proportion_over_time/vaccines_proportion_over_time_%s", reg))
}

for(reg in regions){
  for(vacc in vaccination_strata){
    for(age in age_levels[1:17]){
      p1 <- 
        filter(vaccine_df, region %in% reg) %>%
        filter(vaccine_strata %in% vacc) %>%
        filter(age_group %in% age) %>%
        filter(time > 200) %>%
        ggplot(aes(x = sircovid::sircovid_date_as_date(time), 
                   y = mean_cum_doses/1000, 
                   color = version, fill = version)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = cri_lower/1000, ymax = cri_upper/1000), alpha = 0.2, colour = NA) +
        scale_fill_manual(values = scenario_cols,
                          labels = c("Factual" = "2019 baseline", 
                                     "Counterfactual" = "2047 vaccine\n deployment")) +
        scale_color_manual(values = scenario_cols,
                          labels = c("Factual" = "2019 baseline", 
                                     "Counterfactual" = "2047 vaccine\n deployment"))  +
        scale_x_date(
          date_breaks = "1 month",
          date_labels = "%b %y"
        ) +
        labs(title = sprintf("Vaccines given - %s - %s - %s", vacc, age, reg),
             x = "Date", y = "Doses (thousands)", fill = "Population Scenario:",
             color = "Population Scenario:") +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "left")) +
        my_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2 ),
              legend.position = "bottom",
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.length.x = unit(3, "pt"))
      
      ggsave(
        filename = sprintf("vaccines_over_time/vaccines_over_time_%s/%s_%s.png", reg, vacc, age),
        plot = p1,
        width = 10,      # inches
        height = 8,      # inches
        dpi = 320        # high resolution
      )
      
      p2 <- 
        filter(vaccine_df, region %in% reg) %>%
        filter(vaccine_strata %in% vacc) %>%
        filter(age_group %in% age) %>%
        filter(time > 200) %>%
        ggplot(aes(x = sircovid::sircovid_date_as_date(time), 
                   y = mean_cum_doses/population, 
                   color = version, fill = version)) +
        geom_line(size = 1.1) +
        geom_ribbon(aes(ymin = cri_lower/population, ymax = cri_upper/population), alpha = 0.2, colour = NA) +
        scale_fill_manual(values = scenario_cols,
                          labels = c("Factual" = "2019 baseline", 
                                     "Counterfactual" = "2047 vaccine\n deployment")) +
        scale_color_manual(values = scenario_cols,
                           labels = c("Factual" = "2019 baseline", 
                                      "Counterfactual" = "2047 vaccine\n deployment"))  +
        scale_x_date(
          date_breaks = "1 month",
          date_labels = "%b %y"
        ) +
        labs(title = sprintf("Vaccines given - %s - %s - %s", vacc, age, reg),
             x = "Date", y = "Proportion Vaccinated (%)", fill = "Population Scenario:",
             color = "Population Scenario:") +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "left")) +
        my_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2 ),
              legend.position = "bottom",
              axis.ticks.x = element_line(color = "black"),
              axis.ticks.length.x = unit(3, "pt"))
      
      ggsave(
        filename = sprintf("vaccines_proportion_over_time/vaccines_proportion_over_time_%s/%s_%s.png", reg, vacc, age),
        plot = p2,
        width = 10,      # inches
        height = 8,      # inches
        dpi = 320        # high resolution
      )
      
    }
  }
}

#######
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
                   "2021-01-01",
                                            "2021-02-01", "2021-03-01",
                                            "2021-04-01", "2021-05-01",
                                            "2021-06-01", "2021-07-01",
                                            "2021-08-01", "2021-09-01",
                                            "2021-10-01", "2021-11-01",
                                            "2021-12-01", "2022-01-01",
                                            "2022-02-01", "2022-03-01")

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
  filter(age_group == "total") %>% 
  filter(region == "england") %>%
  mutate(time = as.Date(time)) %>%
  # compute proportion in each stage (mean_in_stage must already exist)
  mutate(prop_in_stage = mean_in_stage / population) %>%
  arrange(time, version, vaccine_strata)

# ---- plotting ----
dir.create("england_vaccine_strata")
# stacked bar plot
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

ggplot(plot_df_nudged,
       aes(x = time_nudge,
           y = prop_in_stage,
           fill = vaccine_strata,
           alpha = version,
           linetype = version)) +
  geom_col(
    colour = "grey20",   # border colour for readability
    linewidth = 0.3,
    width = 12           # bar width in days; tweak as needed
  ) +
  # show original month ticks in the x axis (use the original `time` values)
  scale_x_date(
    breaks = unique(plot_df_nudged$time),
    labels = function(x) format(x, "%b %Y"),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_fill_manual(values = my_palette) +
  scale_alpha_manual(name = "Model", values = alpha_map) +
  scale_linetype_manual(name = "Model", values = linetype_map,
                        guide = guide_legend(override.aes = list(fill = "grey70"))) +
  labs(
    title = "Total England population proportion in each vaccine strata",
    x = "Date (first of month)",
    y = "Proportion of population",
    fill = "Vaccine strata"
  ) +
  theme_minimal(base_size = 22) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    legend.key.size = unit(0.9, "lines")
  ) -> england_plot

ggsave(
  filename = sprintf("england_vaccine_strata/vaccines_%s.png", "total"),
  plot = england_plot,
  width = 16,      # inches
  height = 10,      # inches
  dpi = 320        # high resolution
)

# Now output it for each individual age group
for(i_age in age_levels){
  ## Pluck out a subset to plot:
  plot_df <- vaccine_current_df_extended %>% 
    # choose which age_group to plot
    filter(age_group == i_age) %>% 
    filter(region == "england") %>%
    mutate(time = as.Date(time)) %>%
    # compute proportion in each stage (mean_in_stage must already exist)
    mutate(prop_in_stage = mean_in_stage / population) %>%
    arrange(time, version, vaccine_strata)
  
  plot_df_nudged <- plot_df %>%
    mutate(
      time_nudge = case_when(
        version == "Factual"       ~ time - days(nudge_days),
        version == "Counterfactual"~ time + days(nudge_days),
        TRUE                       ~ time  # fallback if other labels exist
      )
    )
  
  ggplot(plot_df_nudged,
         aes(x = time_nudge,
             y = prop_in_stage,
             fill = vaccine_strata,
             alpha = version,
             linetype = version)) +
    geom_col(
      colour = "grey20",   # border colour for readability
      linewidth = 0.3,
      width = 12           # bar width in days; tweak as needed
    ) +
    # show original month ticks in the x axis (use the original `time` values)
    scale_x_date(
      breaks = unique(plot_df_nudged$time),
      labels = function(x) format(x, "%b %Y"),
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
    scale_fill_manual(values = my_palette) +
    scale_alpha_manual(name = "Model", values = alpha_map) +
    scale_linetype_manual(name = "Model", values = linetype_map,
                          guide = guide_legend(override.aes = list(fill = "grey70"))) +
    labs(
      title = sprintf("England population (age %s) proportion in each vaccine strata", i_age),
      x = "Date (first of month)",
      y = "Proportion of population",
      fill = "Vaccine strata"
    ) +
    theme_minimal(base_size = 22) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      legend.position = "right",
      legend.key.size = unit(0.9, "lines")
    ) -> p
  
  ggsave(
    filename = sprintf("england_vaccine_strata/vaccines_%s.png", i_age),
    plot = p,
    width = 16,      # inches
    height = 10,      # inches
    dpi = 320        # high resolution
  )
  
}

#########
## Now, we want a "mean population protection" VE % over time for each scenario
