
source("global_util.R")
## Load data:
base_params <- readRDS("central_base.rds")
base_params$england <- list(
  population = Reduce(`+`, lapply(base_params, `[[`, "population"))
)
baseline_df <- readRDS("baseline_vaccine_simulations.rds")
central_counterfactual_df <- readRDS("central_vaccine_simulations.rds")
pessimistic_counterfactual_df <- readRDS("pessimistic_vaccine_simulations.rds")
optimistic_counterfactual_df <- readRDS("optimistic_vaccine_simulations.rds")
baseline_vacc_schedules <- readRDS("baseline_vaccine_schedules.rds")
central_counterfactual_vacc_schedules <- readRDS("central_vaccine_schedules.rds")
pessimistic_counterfactual_vacc_schedules <- readRDS("pessimistic_vaccine_schedules.rds")
optimistic_counterfactual_vacc_schedules <- readRDS("optimistic_vaccine_schedules.rds")

baseline_df$version <- "Factual"
central_counterfactual_df$version <- "central_Counterfactual"
pessimistic_counterfactual_df$version <- "pessimistic_Counterfactual"
optimistic_counterfactual_df$version <- "optimistic_Counterfactual"

vaccine_df <- rbind(baseline_df, optimistic_counterfactual_df, pessimistic_counterfactual_df, central_counterfactual_df)
regions <- unique(vaccine_df$region)
vaccination_strata <- unique(vaccine_df$vaccine_strata)

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64",
  "65-69", "70-74", "75-79", "80+", "CHR", "CHW")

# Set age levels:
vaccine_df <- mutate(vaccine_df, age_group = factor(age_group, levels = age_levels))
vaccine_df <- filter(vaccine_df, age_group %in% c(
  "0-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64",
  "65-69", "70-74", "75-79", "80+") ) %>%
  filter(vaccine_strata != "SHOULD BE EMPTY" )

england_vaccine_df <- filter(vaccine_df, region == "england")

#write.csv(england_vaccine_df, "england_vaccine_df.csv", row.names = FALSE)
###########################################################################
# Convert the integer day index to a real date. time = 0 corresponds to
# 2020-01-01, the start of the model's fitting period.
england_vaccine_df <- england_vaccine_df %>%
  mutate(date = as.Date("2020-01-01") + time)

# Tidy scenario labels, in the order we want them to appear/be coloured
scenario_levels <- c("Factual", "A) Historic vaccinations", "B) Scaled & reallocated", "C) Scaled by age")
england_vaccine_df <- england_vaccine_df %>%
  mutate(scenario = recode(version,
                           "Factual" = "Factual",
                           "pessimistic_Counterfactual" = "A) Historic vaccinations",
                           "central_Counterfactual" = "B) Scaled & reallocated",
                           "optimistic_Counterfactual" = "C) Scaled by age"
  )) %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

scenario_colours <- c(
  "Factual" = "#5f5e5a",
  "2019 vaccinations" = "#d85a30",
  "Scaled & reallocated" = "#ba7517",
  "Scaled by age" = "#0f6e56"
)
scenario_linetypes <- c(
  "Factual" = "solid",
  "2019 vaccinations" = "22",
  "Scaled & reallocated" = "42",
  "Scaled by age" = "13"
)

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"
)

## ---- Figure A: cumulative doses per 100 population, by broad age band ----

# "Had 1st dose", "Full dose 2 protection" and "Full dose 3 protection" each
# represent a distinct physical dose event (1st, 2nd, booster); the other
# three strata ("Full dose 1 protection", "Waned dose 2/3 protection") are
# downstream protection sub-states of those same dose events and would
# double-count doses if included in this sum.
dose_strata <- c("Had 1st dose", "Full dose 2 protection", "Full dose 3 protection")

broad_band <- function(age_group) {
  case_when(
    age_group %in% c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49") ~ "0-49",
    age_group %in% c("50-54","55-59","60-64") ~ "50-64",
    age_group %in% c("65-69","70-74","75-79") ~ "65-79",
    age_group == "80+" ~ "80+"
  )
}

fig_a_data <- england_vaccine_df %>%
  filter(vaccine_strata %in% dose_strata) %>%
  mutate(age_band = factor(broad_band(age_group), levels = c("0-49","50-64","65-79","80+"))) %>%
  group_by(scenario, age_band, date) %>%
  summarise(
    total_doses = sum(mean_cum_doses),
    total_doses_lower = sum(cri_lower),
    total_doses_upper = sum(cri_upper),
    .groups = "drop"
  )

# Population differs by scenario as well as age group (the counterfactual
# scenarios are run on the larger 2047 projected population), so the lookup
# must be computed per (scenario, age_band) -- NOT collapsed to age_band
# alone, and NOT taken from Factual only.
band_pop_lookup <- england_vaccine_df %>%
  filter(vaccine_strata == dose_strata[1]) %>%
  distinct(scenario, age_group, population) %>%
  mutate(age_band = broad_band(age_group)) %>%
  group_by(scenario, age_band) %>%
  summarise(band_population = sum(population), .groups = "drop")

fig_a_data <- fig_a_data %>%
  left_join(band_pop_lookup, by = c("scenario", "age_band")) %>%
  mutate(
    doses_per_100 = total_doses / band_population * 100,
    doses_per_100_lower = total_doses_lower / band_population * 100,
    doses_per_100_upper = total_doses_upper / band_population * 100
  ) %>%
  filter(date >= as.Date("2020-12-01"))

fig_a <- ggplot(fig_a_data, aes(x = date, y = doses_per_100, colour = scenario, linetype = scenario)) +
  geom_ribbon(aes(ymin = doses_per_100_lower, ymax = doses_per_100_upper, fill = scenario),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~age_band, nrow = 2) +
  scale_colour_manual(values = scenario_colours, name = NULL) +
  scale_fill_manual(values = scenario_colours, name = NULL) +
  scale_linetype_manual(values = scenario_linetypes, name = NULL) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  labs(x = NULL, y = "Cumulative doses per 100 population") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fig_a_doses_per_capita.png", fig_a, width = 8, height = 6, dpi = 320)

## ---- Figure B: heatmap of vaccine protection deficit, by 5-year age band ----

# "Fully protected" = reached "Full dose 2 protection" (completed primary course)
protection_strata <- "Full dose 2 protection"

monthly_dates <- seq(as.Date("2021-01-01"), as.Date("2022-02-01"), by = "month")

coverage_data <- england_vaccine_df %>%
  filter(vaccine_strata == protection_strata, date %in% monthly_dates) %>%
  mutate(coverage = mean_cum_doses / population * 100) %>%
  select(scenario, age_group, date, coverage)

deficit_data <- coverage_data %>%
  filter(scenario == "Factual") %>%
  select(age_group, date, factual_coverage = coverage) %>%
  inner_join(
    coverage_data %>% filter(scenario != "Factual"),
    by = c("age_group", "date")
  ) %>%
  mutate(
    deficit = coverage - factual_coverage,
    age_group = factor(age_group, levels = age_levels)
  )

# Small in-panel annotation for the "Scaled by age" facet, since a near-zero
# deficit could otherwise be misread as missing data rather than a genuine
# (near) null result.
byage_max <- deficit_data %>%
  filter(scenario == "C) Scaled by age") %>%
  summarise(max_dev = max(abs(deficit), na.rm = TRUE)) %>%
  pull(max_dev)

annotation_df <- data.frame(
  scenario = factor("C) Scaled by age", levels = levels(deficit_data$scenario)),
  date = monthly_dates[round(length(monthly_dates) / 2)],
  age_group = factor(age_levels[round(length(age_levels) / 2)], levels = age_levels),
  label = paste0("Max deviation: ", sprintf("%.1f", byage_max), "pp")
)

scale_max <- max(abs(deficit_data$deficit), na.rm = TRUE)

fig_b <- ggplot(deficit_data, aes(x = date, y = age_group, fill = deficit)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(data = annotation_df, aes(x = date, y = age_group, label = label),
            inherit.aes = FALSE, size = 3, colour = "grey40", fontface = "italic") +
  facet_wrap(~scenario, ncol = 1) +
  scale_fill_gradient2(
    high = "#185fa5", mid = "white", low = "#a32d2d", midpoint = 0,
    limits = c(-scale_max, scale_max),
    name = "Percentage point\ndifference in coverage\n(counterfactual minus\nfactual)"
  ) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

ggsave("Fig6.png", fig_b, width = 8, height = 9, dpi = 300)
ggsave("Fig6.pdf", fig_b, width = 8, height = 9, dpi = 300)
