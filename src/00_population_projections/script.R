library(readxl)
library(dplyr)
library(tidyr)

# Central estimates
###################
population <- read_excel("ONS_data_sheets/popprojsicb5yrmigcat2322based.xls", 
                         sheet = "Persons", skip = 3)
population <- population[,c("CODE", "AREA", "AGE GROUP", "2027", "2032", "2037", "2042","2047")]
population <- population %>% 
  filter(startsWith(CODE, "E4"))
population$`2047` <- as.numeric(population$`2047`)
population$`2042` <- as.numeric(population$`2042`)
population$`2037` <- as.numeric(population$`2037`)
population$`2032` <- as.numeric(population$`2032`)
population$`2027` <- as.numeric(population$`2027`)

population_long <- population %>%
  pivot_longer(
    cols = c(`2027`, `2032`, `2037`, `2042`, `2047`),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.integer(year))

population_wide <- population_long %>%
  pivot_wider(
    id_cols = c(CODE, AREA, year),
    names_from = 'AGE GROUP',
    values_from = population
  )

## Cut "all ages"
population_wide$`All ages` <- NULL
## Combine last 3 into 80+:
population_wide$`80+` <- population_wide$`80-84` + population_wide$`85-89` + population_wide$`90+`
#Cut the originals
population_wide$`90+` <- NULL
population_wide$`85-89` <- NULL
population_wide$`80-84` <- NULL
population_wide$scenario <- "ONS_NHS_region_principal"

## Make region naming consistent:
region_map <- c(
  "London"                   = sircovid::regions("england")[2],
  "South East"               = sircovid::regions("england")[6],
  "South West"               = sircovid::regions("england")[7],
  "Midlands"                 = sircovid::regions("england")[3],
  "North East and Yorkshire" = sircovid::regions("england")[4],
  "East of England"          = sircovid::regions("england")[1],
  "North West"               = sircovid::regions("england")[5]
)

population_wide$AREA <- region_map[population_wide$AREA]
ONS_forecasts <- population_wide

#Load lookup table:
LTLA_to_NHS_region <- read.csv("ONS_data_sheets/LTLA_to_Region.csv")

# Low migration scenario:
###########################
population <- read_excel("ONS_data_sheets/popprojla5yrlow22based.xls", 
                         sheet = "Persons", skip = 3)
population <- population[,c("CODE", "AREA", "AGE GROUP", "2027", "2032", "2037", "2042","2047")]
#Filter out everything that doesn't start E0
population <- population %>% 
  filter(startsWith(CODE, "E0"))
population$`2047` <- as.numeric(population$`2047`)
population$`2042` <- as.numeric(population$`2042`)
population$`2037` <- as.numeric(population$`2037`)
population$`2032` <- as.numeric(population$`2032`)
population$`2027` <- as.numeric(population$`2027`)
#309 regions
regions_in_data_sheet <- unique(population$CODE)
regions_in_look_up <- unique(LTLA_to_NHS_region$LAD21CD)
if (any(!(regions_in_data_sheet %in% regions_in_look_up))) {
  stop("Error: region code in data sheet not found in lookup table")
}
if (any(!(regions_in_look_up %in% regions_in_data_sheet))) {
  stop("Error: region code in lookup table not accounted for in data")
}

# Convert from LTLA to nhs_region
population_region <- population %>%
  left_join(
    LTLA_to_NHS_region %>% select(LAD21CD, RGN21NM),
    by = c("CODE" = "LAD21CD")
  ) %>%
  # add a new column with the NHS region
  rename(NHS_region = RGN21NM) %>%
  # replace the AREA column with region where join succeeded
  mutate(AREA = coalesce(NHS_region, AREA))

#Check that all converted:
if (any(!(population_region$AREA == population_region$NHS_region))) {
  stop("Error: population AREA did not map.")
}

#Sum all region totals together
population_region$NHS_region <- NULL
population_region <- filter(population_region, `AGE GROUP` != "All ages")
sum_before_aggregate <- sum(population_region$`2047`)

population_agg <- population_region %>%
  group_by(AREA, `AGE GROUP`) %>%
  summarise(
    `2047` = sum(`2047`, na.rm = TRUE),
    `2042` = sum(`2042`, na.rm = TRUE),
    `2037` = sum(`2037`, na.rm = TRUE),
    `2032` = sum(`2032`, na.rm = TRUE),
    `2027` = sum(`2027`, na.rm = TRUE),
    .groups = "drop"
  )

sum_after_aggregate <- sum(population_agg$`2047`)

population_long <- population_agg %>%
  pivot_longer(
    cols = c(`2027`, `2032`, `2037`, `2042`, `2047`),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.integer(year))

population_wide <- population_long %>%
  pivot_wider(
    id_cols = c(AREA, year),
    names_from = 'AGE GROUP',
    values_from = population
  )

## Combine last 3 into 80+:
population_wide$`80+` <- population_wide$`80-84` + population_wide$`85-89` + population_wide$`90+`
#Cut the originals
population_wide$`90+` <- NULL
population_wide$`85-89` <- NULL
population_wide$`80-84` <- NULL
population_wide$scenario <- "ONS_NHS_region_low_migration"
#set code
population_wide <- population_wide %>%
  left_join(
    ONS_forecasts %>%
      select(AREA, CODE) %>%
      distinct(),   # important: avoid duplicates
    by = "AREA"
  )

#Bind together
ONS_forecasts <- rbind(ONS_forecasts, population_wide)

######################################################
#High migration
population <- read_excel("ONS_data_sheets/popprojla5yrhigh2022based.xls", 
                         sheet = "Persons", skip = 3)
population <- population[,c("CODE", "AREA", "AGE GROUP", "2027", "2032", "2037", "2042","2047")]
#Filter out everything that doesn't start E0
population <- population %>% 
  filter(startsWith(CODE, "E0"))
population$`2047` <- as.numeric(population$`2047`)
population$`2042` <- as.numeric(population$`2042`)
population$`2037` <- as.numeric(population$`2037`)
population$`2032` <- as.numeric(population$`2032`)
population$`2027` <- as.numeric(population$`2027`)
#309 regions
regions_in_data_sheet <- unique(population$CODE)
regions_in_look_up <- unique(LTLA_to_NHS_region$LAD21CD)
if (any(!(regions_in_data_sheet %in% regions_in_look_up))) {
  stop("Error: region code in data sheet not found in lookup table")
}
if (any(!(regions_in_look_up %in% regions_in_data_sheet))) {
  stop("Error: region code in lookup table not accounted for in data")
}

# Convert from LTLA to nhs_region
population_region <- population %>%
  left_join(
    LTLA_to_NHS_region %>% select(LAD21CD, RGN21NM),
    by = c("CODE" = "LAD21CD")
  ) %>%
  # add a new column with the NHS region
  rename(NHS_region = RGN21NM) %>%
  # replace the AREA column with region where join succeeded
  mutate(AREA = coalesce(NHS_region, AREA))

#Check that all converted:
if (any(!(population_region$AREA == population_region$NHS_region))) {
  stop("Error: population AREA did not map.")
}

#Sum all region totals together
population_region$NHS_region <- NULL
population_region <- filter(population_region, `AGE GROUP` != "All ages")
sum_before_aggregate <- sum(population_region$`2047`)

population_agg <- population_region %>%
  group_by(AREA, `AGE GROUP`) %>%
  summarise(
    `2047` = sum(`2047`, na.rm = TRUE),
    `2042` = sum(`2042`, na.rm = TRUE),
    `2037` = sum(`2037`, na.rm = TRUE),
    `2032` = sum(`2032`, na.rm = TRUE),
    `2027` = sum(`2027`, na.rm = TRUE),
    .groups = "drop"
  )

sum_after_aggregate <- sum(population_agg$`2047`)

population_long <- population_agg %>%
  pivot_longer(
    cols = c(`2027`, `2032`, `2037`, `2042`, `2047`),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.integer(year))

population_wide <- population_long %>%
  pivot_wider(
    id_cols = c(AREA, year),
    names_from = 'AGE GROUP',
    values_from = population
  )

## Combine last 3 into 80+:
population_wide$`80+` <- population_wide$`80-84` + population_wide$`85-89` + population_wide$`90+`
#Cut the originals
population_wide$`90+` <- NULL
population_wide$`85-89` <- NULL
population_wide$`80-84` <- NULL
population_wide$scenario <- "ONS_NHS_region_high_migration"
#set code
population_wide <- population_wide %>%
  left_join(
    ONS_forecasts %>%
      select(AREA, CODE) %>%
      distinct(),   # important: avoid duplicates
    by = "AREA"
  )

#Bind together
ONS_forecasts <- rbind(ONS_forecasts, population_wide)


## Set a description of the scenario
dir.create("outputs")
saveRDS(ONS_forecasts, "outputs/ONS_population_projections.rds")
