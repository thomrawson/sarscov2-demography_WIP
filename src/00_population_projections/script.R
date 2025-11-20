library(readxl)
library(dplyr)
library(tidyr)

population <- read_excel("ONS_data_sheets/popprojsicb5yrmigcat2322based.xls", 
                         sheet = "Persons", skip = 3)
population <- population[,c("CODE", "AREA", "AGE GROUP", "2047")]
population <- population %>% 
  filter(startsWith(CODE, "E4"))
population$`2047` <- as.numeric(population$`2047`)

population_wide <- population %>%
  pivot_wider(
    names_from = 'AGE GROUP',
    values_from = '2047'
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

## Set a description of the scenario
dir.create("outputs")
saveRDS(population_wide, "outputs/ONS_population_projections.rds")
