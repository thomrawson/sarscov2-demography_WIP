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

## Set a description of the scenario
dir.create("outputs")
saveRDS(population_wide, "outputs/ONS_population_projections.rds")
