library(orderly1)

orderly_run("02_parameters",
            parameters = list(population_assumptions = "rtm_baseline", 
                              population_year = 2019,                             #2019 is the default which should only be used for "rtm_baseline" population_assumptions. Otherwise, 2027,2032,2037,2042,2047
                              
                              vaccine_assumptions = "baseline"),
            use_draft = "newer")


orderly_run("03_simulations",
            parameters = list(population_assumptions = "rtm_baseline",
                              population_year = 2019,
                              vaccine_assumptions = "baseline",
                              region = "london",
                              param_iterations = 1000),
            use_draft = "newer")