#Save the parameters for easier cross-reference
param_text <- sprintf("Parameters: \n
                      population_assumptions: %s \n
                      population_year: %s \n
                      vaccine_assumptions: %s \n
                      param_iterations: %s",
                      population_assumptions, population_year, vaccine_assumptions, param_iterations)
writeLines(param_text, "parameters.txt")