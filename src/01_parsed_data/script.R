severity_fits_parameters <- 
  list(east_of_england = readRDS("severity_fits_outputs/east_of_england_fit_parameters.rds"),
       london = readRDS("severity_fits_outputs/london_fit_parameters.rds"),
       midlands = readRDS("severity_fits_outputs/midlands_fit_parameters.rds"),
       north_east_and_yorkshire = readRDS("severity_fits_outputs/north_east_and_yorkshire_fit_parameters.rds"),
       north_west = readRDS("severity_fits_outputs/north_west_fit_parameters.rds"),
       south_east = readRDS("severity_fits_outputs/south_east_fit_parameters.rds"),
       south_west = readRDS("severity_fits_outputs/south_west_fit_parameters.rds"))

saveRDS(severity_fits_parameters, "outputs/severity_fits_parameters.rds")