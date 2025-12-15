source("global_util.R")

version_check("sircovid", "0.14.11")
version_check("spimalot", "0.8.23")

date <- "2022-02-24"
baselines <- readRDS("parameters/base.rds")
## Extract populations:
if(population_assumptions == "rtm_baseline"){
  region_populations <- list("east_of_england" = c(sircovid:::sircovid_population("east_of_england"),0,0),
                             "london" = c(sircovid:::sircovid_population("london"),0,0),
                             "midlands" = c(sircovid:::sircovid_population("midlands"),0,0),
                             "north_east_and_yorkshire" = c(sircovid:::sircovid_population("north_east_and_yorkshire"),0,0),
                             "north_west" = c(sircovid:::sircovid_population("north_west"),0,0),
                             "south_east" = c(sircovid:::sircovid_population("south_east"),0,0),
                             "south_west" = c(sircovid:::sircovid_population("south_west"),0,0))
}else{
  region_populations <- lapply(baselines, function(x) c(x$population,0,0))
}

region_populations$england <- Reduce(`+`, region_populations)

##################################
## We have to load all simulations so we can add them all together to make the England total one.
regional_vaccinations <- list(east_of_england = readRDS("regional_vaccinations/east_of_england_vaccinations.rds"),
                             london = readRDS("regional_vaccinations/london_vaccinations.rds"),
                             midlands = readRDS("regional_vaccinations/midlands_vaccinations.rds"),
                             north_east_and_yorkshire = readRDS("regional_vaccinations/north_east_and_yorkshire_vaccinations.rds"),
                             north_west = readRDS("regional_vaccinations/north_west_vaccinations.rds"),
                             south_east = readRDS("regional_vaccinations/south_east_vaccinations.rds"),
                             south_west = readRDS("regional_vaccinations/south_west_vaccinations.rds")
                             )
# Add all together to get England total across all indices.

  # Map over iterations
  england_vaccinations <- Map(
    function(i) {
      # extract i-th matrix from each region (list of n_regions matrices)
      mats <- lapply(regional_vaccinations, `[[`, i)
      # plain elementwise sum across regions
      sum_mat <- Reduce("+", mats)
      sum_mat
    },
    seq_len(param_iterations)
  )


#########################################
  ## Extract time vector from the East of England sims
  example_sims <- readRDS("east_of_england_sim.rds")
  time_vector <- example_sims[[1]][1,]
  
  ##Define age_groups:
  age_names <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                 "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+", "CHR", "CHW")
  vaccine_names <- c("Had 1st dose", "Full dose 1 protection", "Full dose 2 protection", "Waned dose 2 protection", "Full dose 3 protection", "Waned dose 3 protection", "SHOULD BE EMPTY")
  
  ## Define a function that takes an array in, and returns the dataframe version with the mean and 95% CrIs.

summarise_iterations <- function(mat_list, index_names, vaccine_names, region_name, region_populations,
                                 lower_prob = 0.025, upper_prob = 0.975,
                                 time_vec,
                                 param_iterations) {
  ## basic sanity checks
  if (!is.list(mat_list) || length(mat_list) != param_iterations) {
    stop("mat_list must be a list of length param_iterations.")
  }
  if (!is.character(index_names) || length(index_names) != 19) {
    stop("index_names must be a character vector of length 19 (age groups).")
  }
  if (!is.character(vaccine_names) || length(vaccine_names) != 7) {
    stop("vaccine_names must be a character vector of length 7 (vaccine strata).")
  }
  if (length(vaccine_names)*length(index_names) != nrow(mat_list[[1]])) {
    stop("vaccine_names * index_names should equal the number of matrix rows.")
  }
  if (!is.character(region_name) || length(region_name) != 1L) {
    stop("region_name must be a single string.")
  }
  if (!is.numeric(region_populations) || length(region_populations) != 19L) {
    stop("region_populations must be a length 19 numeric.")
  }
  
  # Collapse to an array
  arr <- try(simplify2array(mat_list), silent = TRUE)
  
  # the output indices are the remaining names
  out_names <- index_names
  
  # For each output index (rows 2...onwards) compute mean and quantiles across the param iterations
  # arr[j, , ] is (786 x param_iterations) where apply(..., 1, FUN) computes along the 786 time-steps
  n_age <- length(out_names)
  n_vaccine <- length(vaccine_names)
  n_out <- nrow(mat_list[[1]])
  n_time <- dim(arr)[2]
  
  # Preallocate result storage
  total_rows <- n_out * n_time
  region_col <- rep(region_name, total_rows)
  time_col <- rep(time_vec, times = n_out)        # repeated for each output_type block
  age_col <- rep(rep(out_names, each = n_time), times = n_vaccine )
  population_col <- rep(rep(region_populations, each = n_time), times = n_vaccine )
  vaccine_col <- rep(vaccine_names, each = n_time*n_age)
  
  mean_col <- numeric(total_rows)
  cri_lower_col <- numeric(total_rows)
  cri_upper_col <- numeric(total_rows)
  
  idx <- 1L
  for (j in seq_len(n_out)) {
    row_index <- j
    # arr[row_index, , ] : a matrix 786 x 1000 (apply over rows => for each timepoint)
    vals_mat <- arr[row_index, , , drop = FALSE]   # dims: 1 x 786 x 1000
    # simplify to a 786 x 1000 matrix
    vals_mat <- matrix(vals_mat, nrow = n_time, ncol = dim(arr)[3])
    
    # compute mean and quantiles across iterations for each time-step
    means <- rowMeans(vals_mat, na.rm = TRUE)                    # length n_time
    
    ## Code in some checks for when ihr/ifr has NaNs:
    quants <- t(apply(vals_mat, 1, function(x) {
      # if (any(is.nan(x))) {
      #   return(c(NaN, NaN))
      # } else {
        quantile(x, probs = c(lower_prob, upper_prob), names = FALSE, na.rm = TRUE)
      # }
    }))
    
    # fill into preallocated vectors
    sel <- idx:(idx + n_time - 1L)
    mean_col[sel] <- means
    cri_lower_col[sel] <- quants[, 1L]
    cri_upper_col[sel] <- quants[, 2L]
    
    idx <- idx + n_time
  }
  
  # assemble into data.frame
  out_df <- data.frame(
    region = region_col,
    time = time_col,
    age_group = age_col,
    population = population_col,
    vaccine_strata = vaccine_col,
    mean_cum_doses = mean_col,
    cri_lower = cri_lower_col,
    cri_upper = cri_upper_col,
    stringsAsFactors = FALSE
  )
  
  out_df
}


england_dataframe <- summarise_iterations(england_vaccinations, age_names, vaccine_names, 
                                          "england", region_populations$england,
                                          time_vec = time_vector,
                                          param_iterations = param_iterations)

total_dataframe <- rbind(england_dataframe,
                         summarise_iterations(regional_vaccinations$east_of_england, age_names, vaccine_names,
                                              "east_of_england", region_populations$east_of_england,
                                              time_vec = time_vector,
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_vaccinations$london, age_names, vaccine_names,
                                              "london", region_populations$london,
                                              time_vec = time_vector,
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_vaccinations$midlands, age_names, vaccine_names,
                                              "midlands", region_populations$midlands,
                                              time_vec = time_vector,
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_vaccinations$north_east_and_yorkshire, age_names, vaccine_names,
                                              "north_east_and_yorkshire", region_populations$north_east_and_yorkshire,
                                              time_vec = time_vector,
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_vaccinations$north_west, age_names, vaccine_names,
                                              "north_west", region_populations$north_west,
                                              time_vec = time_vector,
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_vaccinations$south_east, age_names, vaccine_names,
                                              "south_east", region_populations$south_east,
                                              time_vec = time_vector,
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_vaccinations$south_west, age_names, vaccine_names,
                                              "south_west", region_populations$south_west,
                                              time_vec = time_vector,
                                              param_iterations = param_iterations)
                         )

####################################
## Save the final dataframe
saveRDS(total_dataframe, "combined_vaccination_dataframe.rds")
region_vaccine_schedules <- lapply(baselines, function(x) x$vaccine_schedule)
saveRDS(region_vaccine_schedules, "vaccine_schedules.rds")