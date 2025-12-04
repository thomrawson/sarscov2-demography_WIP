source("global_util.R")

version_check("sircovid", "0.14.11")
version_check("spimalot", "0.8.23")

date <- "2022-02-24"
baselines <- readRDS("parameters/base.rds")
## Extract populations:
if(population_assumptions == "rtm_baseline"){
  region_populations <- c(sum(sircovid:::sircovid_population("east_of_england")),
                          sum(sircovid:::sircovid_population("london")),
                          sum(sircovid:::sircovid_population("midlands")),
                          sum(sircovid:::sircovid_population("north_east_and_yorkshire")),
                          sum(sircovid:::sircovid_population("north_west")),
                          sum(sircovid:::sircovid_population("south_east")),
                          sum(sircovid:::sircovid_population("south_west")))
}else{
  region_populations <- sapply(baselines, function(x) sum(x$population))
}


##################################
##Check that all indices match:

index <- list(east_of_england_indices = readRDS("regional_simulations/east_of_england_index.rds"),
                         london_indices = readRDS("regional_simulations/london_index.rds"),
                         midlands_indices = readRDS("regional_simulations/midlands_index.rds"),
                         north_east_and_yorkshire_indices = readRDS("regional_simulations/north_east_and_yorkshire_index.rds"),
                         north_west_indices = readRDS("regional_simulations/north_west_index.rds"),
                         south_east_indices = readRDS("regional_simulations/south_east_index.rds"),
                         south_west_indices = readRDS("regional_simulations/south_west_index.rds"))
# Check all identical to the first vector
all_same <- all(vapply(index[-1], identical, logical(1), index[[1]]))
if (!all_same) {
  stop("Index vectors are not identical (names and/or values differ).")
}
# Just keep one
index <- index[[1]]
index_names <- names(index)
##################################
## We have to load all simulations so we can add them all together to make the England total one.
regional_simulations <- list(east_of_england = readRDS("regional_simulations/east_of_england_sim.rds"),
                             london = readRDS("regional_simulations/london_sim.rds"),
                             midlands = readRDS("regional_simulations/midlands_sim.rds"),
                             north_east_and_yorkshire = readRDS("regional_simulations/north_east_and_yorkshire_sim.rds"),
                             north_west = readRDS("regional_simulations/north_west_sim.rds"),
                             south_east = readRDS("regional_simulations/south_east_sim.rds"),
                             south_west = readRDS("regional_simulations/south_west_sim.rds")
                             )
# Add all together to get England total across all indices.
# However, some of the rows we don't want to sum, we want to take the population weighted average, e.g. Rt and ifr
# So we'll define a custom function to do that.
dont_sum <- c("time", 
              "ihr", "hfr", "ifr",
              "eff_Rt_all", "eff_Rt_general",
              "Rt_all", "Rt_general")

# Inputs you must provide:
# regional_simulations : list of length 7 (regions), each is a list length param_iterations of matrices (n_row x n_col)
# param_iterations     : integer, number of iterations (e.g. 100)
# pop                  : numeric vector length 7, population per region (used as weights)
# rows_to_weight_names : character vector of rownames to *population-weight average* instead of summing

make_england_from_regions <- function(regional_simulations,
                                      param_iterations,
                                      pop,
                                      rows_to_weight_names = character(0)) {
  ## basic checks
  if (!is.list(regional_simulations) || length(regional_simulations) != 7) {
    stop("regional_simulations must be a list of regions.")
  }
  n_regions <- length(regional_simulations)
  if (length(pop) != n_regions) stop("pop must be the same length as regional_simulations.")
  if (!is.numeric(pop) || any(is.na(pop))) stop("pop must be numeric and non-missing.")
  if (!is.numeric(param_iterations) || param_iterations < 1) stop("param_iterations must be >= 1.")
  
  # Normalize population weights to sum to 1 for a proper weighted average
  w_norm <- pop / sum(pop)
  
  # pick a representative matrix to infer dims/rownames (first region, first iteration)
  example_mat <- regional_simulations[[1]][[1]]
  if (is.null(rownames(example_mat)) && length(rows_to_weight_names) > 0) {
    stop("Matrices do not have rownames; provide row indices or add rownames.")
  }
  
  # Map over iterations
  england_simulations <- Map(
    function(i) {
      # extract i-th matrix from each region (list of n_regions matrices)
      mats <- lapply(regional_simulations, `[[`, i)
      # plain elementwise sum across regions
      sum_mat <- Reduce("+", mats)
      
      
      
      # if rows specified by name, get indices
      if (length(rows_to_weight_names) > 0) {
        # compute population-weighted average matrix of same dims:
        # weighted_mat = sum_i (w_i * mat_i)
        #weighted_mat <- Reduce("+", Map(function(m, w) m * w, mats, w_norm))
        # Weighted sum ignoring NAs
        num <- Reduce("+", Map(function(m, w) w * ifelse(is.na(m), 0, m), mats, w_norm))
        
        # Sum of weights that contributed
        den <- Reduce("+", Map(function(m, w) ifelse(is.na(m), 0, w), mats, w_norm))
        
        # Final weighted mean (or leave out division if you only want weighted sum)
        # (Remember, w sums to 1, so this is just inflating up for the missing values.)
        weighted_mat <- num / den
        
        rnames <- rownames(mats[[1]])
        rows_idx <- match(rows_to_weight_names, rnames)
        if (any(is.na(rows_idx))) {
          stop("Some rows_to_weight_names not found in rownames of matrices.")
        }
        # replace those rows in the summed matrix with the weighted-average rows
        sum_mat[rows_idx, ] <- weighted_mat[rows_idx, ]
      }
      
      sum_mat
    },
    seq_len(param_iterations)
  )
  
  england_simulations
}

england_simulations <- make_england_from_regions(regional_simulations,
                                                 param_iterations,
                                                 region_populations,
                                                 dont_sum)

#########################################
# We don't have to do this for "time" though! (The first index)
## Currently handled by the weighting above.

# england_simulations <- lapply(england_simulations, function(m) {
#   m[1, ] <- regional_simulations$east_of_england[[1]][which(index_names == "time"),]
#   m
# })
##################################
## Define a function that takes an array in, and returns the dataframe version with the mean and 95% CrIs.

summarise_iterations <- function(mat_list, index_names, region_name,
                                 lower_prob = 0.025, upper_prob = 0.975,
                                 param_iterations) {
  ## basic sanity checks
  if (!is.list(mat_list) || length(mat_list) != param_iterations) {
    stop("mat_list must be a list of length param_iterations.")
  }
  if (!is.character(index_names) || length(index_names) != nrow(mat_list[[1]])) {
    stop("index_names must be a character vector of same length as matrix rows.")
  }
  if (!is.character(region_name) || length(region_name) != 1L) {
    stop("region_name must be a single string.")
  }
  
  # Collapse to an array
  arr <- try(simplify2array(mat_list), silent = TRUE)
  # extract time vector from the first index (first row) of the first iteration
  time_vec <- as.vector(arr[1, , 1])
  
  # the output indices are the remaining names
  out_names <- index_names[-1L]
  
  # For each output index (rows 2...onwards) compute mean and quantiles across the param iterations
  # arr[j, , ] is (786 x param_iterations) where apply(..., 1, FUN) computes along the 786 time-steps
  n_out <- length(out_names)
  n_time <- dim(arr)[2]
  
  # Preallocate result storage
  total_rows <- n_out * n_time
  region_col <- rep(region_name, total_rows)
  time_col <- rep(time_vec, times = n_out)        # repeated for each output_type block
  output_type_col <- rep(out_names, each = n_time) # each output_type has n_time rows
  
  mean_col <- numeric(total_rows)
  cri_lower_col <- numeric(total_rows)
  cri_upper_col <- numeric(total_rows)
  
  idx <- 1L
  for (j in seq_len(n_out)) {
    # j corresponds to row index (2...onwards) skipping time
    row_index <- j + 1L
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
    output_type = output_type_col,
    mean = mean_col,
    cri_lower = cri_lower_col,
    cri_upper = cri_upper_col,
    stringsAsFactors = FALSE
  )
  
  out_df
}

england_dataframe <- summarise_iterations(england_simulations, index_names, "england",
                                          param_iterations = param_iterations)

total_dataframe <- rbind(england_dataframe,
                         summarise_iterations(regional_simulations$east_of_england, index_names, "east_of_england",
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_simulations$london, index_names, "london",
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_simulations$midlands, index_names, "midlands",
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_simulations$north_east_and_yorkshire, index_names, "north_east_and_yorkshire",
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_simulations$north_west, index_names, "north_west",
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_simulations$south_east, index_names, "south_east",
                                              param_iterations = param_iterations),
                         summarise_iterations(regional_simulations$south_west, index_names, "south_west",
                                              param_iterations = param_iterations)
                         )

####################################
## Save the final dataframe
saveRDS(total_dataframe, "combined_output_dataframe.rds")
saveRDS(index_names, "index_names.rds")