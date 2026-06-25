
set.seed(42L)
source("global_util.R")
# 1) load the baseline (from severity_parameters)
baseline_list <- readRDS("parameters/base.rds")
if (!region %in% names(baseline_list)) stop("Region not found in parameters_base.rds; names: ", paste(names(baseline_list), collapse = ", "))
baseline <- baseline_list[[region]]

epoch_dates <- baseline$epoch_dates
final_date <- baseline$date
## Check all epoch dates that are beyond our final date
epoch_dates <- epoch_dates[epoch_dates <= sircovid_date(final_date)]
# 2) source the transform helpers (this will define compute_severity(), compute_progression(), compute_observation(), make_transform())
#source("parameters/transform.R")


#############################################################
## FIX???
# Create a private environment and inject the required globals
transform_env <- new.env(parent = baseenv())
transform_env$assumptions <- assumptions

# source the transform into that environment so functions defined there close over it
sys.source("parameters/transform.R", envir = transform_env)

# Grab the make_transform function from the sourced environment
if (!exists("make_transform", envir = transform_env, inherits = FALSE)) {
  stop("make_transform not found in parameters/transform.R when sourced in transform_env")
}
make_transform_fn <- get("make_transform", envir = transform_env, inherits = FALSE)

# Now create the transform closure with baseline (the closure will capture transform_env,
# so it will find 'assumptions' and other globals there at runtime)
transform_fun <- make_transform_fn(baseline)
#########################################################################################

# 3) create the transform closure
#transform_fun <- make_transform(baseline)

# 4) load the pars samples from the 1000 fits of the severity paper:
severity_fits_parameters <- readRDS("data/severity_fits_parameters.rds")
severity_fits_parameters <- severity_fits_parameters[[region]]

final_hosps <- list()
final_vaccs <- list()

## Can I actually parallelise this?
for(i in 1:param_iterations){
  idx <- ((i - 1) %% nrow(severity_fits_parameters)) + 1
  numeric_initial <- severity_fits_parameters[idx,]
  if (is.null(names(numeric_initial))) stop("initial vector is not named — something unexpected in pars_meta$mcmc$initial()")
  
  # 6) call the transform to get the multistage parameters object
  multistage_params <- transform_fun(numeric_initial)
  iteration_hosps <- NULL
  iteration_vaccs <- NULL
  for(j in 1:(length(epoch_dates)+1)){
    mod <- sircovid::lancelot$new(multistage_params[[j]]$pars, ifelse(j==1, 0, (epoch_dates[j-1]/multistage_params[[j-1]]$pars$dt)-1),
                                  1) #, seed = 1L) #pars, init time, and n_particles
    if(j == length(epoch_dates)+1){
      end <- sircovid_date(final_date) / multistage_params[[j]]$pars$dt - 1
    }else{
      end <- epoch_dates[j] / multistage_params[[j]]$pars$dt - 1
    }
    
    info <- mod$info()
    
    if(j == 1){
      initial <- sircovid::lancelot_initial(info, 1, multistage_params[[j]]$pars)
    } else{
      initial <- where_we_left_off
      initial <- multistage_params[[j]]$transform_state(matrix(initial, ncol = 1), old_info, info)
    }
    
    mod$update_state(state = initial)
    
    ## Return "interesting" state indices:
    ## https://mrc-ide.github.io/sircovid/reference/lancelot_index.html
    
    ## Note, only save vaccinations for the first 100 iterations, it's really overkill after that.
    if(i > 100){
      index <- c(sircovid::lancelot_index(info)$run,
                 deaths_comm = info$index[["D_comm_tot"]],
                 deaths_hosp = info$index[["D_hosp_tot"]],
                 admitted = info$index[["cum_admit_conf"]],
                 diagnoses = info$index[["cum_new_conf"]],
                 sympt_cases = info$index[["cum_sympt_cases"]],
                 sympt_cases_over25 = info$index[["cum_sympt_cases_over25"]],
                 D_tot = info$index[["D_tot"]],
                 D_inc = info$index[["D_inc"]],
                 ihr = info$index[["ihr"]],
                 hfr = info$index[["hfr"]],
                 ifr = info$index[["ifr"]],
                 #cum_n_vaccinated = info$index[["cum_n_vaccinated"]], #Currently the dimension causes rbind issues, maybe return to this later
                 S = info$index[["S"]], #Needed for Rt calculations
                 prob_strain = info$index[["prob_strain"]], #Needed for Rt calculations
                 R = info$index[["R"]], #Needed for Rt calculations
                 # Extract prop_protected
                 protected_S_vaccinated = info$index[["protected_S_vaccinated"]],
                 protected_R_vaccinated = info$index[["protected_R_vaccinated"]],
                 protected_R_unvaccinated = info$index[["protected_R_unvaccinated"]]
                 )
    }else{
      index <- c(sircovid::lancelot_index(info)$run,
                 deaths_comm = info$index[["D_comm_tot"]],
                 deaths_hosp = info$index[["D_hosp_tot"]],
                 admitted = info$index[["cum_admit_conf"]],
                 diagnoses = info$index[["cum_new_conf"]],
                 sympt_cases = info$index[["cum_sympt_cases"]],
                 sympt_cases_over25 = info$index[["cum_sympt_cases_over25"]],
                 D_tot = info$index[["D_tot"]],
                 D_inc = info$index[["D_inc"]],
                 ihr = info$index[["ihr"]],
                 hfr = info$index[["hfr"]],
                 ifr = info$index[["ifr"]],
                 #cum_n_vaccinated = info$index[["cum_n_vaccinated"]], #Currently the dimension causes rbind issues, maybe return to this later
                 S = info$index[["S"]], #Needed for Rt calculations
                 prob_strain = info$index[["prob_strain"]], #Needed for Rt calculations
                 R = info$index[["R"]], #Needed for Rt calculations
                 # Extract prop_protected
                 protected_S_vaccinated = info$index[["protected_S_vaccinated"]],
                 protected_R_vaccinated = info$index[["protected_R_vaccinated"]],
                 protected_R_unvaccinated = info$index[["protected_R_unvaccinated"]],
                 #The additional vaccinated
                 cum_vaccinated = info$index[["cum_n_vaccinated"]])
    }
    
    
    ## Set the "index" vector that is used to return a subset of pars after using run(). 
    ## If this is not used then run() returns all elements in the state vector, which may be excessive and slower than necessary
    mod$set_index(index)
    
    ## Simulate model:
    time_series <- seq(from = ifelse(j==1, 0, (epoch_dates[j-1]/multistage_params[[j]]$pars$dt)),
                       to= end,
                       by = 4)
    res_sim <- mod$simulate(time_series)[,1,]
    
    # Expand to the full state space again to extract our initialisation point for the next epoch
    mod$set_index(seq(1,length(initial)))
    # And extract the end point:
    where_we_left_off <- mod$run(end)[,1]
    
    ## Calculate Rt
    if(multistage_params[[j]]$pars$n_strains == 1){
      Rt <- sircovid::lancelot_Rt(time = time_series,
                                  S = matrix(res_sim[grep("^S[0-9]+$", names(index)),], nrow = 19),
                                  p = multistage_params[[j]]$pars,
                                  R = NULL, #NULL IF ONE STRAIN
                                  prob_strain = NULL, #NULL IF ONE STRAIN
                                  type = NULL,
                                  weight_Rt = TRUE,
                                  keep_strains_Rt = TRUE)
    } else{
      n_strains <- multistage_params[[j]]$pars$n_strains
      n_Rs <- multistage_params[[j]]$pars$n_strains_R
      n_vaccs <- multistage_params[[j]]$pars$n_vacc_classes
      n_groups <- multistage_params[[j]]$pars$n_groups
      
      Rt <- sircovid::lancelot_Rt(time = time_series,
                                  S = matrix(res_sim[grep("^S[0-9]+$", names(index)),], nrow = n_groups*n_vaccs),
                                  p = multistage_params[[j]]$pars,
                                  R = matrix(res_sim[grep("^R[0-9]+$", names(index)),], nrow = n_groups*n_Rs*n_vaccs), 
                                  prob_strain = matrix(res_sim[grep("prob_strain", names(index)),], nrow = 2), 
                                  type = NULL,
                                  weight_Rt = TRUE,
                                  keep_strains_Rt = FALSE)
    }
    ## Update index to include the Rt calculations
    index["eff_Rt_all"] <- 0L
    index["eff_Rt_general"] <- 0L
    index["Rt_all"] <- 0L
    index["Rt_general"] <- 0L
    ## And add the trajectories to res_sim
    res_sim <- rbind(res_sim, 
                     eff_Rt_all = Rt$eff_Rt_all, 
                     eff_Rt_general = Rt$eff_Rt_general, 
                     Rt_all = Rt$Rt_all, 
                     Rt_general = Rt$Rt_general)
    
    ## To save space, we'll now cut the S, prob_strain, and R vectors. From res_sim AND index
    res_sim <- res_sim[-c(grep("^S[0-9]+$", names(index)), grep("prob_strain", names(index)), grep("^R[0-9]+$", names(index))),]
    ## And cut them from the index vector too.
    index <- index[-c(grep("^S[0-9]+$", names(index)), grep("prob_strain", names(index)), grep("^R[0-9]+$", names(index)))]
    
    ## Also, because vaccine dimensions keep changing, it'll likely be safer to just save them in a separate item.
    if(i < 101){
      res_vacc <- res_sim[grep("^cum_vaccinated[0-9]+$", names(index)),]
      if(!is.null(iteration_vaccs)){
        vacc_dim <- max(nrow(iteration_vaccs), nrow(res_vacc))
        if(nrow(iteration_vaccs) < vacc_dim){
          missing_rows <- vacc_dim - nrow(iteration_vaccs)
          iteration_vaccs <- rbind(iteration_vaccs, matrix(0, nrow = missing_rows, ncol = ncol(iteration_vaccs)))
        }
      }
      iteration_vaccs <- cbind(iteration_vaccs, res_vacc)
      ## Now remove cum_n_vaccinated from res_sim and index:
      res_sim <- res_sim[-grep("^cum_vaccinated[0-9]+$", names(index)),]
      index <- index[-grep("^cum_vaccinated[0-9]+$", names(index))]
      
    }

    ## if j = 1, then protected_* categories are only dim 1, but for all other j it will be 2, so we need to inflate in the extra rows
    if(j == 1){
      targets <- c(
        "protected_S_vaccinated",
        "protected_R_vaccinated",
        "protected_R_unvaccinated"
      )
      
      for (target in rev(targets)) {  # reverse order so row indices don't shift
        target_index <- which(rownames(res_sim) == target)
        
        # Rename original row
        rownames(res_sim)[target_index] <- paste0(target, "1")
        
        # Create new zero row
        new_row <- matrix(0, nrow = 1, ncol = ncol(res_sim))
        colnames(new_row) <- colnames(res_sim)
        rownames(new_row) <- paste0(target, "2")
        
        # Insert immediately below original row
        res_sim <- rbind(
          res_sim[1:target_index, , drop = FALSE],
          new_row,
          res_sim[(target_index + 1):nrow(res_sim), , drop = FALSE]
        )
      }
    }
    
    iteration_hosps <- cbind(iteration_hosps, res_sim)
    old_info <- info
  }
  
  #Make a total hospitalisations over all ages variable:
  iteration_hosps <- rbind(
    iteration_hosps,
    total_hospitalisations = iteration_hosps["admitted_inc", ] + iteration_hosps["diagnoses_inc", ]
  )
  final_hosps[[i]] <- iteration_hosps
  
  if(i < 101){
    final_vaccs[[i]] <- iteration_vaccs
  }

}
dir.create("outputs")
saveRDS(final_hosps, "outputs/model_simulations.rds")
saveRDS(final_vaccs, "outputs/vaccine_simulations.rds")
index["total_hospitalisations"] <- 0L
saveRDS(index, "outputs/index.rds")

england_data <- read_csv("data/england_region_data.csv")
this_region <- region
england_data %>%
  filter(region == this_region) %>%
  select(c("date", "ons_death_hospital"))-> england_data
england_data$date <- sircovid::sircovid_date(england_data$date)             

# extract row r from each matrix and keep as list of vectors

lst_row <- map(final_hosps, function(m) m[which(names(index) == "deaths_hosp_inc"), ])
# convert to tibble with time as column
df <- tibble::enframe(lst_row, name = "draw") %>%   # draw = 1..100, value = numeric vector
  tidyr::unnest_longer(value) %>%               # expand each vector to rows
  group_by(pos = rep(seq_along(lst_row[[1]]), times = length(lst_row))) %>% 
  # alternative: use index while unnesting; above ensures a 'pos' column (1..L)
  summarise(
    time = first(pos) - 1,
    mean = mean(value, na.rm = TRUE),
    lower = quantile(value, 0.025, na.rm = TRUE),
    upper = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(time, mean, lower, upper)

# ggplot(df, aes(x = time, y = mean)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#   geom_point(data = england_data, aes(x = date, y = ons_death_hospital )) +
#   theme_minimal()
if(population_assumptions == "rtm_baseline"){
  total_pop <- sum(sircovid:::sircovid_population(region))
}else{
  total_pop <- sum(baseline$population)
}

ggplot() +
  # Simulation ribbon
  geom_ribbon(
    data = df,
    aes(x = as.Date(sircovid::sircovid_date_as_date(time)), 
        ymin = lower/total_pop*1000, 
        ymax = upper/total_pop*1000, 
        fill = "95% CrI"),
    alpha = 0.18,
    colour = NA
  ) +
  # Simulation mean
  geom_line(
    data = df,
    aes(x = as.Date(sircovid::sircovid_date_as_date(time)), y = mean/total_pop*1000, 
        colour = "Model mean"),
    size = 1.1
  ) +
  # Observed data
  geom_point(
    data = england_data,
    aes(x = as.Date(sircovid::sircovid_date_as_date(date)), y = ons_death_hospital/sum(sircovid:::sircovid_population(region))*1000, colour = "Observed"),
    size = 2.2,
    alpha = 0.65
  ) +
  
  # Colour/fill scales
  scale_colour_manual(
    name = "",
    values = c(
      "Model mean" = "#1B73BA",
      "Observed"   = "#B60000"
    )
  ) +
  scale_fill_manual(
    name = "",
    values = c("95% CrI" = "#1B73BA")
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(colour = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_text(size = 11, colour = "grey40")
  ) +
  
  labs(
    title = "Modelled COVID-19 Hospital Deaths vs Observed Data",
    subtitle = sprintf("Mean model trajectory with 95%% credible interval - %s", this_region),

    x = "Time (days)",
    y = "Daily hospital deaths (per 1000 people)"
  ) -> p1

ggsave(
  filename = "outputs/model_vs_observed.png",
  plot = p1,
  width = 10,      # inches
  height = 6,      # inches
  dpi = 320        # high resolution
)

#Save the contact matrix
c_mat <- multistage_params[[1]]$pars$m
saveRDS(c_mat, "contact_matrix.rds")
#Save the parameters for easier cross-reference
param_text <- sprintf("Parameters: \n
                      population_assumptions: %s \n
                      population_year: %s \n
                      region: %s \n
                      vaccine_assumptions: %s \n
                      param_iterations: %s",
                      population_assumptions, population_year, region, vaccine_assumptions, param_iterations)
writeLines(param_text, "parameters.txt")

## Clear cache so we don't re-use transmission matrices in future counterfactuals
sircovid:::clear_cache()



