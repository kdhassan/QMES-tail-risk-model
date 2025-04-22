library(MASS)
library(fitdistrplus)
library(actuar)
library(dplyr)
library(tidyr)
library(boot)
library(yaml)
library(ggplot2)
library(glue)
library(logger)
library(readxl)
library(rlang)
source('Excess Layer Pricing\\excess_utils.R')


### DEDUCTIBLE TRACKING ###  



##  --- DO NOT MODIFY --- ##
config =  yaml::yaml.load_file("Excess Layer Pricing\\Deductible Tracking\\config.yaml")

##  --- DO NOT MODIFY --- ##


## READ THE DATA
df = readxl::read_excel(path = config$data_path)

## FILTER THE DATA IF MODEL NEEDS TO RUN ON SUBSET OF DATA, CURRENTLY ONLY ALLOWS ONE COLUMN FOR FILTER
if (!is.null(config$filter_col))  {
  df = subset(df, (df[config$filter_col] == config$filter_val) )
  
}

## ASSERT COLUMN NAMES: 'loss_amount' , 'year_of_loss' EXIST
check_column_names(df)

## PRE-PROCESS DATA
df = preproc(df, config$excess_lower_threshold)


log_info("CREATING PROJECT FOLDER....")
project_dir = create_project_dir(config$PROJECT_NAME, config$save_dir)
## SAVE THE PARAMETERS USED TO RUN THIS EXPERIMENT
capture.output(config, file = file.path(project_dir, "config.txt"))

log_info("GENERATING BASIC COUNT AND SEVERITY PLOT....PLEASE ANALYZE CAREFULLY...")
run_diagnostic(df , project_dir)



if (!config$tail_fitting_method %in% c("default", 'at_excess_threshold')) {
  stop(
    "INVALID argument for...'tail_fitting_method. Use: 'default' or 'at_excess_threshold' as argument values "
  )
}


if (config$tail_fitting_method == 'default') {
  best_lower_tail_threshold = get_best_threshold(
    df,
    lower_bound = config$lower_quantile_for_tail,
    upper_bound = config$upper_quantile_for_tail
  )
  
  fit_pareto  = fitdist(
    df$loss_amount,
    "pareto",
    method = "qme",
    probs = c(best_lower_tail_threshold, 0.999)
  )
}

if (config$tail_fitting_method == 'at_excess_threshold') {
  lower_tail_threshold_at_excess = get_threshold_at_excess(df, config$excess_lower_threshold)
  
  fit_pareto  = fitdist(
    df$loss_amount,
    "pareto",
    method = "qme",
    probs = c(lower_tail_threshold_at_excess, 0.999)
  )
}

## FIT THE LOWER LEVEL CLAIM PARETO USING MLE 
fit_pareto_lower_layer = fitdist(df$loss_amount, "pareto")





if (!config$count_method %in% c("lnorm", "pois", "fixed")) {
  stop(
    "INVALID argument for... 'count_method'... Use: 'lnorm', or 'pois', or 'fixed' as argument values "
  )
}


if (config$count_method == 'lnorm') {
  lnorm_count_dist = get_lnorm_count_dist(df)
  
} else if (config$count_method == 'pois') {
  lambda = get_rpois_lambda(df, use = config$poisson_lambda)
  
} else if (config$count_method == 'fixed') {
  clm_count = config$fixed_count
  
}


#################### RUN THE SIMULATION ######################

simulation_means = c()
simulation_counts = c()
# run experiment N-times
for (simulation_n_time in 1:config$run_simulation_n_times) {
  log_info(glue("RUNNING SIMULATION # : {simulation_n_time}"))
  
  ## define empty vector where all simulation means will be appended
  simulated_losses = c()
  
  for (draw in 1:config$no_of_simulation_draws) {
    if (config$count_method == 'lnorm') {
      no_of_clms = rlnorm(1,
                          lnorm_count_dist$estimate['meanlog'],
                          lnorm_count_dist$estimate['sdlog'])
      
    } else if (config$count_method == 'pois') {
      no_of_clms = rpois(1, lambda)
      
    } else if (config$count_method == 'fixed') {
      no_of_clms = config$fixed_count
      
    }
    
    simulation_counts = c(simulation_counts, no_of_clms)
    
    ## GENERATE SEVERITIES. UPPER LEVEL
    simulatedclaims_upper_level = rpareto(no_of_clms, fit_pareto$estimate['shape'], fit_pareto$estimate['scale'])
    simulatedclaims_upper_level = simulatedclaims_upper_level[simulatedclaims_upper_level > config$deductible]
    
    ## GENERATE SEVERITIES. LOWER LEVEL
    simulatedclaims_lower_level = rpareto(no_of_clms, fit_pareto_lower_layer$estimate['shape'], fit_pareto_lower_layer$estimate['scale'])
    simulatedclaims_lower_level = simulatedclaims_lower_level[simulatedclaims_lower_level <= config$deductible]
    
    ## COMBINE LOWER AND UPPER LAYER SEVERITIES
    simulatedclaims = c(simulatedclaims_lower_level, simulatedclaims_upper_level)
    
    ## CONVERT SIMULATED LOSSES TO EXCESS LAYER 
    simulatedclaims_excess = as.numeric(lapply(simulatedclaims, FUN = function(x) max(x - config$deductible, 0)))
    ## CAP LOSSES AT THE COVERED IMIT 
    simulatedclaims_excess = pmin(simulatedclaims_excess, config$covered_limit)
    
    ## KEEP TRACK OF DECTUBIBLE LIMIT 
    deductible_vector = pmin(simulatedclaims, config$deductible)
    deductible_vector_cumsum = cumsum(deductible_vector)
    
    ## TRACK WHICH CLAIMS ARE ABOVE THE DEDUCTIBLE LIMIT
    covered_deductible = deductible_vector[deductible_vector_cumsum > config$deductible_limit]
    
    ## trigger this if deductible limit is reached 
    if (max(deductible_vector_cumsum) >= config$deductible_limit)
    {
      # if deductible limit is reached get the additional we need to pay for the n-1 claim before deductible is reached &
      # the amount they will pay before they reach the full deductible limit
      first_loss_covered_by_us = deductible_vector[deductible_vector_cumsum > config$deductible_limit][1]
      # deductible that still needs to be paid by client before they reach limit
      deductible_covered_by_client = config$deductible_limit  - max(deductible_vector_cumsum[deductible_vector_cumsum < config$deductible_limit])
      # remaining covered by us
      deductible_covered_by_us = first_loss_covered_by_us - deductible_covered_by_client
      # replace the first deductible value of the claim by correct amount,  # the remaining amount we will pay
      covered_deductible[1] = deductible_covered_by_us

    }
    
    
    covered_claim = sum(c(simulatedclaims_excess, covered_deductible))
    
    
    
    simulated_losses = c(simulated_losses, covered_claim)
    
    
    
  }
  
  simulation_means = c(simulation_means, mean(simulated_losses))
  
  #################################################################################
  
  print("FINAL ESTIMATE: ")
  print(mean(simulation_means))
  
}




if (config$save_loss_distribtuion) {
  save_value = simulated_losses
  
  if (config$apply_deductible_tracking == FALSE) {
    save_value = rpareto(config$no_of_simulation_draws,
                         fit_pareto$estimate['shape'],
                         fit_pareto$estimate['scale'])
  }
  
  write.table(
    tibble(save_value) ,
    file = file.path(project_dir, 'simulated_loss_distribution.csv'),
    sep = ",",
    row.names = FALSE
  )
  
}

# get emprical bs means
empirical_bootstrapped_mean = get_excess_bootstrap_mean(df, config$excess_lower_threshold, config$excess_upper_threshold, 10000)


capture.output(mean(simulation_means),
               file = file.path(project_dir, "final_estimate.txt"))

capture.output(empirical_bootstrapped_mean,
               file = file.path(project_dir, "empirical_bootstrapped_mean.txt"))



# save the run parameters
fit_pareto_summary  = summary(fit_pareto)
fit_pareto_lower_layer_summary = summary(fit_pareto_lower_layer)


capture.output(summary(fit_pareto_summary),
               file = file.path(project_dir, "fit_summary.txt"))

capture.output(summary(fit_pareto_lower_layer_summary),
               file = file.path(project_dir, "fit_summary_lower_layer.txt"))

## save the fitted distribution plots: upper level
jpeg(
  file.path(project_dir, "fit_pareto_plot.jpg"),
  width = 1080,
  height = 1080
)
plot(fit_pareto)
dev.off()

## save the fitted distribution plots: lower layer 
jpeg(
  file.path(project_dir, "fit_pareto_lower_layer_plot.jpg"),
  width = 1080,
  height = 1080
)
plot(fit_pareto_lower_layer_summary)
dev.off()


## save the count plot 
jpeg(
  file.path(project_dir, "simulation_counts.jpg"),
  width = 1080,
  height = 1080
)
hist(simulation_counts)
dev.off()
