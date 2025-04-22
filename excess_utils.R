# excess_utils.R — Helper Functions for Deductible Tracking Simulation

library(dplyr)
library(ggplot2)
library(logger)
library(fitdistrplus)
library(actuar)
library(boot)

# Check for required columns
check_column_names <- function(df) {
  required_cols <- c("loss_amount", "year_of_loss")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(glue::glue("Missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }
}

# Preprocess data by filtering based on excess lower threshold
preproc <- function(df, threshold) {
  df <- df %>% filter(!is.na(loss_amount), loss_amount >= threshold)
  return(df)
}

# Create timestamped output directory
create_project_dir <- function(project_name, save_dir) {
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  dir_path <- file.path(save_dir, paste0(project_name, "_", timestamp))
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  return(dir_path)
}

# Diagnostic plot for count + severity
run_diagnostic <- function(df, project_dir) {
  jpeg(file.path(project_dir, "loss_amount_histogram.jpg"))
  hist(df$loss_amount, main = "Histogram of Loss Amounts", xlab = "Loss", breaks = 50)
  dev.off()
  
  jpeg(file.path(project_dir, "loss_by_year.jpg"))
  df %>%
    group_by(year_of_loss) %>%
    summarise(total_loss = sum(loss_amount)) %>%
    ggplot(aes(x = year_of_loss, y = total_loss)) +
    geom_col(fill = "steelblue") +
    theme_minimal() +
    labs(title = "Total Loss by Year", x = "Year", y = "Total Loss")
  dev.off()
}

# Find best lower threshold for tail fitting
get_best_threshold <- function(df, lower_bound = 0.90, upper_bound = 0.99) {
  best_ad <- Inf
  best_q <- lower_bound
  
  for (q in seq(lower_bound, upper_bound, by = 0.01)) {
    threshold <- quantile(df$loss_amount, q)
    tail_data <- df$loss_amount[df$loss_amount > threshold]
    
    if (length(tail_data) > 10) {
      fit <- tryCatch({
        fitdist(tail_data, "pareto", method = "qme", probs = c(0.5, 0.999))
      }, error = function(e) NULL)
      
      if (!is.null(fit)) {
        ad <- gofstat(fit)$ad
        if (!is.na(ad) && ad < best_ad) {
          best_ad <- ad
          best_q <- q
        }
      }
    }
  }
  return(best_q)
}

# Get threshold at excess
get_threshold_at_excess <- function(df, threshold) {
  return(quantile(df$loss_amount, probs = threshold))
}

# Poisson lambda estimation
get_rpois_lambda <- function(df, use = NULL) {
  if (!is.null(use)) {
    return(use)
  }
  return(mean(table(df$year_of_loss)))
}

# Estimate log-normal for count distribution
get_lnorm_count_dist <- function(df) {
  yearly_counts <- df %>% count(year_of_loss) %>% pull(n)
  fit <- fitdist(yearly_counts, "lnorm")
  return(fit)
}

# Bootstrap mean for empirical reference
get_excess_bootstrap_mean <- function(df, lower, upper, R = 10000) {
  boot_data <- df %>% filter(loss_amount >= lower, loss_amount <= upper) %>% pull(loss_amount)
  boot_mean <- boot(boot_data, statistic = function(d, i) mean(d[i]), R = R)
  return(mean(boot_mean$t))
}
