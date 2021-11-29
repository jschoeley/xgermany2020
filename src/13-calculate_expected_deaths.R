# Fit expected deaths models

# Init ------------------------------------------------------------

set.seed(1987)

library(here); library(glue)
library(yaml)
library(tidyverse)
library(foreach)
library(doParallel)

# Constants -------------------------------------------------------

wd <- here(); setwd(wd)

# paths
path <- list(
  mod_def = 'tmp/mod_def.rds',
  mod_para = 'tmp/mod_para.rds',
  country_metadata = 'src/region_metadata.csv',
  config = 'src/config.yaml',
  glob = 'src/00-global_objects.R',
  log = 'tmp/fitted_models_log.txt',
  mocy_cv = 'tmp/mocy_cv.rds',
  expected_deaths_cv = 'tmp/expected_deaths_cv.rds'
)

# global functions and constants
source(path$glob)

# model specifications
ModDef <- readRDS(path$mod_def)
# model parametrizations
mod_para <- readRDS(path$mod_para)

# info on regions
region_metadata <- read_csv(path$country_metadata)

# constants
cnst <- list()
cnst <- within(cnst,{
  config = read_yaml(path$config)
  # how many threads used to fit models?
  cpu_nodes = 16
  # how many draws from the posterior predictive distribution?
  ndraws = 100
  # weeks to train on for models with partial fit
  countries_north  = filter(region_metadata, hemisphere == 'n')$region_code
  weeks_for_training_north = c(15:26, 36:45)
  countries_south  = filter(region_metadata, hemisphere == 's')$region_code
  weeks_for_training_south = c(1:20, 42:52)
})

dat <- list()
fig <- list()

# setup parallel computation
cnst$cl <- makeCluster(cnst$cpu_nodes, outfile = path$log)
registerDoParallel(cnst$cl)

# Load data -------------------------------------------------------

# load data for cross validation
mocy_cv <- readRDS(path$mocy_cv)

# Prepare data for fit --------------------------------------------

# merge data with model definitions
dat$fit_data <-
  mocy_cv %>%
  filter(region_iso %in% cnst$config$skeleton$region) %>%
  nest(data = c(-region_iso, -cv_id)) %>%
  expand_grid(mod_para)

# Fit models and predict ------------------------------------------

# fit models
dat$fitted_models <-
  # iterate in parallel over region and CV id
  foreach(
    x = iter(dat$fit_data, by = 'row'),
    .combine = bind_rows,
    .packages = c('dplyr', 'tidyr', 'INLA')
  ) %dopar% {suppressPackageStartupMessages({
    
    cat(format(Sys.time(), '%Y-%m-%d %H:%M:%S'),
        ' Fit ', x$region_iso,
        ' on CV set ', x$cv_id,
        ' for ', x$model_id, '\n', sep = '')
    
    # prepare training and prediction data
    # single region, cv id and model
    input_dat <- x[,'data'][[1]][[1]]
    # model parametrization
    model_para <- x$model_para[[1]]

    # fit models and capture errors
    result <- tryCatch({
      
      # fit GLM model
      
      if (x$model_class == 'glm') {
        
        predictions <- ModDef$SerflingGLM(
          df = input_dat,
          models = model_para$models,
          family = model_para$family,
          col_sample = 'cv_sample',
          col_stratum = 'stratum_id',
          weeks_for_training = model_para$weeks_for_training,
          col_week = 'iso_week',
          n_years_for_training = model_para$n_years_for_training,
          col_year = 'iso_year',
          nsim = cnst$ndraws, simulate_beta = TRUE, simulate_y = TRUE
        )
        
      }
      
      # fit GAM model
      
      if (x$model_class == 'gam') {
        
        predictions <- ModDef$CountGAM(
          df = input_dat,
          formula = model_para$formula,
          family = model_para$family,
          col_sample = 'cv_sample',
          col_stratum = 'stratum_id',
          n_years_for_training = model_para$n_years_for_training,
          col_year = 'iso_year',
          nsim = cnst$ndraws, simulate_beta = TRUE, simulate_y = TRUE,
          method = 'REML'
        )
        
      }
      
      # fit INLA model

      if (x$model_class == 'lgm') {
        
        predictions <- ModDef$KontisLGM(
          df = input_dat, formula1 = model_para$formula,
          formula2 = model_para$formula_alt, formula2_threshold = 60,
          col_stratum = 'stratum_id', col_sample = 'cv_sample',
          col_death = 'deaths_observed', col_exposure = 'personweeks',
          col_tanomaly = 'temperature_anomaly', col_week = 'iso_week',
          col_time = 'origin_weeks', col_holiday = 'holiday3',
          nsim = cnst$ndraws,
          weeks_for_training_within_year = NULL,
          weeks_for_training_pre_test = NULL,
          # because the iteration is already across multiple cores
          # make INLA only use a single core, this actually speeds-up
          # the whole fitting procedure
          threads = 1
        )
        
      }
      
      # return result if fitting succeeded
      
      result_if_no_error <- bind_cols(
        x,
        tibble(
          predictions = list(predictions),
          error_while_fit = FALSE,
          error_message = NA
        )
      )
      return(result_if_no_error)
      
    },
    
    # return result if fitting did not succeed
    
    error = function(e) {
      cat(format(Sys.time(), '%Y-%m-%d %H:%M:%S'),
          ' Error', x$region_iso, ' on CV set ', x$cv_id,
          ' for ', x$model_id, ': ', geterrmessage(), '\n')
      # return same object as fitted model, but with NA predictions
      input_dat[,c('deaths_predicted',paste0('deaths_sim', 1:cnst$ndraws))] <- NA
      result_if_error <- bind_cols(x, tibble(
        predictions = list(input_dat),
        error_while_fit = TRUE,
        error_message = geterrmessage()
      ))
      return(result_if_error)
      
    }) # end of tryCatch()
    
    return(result)
    
  })}

stopCluster(cnst$cl)

# Plot observed vs. fitted ----------------------------------------

dat$fitted_models %>%
  filter(!error_while_fit) %>%
  group_by(region_iso, model_id) %>%
  group_walk(~{
    
    predictions <- unnest(.x, predictions)
    expected_deaths <-
      predictions %>%
      group_by(cv_id, date, cv_sample) %>%
      summarise(
        deaths_observed = sum(deaths_observed),
        deaths_predicted = sum(deaths_predicted)
      ) %>%
      ungroup()
    simulated_deaths <-
      predictions %>%
      pivot_longer(cols = starts_with('deaths_sim'),
                   names_to = 'sim_id', values_to = 'deaths_sim') %>%
      group_by(cv_id, date, cv_sample, sim_id) %>%
      summarise(
        deaths_sim = sum(deaths_sim)
      ) %>%
      group_by(cv_id, date, cv_sample) %>%
      summarise(
        q05 = quantile(deaths_sim, 0.05, na.rm = TRUE),
        q95 = quantile(deaths_sim, 0.95, na.rm = TRUE)
      ) %>%
      ungroup()
    
    fig_dat <- left_join(expected_deaths, simulated_deaths)

    fig[[paste(.y[[1]], .y[[2]])]] <<-
      fig_dat %>%
      ggplot(aes(x = date)) +
      geom_ribbon(aes(ymin = q05, ymax = q95),
                  fill = 'grey70', color = NA) +
      geom_point(aes(color = cv_sample, y = deaths_observed),
                 size = 0.3) +
      geom_line(aes(y = deaths_predicted, alpha = cv_sample),
                color = 'red') +
      scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
      scale_alpha_manual(values = c(training = 0.3, test = 1)) +
      scale_color_manual(values = figspec$colors$sample) +
      facet_grid(cv_id~'') +
      guides(color = 'none', alpha = 'none') +
      figspec$MyGGplotTheme(grid = 'xy') +
      labs(
        x = NULL, y = 'Weekly deaths',
        title = paste(.y[[1]], .y[[2]])
      )
  })

# Exports ---------------------------------------------------------

saveRDS(dat$fitted_models, file = path$expected_deaths_cv)

ggsave(
  filename = 'fitted_vs_observed_cv.pdf',
  path = 'tmp',
  plot = gridExtra::marrangeGrob(fig, nrow=1, ncol=1), 
  width = 15, height = 9
)
