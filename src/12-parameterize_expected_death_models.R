# Parameterize expected death models
#
# A list specifying all the models to be tested, and their
# parametrizations. See specify_models.R for the exact
# implementation of the models.

# Init ------------------------------------------------------------

library(dplyr)
library(here)

wd <- here(); setwd(wd)

path <- list(
  mod_para = 'tmp/mod_para.rds'
)

# Specifications of models to test --------------------------------

# Just a big list specifying all the models to be tested,
# and their parametrizations. See specify_models.R for the exact
# implementation of the models.

mod_para <-
  tribble(
    
    ~model_id, ~model_class, ~model_para,
    
    # # 5 year average death rates
    'AVR', 'gam', list(
      formula = formula(
        deaths_observed ~
          as.factor(iso_week) +
          offset(log(personweeks))
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 5
    ),

    # 5 year average death counts
    'AVC', 'gam', list(
      formula = formula(
        deaths_observed ~
          # single coefficient for every 4 weeks
          as.factor(iso_week)
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = 5
    ),
    
    # Serfling style regression
    # Forecasting Serfling with exposures
    # AIC selection of seasonality
    'SRF', 'glm', list(
      models = list(
        formula(
          deaths_observed ~
            # log linear long term trend
            origin_weeks +
            # seasonality
            # full year period
            sin(2*pi/52*iso_week) +
            cos(2*pi/52*iso_week) +
            # half year period
            sin(2*pi/26*iso_week) +
            cos(2*pi/26*iso_week) +
            # exposures
            offset(log(personweeks))
        ),
        formula(
          deaths_observed ~
            # log linear long term trend
            origin_weeks +
            # seasonality
            # full year period
            sin(2*pi/52*iso_week) +
            cos(2*pi/52*iso_week) +
            # exposures
            offset(log(personweeks))
        ),
        formula(
          deaths_observed ~
            # log linear long term trend
            origin_weeks +
            # exposures
            offset(log(personweeks))
        )
      ),
      family = quasipoisson(link = 'log'),
      weeks_for_training = NULL,
      n_years_for_training = NULL
    ),
    
    # Count GAM
    'GAM', 'gam', list(
      formula = formula(
        deaths_observed ~
          # log linear long term trend
          origin_weeks +
          # penalized cyclic spline for seasonality
          s(epi_week, bs = 'cp', k = 12, fx = FALSE) +
          # temperature effect
          #s(epi_week, bs = 'cp', k = 12, fx = FALSE, by = temperature_anomaly) +
          # adjustment for special weeks
          s(holiday, bs = 're') +
          # exposures
          offset(log(personweeks))
      ),
      family = quasipoisson(link = 'log'),
      n_years_for_training = NULL
    ),
    
    # Kontis' latent Gaussian autoregressive seasonal Poisson regression
    'LGM', 'lgm', list(
      formula = formula(
        death ~
          1 +
          global_slope +
          holiday +
          f(time_ar,
            model = 'ar1',
            hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
          ) +
          f(time_seas,
            model = 'seasonal', season.length = 52,
            hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
          ) +
          # independent remaining errors
          f(resid_iid,
            model = 'iid',
            hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
          ) +
          offset(log(exposure))
      ),
      # simpler formula for low sample size strata
      formula_alt = formula(
          death ~
            1 +
            global_slope +
            f(time_ar,
              model = 'ar1',
              hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
            ) +
            # independent remaining errors
            f(resid_iid,
              model = 'iid',
              hyper = list(prec = list(prior = 'loggamma', param = c(0.001, 0.001)))
            ) +
            offset(log(exposure))
        ),
      weeks_for_training_within_year = NULL,
      weeks_for_training_pre_test = 7
    )
    
  )

# Export ----------------------------------------------------------

saveRDS(mod_para, file = path$mod_para)
