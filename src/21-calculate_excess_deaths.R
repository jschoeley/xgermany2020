# Calculate excess deaths under various models

# Init ------------------------------------------------------------

library(here)
library(tidyverse)

wd <- here(); setwd(wd)

# paths
path <- list(
  # global objects
  glob = 'src/00-global_objects.R',
  # observed and expected death counts 2020 and later
  observed_and_expected = 'tmp/expected_deaths_cv.rds',
  # excess deaths
  excess_deaths = 'tmp/excess_deaths.rds',
  out = 'out'
)

# global functions and constants
source(path$glob)

# Load data -------------------------------------------------------

# load data on predicted death counts
observed_and_expected <-
  readRDS(path$observed_and_expected) %>%
  filter(cv_id == 0) %>%
  select(-data, -model_para) %>%
  unnest(predictions) %>%
  filter(cv_sample == 'test') %>%
  select(
    cv_id, region_iso, model_id, obs_id, sex, age_group,
    iso_year, iso_week, date,
    deaths_observed, personweeks, starts_with('deaths')
  )

# Calculate excess death statistics -------------------------------

excess <- list()

# names of columns for expected/simulated deaths
excess$varnames_simdeath <-
  grep('deaths_sim', colnames(observed_and_expected), value = TRUE)
# define quantiles
excess$quantiles1 <- c('q05' = 0.05, 'q25' = 0.25, 'q50' = 0.5, 'q75' = 0.75, 'q95' = 0.95)
excess$quantiles2 <- c('q50' = 0.5, 'q70' = 0.70, 'q90' = 0.9, 'q95' = 0.95, 'q99' = 0.99)

# add totals by age
excess$observed_and_expected <-
  observed_and_expected %>%
  group_by(cv_id, region_iso, model_id, sex, iso_year, iso_week, date) %>%
  group_modify(~{
    .x %>% bind_rows(summarise(.,
      across(c(personweeks, deaths_predicted, deaths_observed, starts_with('deaths_sim')), sum),
      across(c(age_group), ~'Total')))
  }) %>%
  ungroup() %>%
  arrange(cv_id, model_id, region_iso, sex, age_group, date) %>%
  select(-obs_id)

# nest by model id and stratum
excess$observed_and_expected <-
  excess$observed_and_expected %>%
  nest(data = c(iso_year, iso_week, date, personweeks, starts_with('deaths')))

excess$excess_measures <-
  excess$observed_and_expected %>%
  group_by(model_id, region_iso, sex, age_group) %>%
  group_modify(~{
    
    cat(.y[['model_id']], .y[['region_iso']], .y[['sex']],
        as.character(.y[['age_group']]), '\n')
    
    # a time ordered data frame of observed and simulated death counts
    # for a single region, sex, age group and model, starting early 2020
    # 1 row per observation
    X <- .x$data[[1]]
    
    # return data frame of row-wise quantiles over columns of X
    Rowquantiles <- function (X, prob, type = 4, na.rm = TRUE) {
      t(apply(X, 1, quantile, prob = prob, type = type, na.rm = na.rm))
    }
    
    # weekly observed deaths
    obs_wkl <- X$deaths_observed
    # cumulative observed deaths (at end of week)
    obs_cum <- cumsum(obs_wkl)
    
    # simulated weekly expected deaths
    xpc_wkl_sim <- as.matrix(X[,excess$varnames_simdeath])
    # simulated cumulative expected deaths
    xpc_cum_sim <- apply(xpc_wkl_sim, 2, cumsum)
    
    # weekly expected deaths quantiles
    xpc_wkl <- Rowquantiles(xpc_wkl_sim, excess$quantiles1)
    colnames(xpc_wkl) <- paste0('xpc_wkl_', names(excess$quantiles1))
    # cumulative expected deaths quantiles
    xpc_cum <- Rowquantiles(xpc_cum_sim, excess$quantiles1)
    colnames(xpc_cum) <- paste0('xpc_cum_', names(excess$quantiles1))
    
    # weekly excess thresholds
    xtr_wkl <- Rowquantiles(xpc_wkl_sim, prob = excess$quantiles2)
    
    # weekly excess deaths type 1 quantiles
    xc1_wkl <- Rowquantiles(obs_wkl-xpc_wkl_sim, excess$quantiles1)
    colnames(xc1_wkl) <- paste0('xc1_wkl_', names(excess$quantiles1))
    # cumulative excess deaths type 1 quantiles
    xc1_cum <- Rowquantiles(obs_cum-xpc_cum_sim, excess$quantiles1)
    colnames(xc1_cum) <- paste0('xc1_cum_', names(excess$quantiles1))
    
    # weekly excess deaths type 2 quantiles (deaths above threshold)
    xc2_wkl <- obs_wkl-xtr_wkl
    xc2_wkl[xc2_wkl < 0] <- 0
    colnames(xc2_wkl) <- paste0('xc2_wkl_', names(excess$quantiles2))
    # cumulative excess deaths type 2 quantiles
    xc2_cum <- apply(xc2_wkl, 2, cumsum)
    colnames(xc2_cum) <- paste0('xc2_cum_', names(excess$quantiles2))
    
    # simulated weekly expected deaths 0 adjusted
    xpc_wkl_sim_zad <- xpc_wkl_sim
    xpc_wkl_sim_zad[xpc_wkl_sim_zad == 0] <- 1
    # simulated cumulative expected deaths 0 adjusted
    xpc_cum_sim_zad <- xpc_cum_sim
    xpc_cum_sim_zad[xpc_cum_sim_zad == 0] <- 1
    
    # weekly P-score quantiles
    psc_wkl <- Rowquantiles((obs_wkl-xpc_wkl_sim)/xpc_wkl_sim_zad,
                            excess$quantiles1, type = 7)
    psc_wkl <- round(psc_wkl, digits = 3)
    colnames(psc_wkl) <- paste0('psc_wkl_', names(excess$quantiles1))
    # cumulative P-score quantiles
    psc_cum <- Rowquantiles((obs_cum-xpc_cum_sim)/xpc_cum_sim_zad,
                            excess$quantiles1, type = 7)
    psc_cum <- round(psc_cum, digits = 3)
    colnames(psc_cum) <- paste0('psc_cum_', names(excess$quantiles1))
    
    timeseries_of_measures <-
      cbind(
        X[,c('iso_year', 'iso_week', 'personweeks')],
        obs_wkl, obs_cum,
        xpc_wkl, xpc_cum,
        xc1_wkl, xc1_cum,
        xc2_wkl, xc2_cum,
        psc_wkl, psc_cum
      )
    
    return(timeseries_of_measures)
    
  })

# each measure*model in its own column
excess$excess_measures_wide <-
  excess$excess_measures %>%
  ungroup() %>%
  pivot_wider(
    names_from = model_id,
    values_from = c(xpc_wkl_q05:last_col()),
    names_glue = '{.value}_{model_id}'
  ) %>%
  # ensure lowercase varnames
  rename_with(tolower)

# reshuffle columns
excess$excess_measures_for_export <-
  excess$excess_measures_wide[, unlist(c(
  'personweeks',
  # observed death counts
  'obs_wkl', 'obs_cum',
  # excess death statistics
  sapply(c('lgm', 'gam', 'avr', 'avc', 'srf'), function (mod) {
    c(
      grep(paste0('xpc_.+_', mod), names(excess$excess_measures_wide), value = TRUE),
      grep(paste0('xc1_.+_', mod), names(excess$excess_measures_wide), value = TRUE),
      grep(paste0('xc2_.+_', mod), names(excess$excess_measures_wide), value = TRUE),
      grep(paste0('psc_.+_', mod), names(excess$excess_measures_wide), value = TRUE)          
    )
  })
))]

# Export ----------------------------------------------------------

saveRDS(excess$excess_measures_for_export, path$excess)

# Plot XPC --------------------------------------------------------

ps <- list(
  region_iso = 'DE',
  model_id = 'lgm',
  timebase = 'wkl'
)

excess$excess_measures_wide %>%
  filter(region_iso == ps$region_iso) %>%
  mutate(date = ISOWeekDateToDate(iso_year, iso_week)) %>%
  rename(
    q05 = paste0('xpc_', ps$timebase, '_q05_', ps$model_id),
    q25 = paste0('xpc_', ps$timebase, '_q25_', ps$model_id),
    q50 = paste0('xpc_', ps$timebase, '_q50_', ps$model_id),
    q75 = paste0('xpc_', ps$timebase, '_q75_', ps$model_id),
    q95 = paste0('xpc_', ps$timebase, '_q95_', ps$model_id),
    obs = paste0('obs_', ps$timebase)
  ) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(
    aes(ymax = q95, ymin = q05),
    fill = prismatic::clr_lighten('blue', shift = 0.7)
  ) +
  geom_ribbon(
    aes(ymax = q75, ymin = q25),
    fill = prismatic::clr_lighten('blue', shift = 0.5)
  ) +
  geom_line(aes(y = q50)) +
  geom_point(aes(y = obs)) +
  geom_hline(yintercept = 0, color = 'grey20') +
  facet_grid(age_group ~ sex, scales = 'free_y') +
  scale_x_date(
    date_breaks = '1 year', date_labels = '%Y',
    limits = as.Date(c('2020-03-01', '2021-05-01'))
  ) +
  figspec$MyGGplotTheme(axis = '') +
  theme(panel.background = element_rect(fill = 'grey95', color = NA)) +
  labs(
    title = paste(ps$region_iso, ps$model_id),
    y = paste('xpc', ps$timebase)
  )

# Plot xc1 --------------------------------------------------------

ps <- list(
  region_iso = 'DE',
  model_id = 'lgm',
  timebase = 'wkl'
)

excess$excess_measures_wide %>%
  filter(region_iso == ps$region_iso) %>%
  mutate(date = ISOWeekDateToDate(iso_year, iso_week)) %>%
  rename(
    q05 = paste0('xc1_', ps$timebase, '_q05_', ps$model_id),
    q25 = paste0('xc1_', ps$timebase, '_q25_', ps$model_id),
    q50 = paste0('xc1_', ps$timebase, '_q50_', ps$model_id),
    q75 = paste0('xc1_', ps$timebase, '_q75_', ps$model_id),
    q95 = paste0('xc1_', ps$timebase, '_q95_', ps$model_id)
  ) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(
    aes(ymax = q95, ymin = q05),
    fill = prismatic::clr_lighten('blue', shift = 0.7)
  ) +
  geom_ribbon(
    aes(ymax = q75, ymin = q25),
    fill = prismatic::clr_lighten('blue', shift = 0.5)
  ) +
  geom_line(aes(y = q50)) +
  geom_hline(yintercept = 0, color = 'grey20') +
  facet_grid(age_group ~ sex, scales = 'free_y') +
  scale_x_date(
    date_breaks = '1 year', date_labels = '%Y',
    limits = as.Date(c('2020-03-01', '2021-05-01'))
  ) +
  figspec$MyGGplotTheme() +
  labs(
    title = paste(ps$region_iso, ps$model_id),
    y = paste('xc1', ps$timebase)
  )

# Plot xc2 --------------------------------------------------------

ps <- list(
  region_iso = 'DE',
  model_id = 'lgm',
  timebase = 'wkl'
)

excess$excess_measures_wide %>%
  filter(region_iso == ps$region_iso) %>%
  mutate(date = ISOWeekDateToDate(iso_year, iso_week)) %>%
  rename(
    q50 = paste0('xc2_', ps$timebase, '_q50_', ps$model_id),
    q75 = paste0('xc2_', ps$timebase, '_q70_', ps$model_id),
    q90 = paste0('xc2_', ps$timebase, '_q90_', ps$model_id),
    q95 = paste0('xc2_', ps$timebase, '_q95_', ps$model_id)
  ) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(
    aes(ymax = q50, ymin = 0),
    fill = prismatic::clr_lighten('blue', shift = 0.7)
  ) +
  geom_ribbon(
    aes(ymax = q75, ymin = 0),
    fill = prismatic::clr_lighten('blue', shift = 0.5)
  ) +
  geom_ribbon(
    aes(ymax = q90, ymin = 0),
    fill = prismatic::clr_lighten('blue', shift = 0.3)
  ) +
  geom_ribbon(
    aes(ymax = q95, ymin = 0),
    fill = prismatic::clr_lighten('blue', shift = 0.1)
  ) +
  geom_hline(yintercept = 0, color = 'grey20') +
  facet_grid(age_group ~ sex, scales = 'free_y') +
  scale_x_date(
    date_breaks = '1 year', date_labels = '%Y',
    limits = as.Date(c('2020-03-01', '2021-01-01'))
  ) +
  figspec$MyGGplotTheme() +
  labs(
    title = paste(ps$region_iso, ps$model_id),
    y = paste('xc2', ps$timebase)
  )

# Total P-scores under different models ---------------------------

ps <- list(
  region_iso = 'DE',
  timebase = 'cum',
  measure = 'psc'
)

de_cumpsc <-
  excess$excess_measures %>%
  filter(region_iso == ps$region_iso, age_group == 'Total') %>%
  mutate(date = ISOWeekDateToDate(iso_year, iso_week)) %>%
  rename(
    q05 = paste0(ps$measure, '_', ps$timebase, '_q05'),
    q25 = paste0(ps$measure, '_', ps$timebase, '_q25'),
    q50 = paste0(ps$measure, '_', ps$timebase, '_q50'),
    q75 = paste0(ps$measure, '_', ps$timebase, '_q75'),
    q95 = paste0(ps$measure, '_', ps$timebase, '_q95')
  ) %>%
  ggplot(aes(x = date)) +
  geom_ribbon(
    aes(ymax = q95, ymin = q05),
    fill = prismatic::clr_lighten('blue', shift = 0.7)
  ) +
  geom_ribbon(
    aes(ymax = q75, ymin = q25),
    fill = prismatic::clr_lighten('blue', shift = 0.5)
  ) +
  geom_line(aes(y = q50)) +
  geom_hline(yintercept = 0, color = 'grey20') +
  facet_grid(sex ~ model_id) +
  scale_x_date(
    date_breaks = '2 months', date_labels = '%b',
    limits = as.Date(c('2020-03-01', '2020-12-31'))
  ) +
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.05), labels = scales::percent) +
  figspec$MyGGplotTheme() +
  labs(
    x = NULL,
    title = 'Cumulative percent excess death March 2020 through December 2020 in Germany',
    y = 'Cumulative P-score',
    caption = '95 and 50% prediction intervals\nAVC: 5 year avg. weekly death counts\nAVR: 5 year avg. weekly death rates\nGAM: Quasi-Poisson regression with smooth seasonality and control for mortality trends and population structure\nLGM: Quasi-Poisson regression with smooth seasonality, autocorrelation, and control for mortality trends and population structure\nSRF: Quasi-Poisson regression in Serfling specification trained on all weeks with control for population structure'
  )

ExportFigure(
  de_cumpsc, path = path$out, filename = 'de_cumpsc',
  device = 'pdf',
  width = figspec$fig_dims$width,
  height = figspec$fig_dims$width*0.8, scale = 1.2
)
