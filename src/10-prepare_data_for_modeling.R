# Prepare MOCY data for model fitting and cross-validation
#
# Find MOCY source data at https://github.com/jschoeley/mocy
#
# ## id variables
#
# - cv_id:
#   identifies cross-validation series;
#   id 0 contains the complete series
# - obs_id:
#   identifies rows across CV series; 
#   pattern: <region_iso, sex, age_start, iso_year, iso_week>
# - stratum_id:
#   identifies unique sex and age combinations
#
# ## strata
#
# - region_iso:
#   ISO 3166-1 alpha-2 country code when region is nation state;
#   ISO 3166-2 region code when region is subdivision of nation state
# - sex:
#   Male and Female
# - age_start:
#   integer start of age group [0, 15, 65, 75, 85]
# - age_width:
#   width of age group; Inf for open age group
# - age_group:
#   age interval for age group
# - iso_year:
#   year as defined in ISO 8601 week date system
# - iso_week:
#   week as defined in ISO 8601 week date system [0, 52];
#   leap-weeks 53 dropped
#
# ## observations
#
# - deaths_observed:
#   number of deaths from any cause
# - population:
#   population count (only observed in week containing January 1st)
# - personweeks:
#   person-weeks of exposure
# - holiday:
#   'none' if week does not feature a public holiday, otherwise name
#   of holiday
# - holiday2:
#   'none' if week does not feature a public holiday, otherwise 'public'
# - holiday3:
#   'none' if week does not feature a public holiday, otherwise 'easter',
#   'newyear', 'christmas', or 'other'
# - temperature:
#   population weighted average temperature in given week and region
#   in degrees Celsius
# - temperature_anomaly:
#   difference between weekly population weighted average temperature
#   in a given region and the expected temperature that week derived
#   from averaging over the training period
#
# ## additional calendar variables
#
# - date:
#   starting date of epi-week
# - epi_year:
#   Epidemiologic year (starts at iso-week 27)
# - epi_year_int:
#   integer year representation of epidemiologic year
# - epi_year_date:
#   starting date of epi-year
# - epi_week:
#   Epidemiologic week;
#   starts at iso-week 27, counts from 0 and increases by unit steps
#   skipping eventual leap-week 53
# - origin_date:
#   starting date of cv series
# - origin_weeks:
#   completed weeks since start of cv series
# - origin_date_test:
#   starting date of test data
# - weeks_since_test_start:
#   weeks since start of test
#
# ## additional region information
# 
# - region_name:
#   natural name of region
# - region_level:
#   0 for nation state / country level, 1 for subdivision
# - country_iso:
#   ISO 3166-1 alpha-2 country code
# - country_name:
#   natural name of country
# - hemisphere:
#   (n)orth or (s)outh
# - continent:
#   continent of region
#
# ## flags
#
# - cv_sample:
#   does this data point belong to the CV id's 'training' or 'test' set?
# - cv_full_series:
#   are complete 5 fold CV series available for this country?
#   'TRUE' or 'FALSE'

# Init ------------------------------------------------------------

library(tidyverse); library(lubridate); library(yaml)
library(here)

wd <- here(); setwd(wd)
dat <- list()
fig <- list()

path <- list(
  input_data = 'dat/mocy.Rdata',
  country_metadata = 'src/region_metadata.csv',
  global_constants = 'src/00-global_objects.R',
  mocy_cv = 'tmp/mocy_cv.rds',
  config = 'src/config.yaml'
)

cnst <- list(
  region_metadata = read_csv(path$country_metadata),
  config = read_yaml(path$config)
)

# global functions and constants
source(path$global_constants)

# Load data -------------------------------------------------------

# time series of weekly death counts by age, sex, and region plus covariates
load(path$input_data)

# Add time related variables --------------------------------------

dat$weekly_mortality <-
  mocy %>%
  rename(iso_week = week, iso_year = year) %>%
  mutate(
    # date at start of week assuming weeks start on Monday
    date =
      ISOWeekDateToDate(iso_year, iso_week, 1),
    # date that corresponding epi-year started
    epi_year_date =
      ISOWeekDateToDate(
        year = ifelse(
          iso_week < cnst$config$forecast$start_of_epi_year_iso_week,
          iso_year-1, iso_year
        ),
        week = cnst$config$forecast$start_of_epi_year_iso_week,
        weekday = 1
      ),
    # starting iso-year of current epi-year
    epi_year_int =
      as.integer(isoyear(epi_year_date)),
    # current epi-year
    epi_year =
      paste0(epi_year_int,'/',epi_year_int+1),
    # weeks into epi-year (starting at 0)
    epi_week =
      ISOWeekToEpiWeek(
        iso_week, w_start = cnst$config$forecast$start_of_epi_year_iso_week,
        # the input data skips leap weeks and assumes each year
        # to have full 52 weeks
        w_53 = FALSE
      )
  )

# Add additional factor variables ---------------------------------

dat$weekly_mortality <-
  dat$weekly_mortality %>%
  mutate(
    stratum_id =
      as.factor(substr(id, 7, 9)) %>% fct_relevel('F00'),
    epi_week_fac =
      as.factor(epi_week) %>% fct_relevel('0'),
    age_group =
      paste0('[', age_start, ',', age_start+age_width , ')') %>%
      as.factor() %>% fct_relevel('[0,15)'),
    holiday =
      as.factor(holiday) %>% fct_relevel('none'),
    holiday2 =
      ifelse(holiday == 'none', 'none', 'public') %>%
      as.factor() %>% fct_relevel('none'),
    holiday3 =
      case_when(
        grepl('Christmas', holiday) ~ 'christmas',
        grepl('Easter|(Good Friday)', holiday) ~ 'easter',
        grepl('New Year', holiday) ~ 'newyear',
        holiday != 'none' ~ 'other',
        TRUE ~ 'none'
      ) %>%
      fct_relevel('none')
  )

# Prepare cross-validation data sets ------------------------------

# define cross-validation series
# training: [training_start, test_start)
# test: [test_start, test_end]
dat$cv_selection <-
  tibble(
    cv_id = 1:3,
    training_start =
      ISOWeekDateToDate(
        2007+0:2,
        week = cnst$config$forecast$start_of_epi_year_iso_week
      ),
    test_start =
      ISOWeekDateToDate(
        2015+0:2,
        week = cnst$config$forecast$start_of_test_iso_week
      ),
    test_end = 
      test_start + weeks(cnst$config$forecast$forecast_n_weeks)
  )
# add the complete data series as cv_id 0
dat$total_selection <-
  tibble(
    cv_id = 0,
    training_start =
      ISOWeekDateToDate(
        2007, week = cnst$config$forecast$start_of_epi_year_iso_week
      ),
    test_start =
      ISOWeekDateToDate(
        2020, week = cnst$config$forecast$start_of_test_iso_week
      ),
    test_end = 
      test_start + weeks(cnst$config$forecast$forecast_n_weeks)
  )
dat$selection <-
  bind_rows(
    dat$total_selection, dat$cv_selection
  )

# countries with complete data for CV split
# complete means no missings in any of the "core" variables, i.e.
# deaths & personweeks
dat$country_selection <-
  dat$weekly_mortality %>%
  drop_na(deaths, personweeks) %>%
  group_by(region_iso) %>%
  summarise(
    min_year = min(iso_year)
  ) %>%
  filter(min_year <= 2007) %>%
  pull(region_iso)

# test-training data
dat$weekly_mortality_cv <-
  dat$selection %>%
  group_by(cv_id) %>%
  group_modify(~{
    
    # retrieve training data
    training <-
      filter(dat$weekly_mortality, date >= .x$training_start, date < .x$test_start) %>%
      mutate(cv_sample = 'training')
    # retrieve test data
    test <-
      filter(dat$weekly_mortality, date >= .x$test_start, date <= .x$test_end) %>%
      mutate(cv_sample = 'test')
    
    # calculate average weekly population weighted temperature by region
    # over the training years
    avg_weekly_temp_training <-
      training %>%
      # temperature only differs by date and region and
      # repeats over the sex and age strata, therefore we ignore
      # these strata when forming the mean
      group_by(region_iso, iso_week) %>%
      summarise(
        temp_c_popwgt_avgtrain = mean(temp_c_popwgt)
      ) %>%
      ungroup()
    
    bind_rows(training, test) %>%
      left_join(
        avg_weekly_temp_training, by = c('region_iso', 'iso_week')
      ) %>%
      # add date of series origin, weeks since origin and
      # date of test data start
      mutate(
        origin_date = .x$training_start,
        origin_weeks = WeeksSinceOrigin(date, .x$training_start),
        origin_date_test = .x$test_start,
        weeks_since_test_start = WeeksSinceOrigin(date, .x$test_start),
        temp_c_popwgt_anomaly = temp_c_popwgt - temp_c_popwgt_avgtrain
      )
  }) %>%
  # designate incomplete cv series
  mutate(cv_full_series = region_iso %in% dat$country_selection) %>%
  # remove incomplete cv series
  filter(cv_full_series | cv_id == 0) %>%
  arrange(cv_id, region_iso, sex, age_start, date) %>%
  ungroup()

# Select and rename -----------------------------------------------

dat$ready_for_export <-
  dat$weekly_mortality_cv %>%
  select(
    # id variables
    cv_id = cv_id,
    obs_id = id,
    stratum_id = stratum_id,
    # strata
    region_iso = region_iso,
    sex = sex,
    age_start = age_start,
    age_width = age_width,
    age_group = age_group,
    iso_year = iso_year,
    iso_week = iso_week,
    # observations
    deaths_observed = deaths,
    population = population,
    personweeks = personweeks,
    holiday = holiday,
    holiday2 = holiday2,
    holiday3 = holiday3,
    temperature = temp_c_popwgt,
    temperature_anomaly = temp_c_popwgt_anomaly,
    # additional calendar variables
    date = date,
    epi_year = epi_year,
    epi_year_int = epi_year_int,
    epi_year_date = epi_year_date,
    epi_week = epi_week,
    origin_date = origin_date,
    origin_weeks = origin_weeks,
    origin_date_test = origin_date_test,
    weeks_since_test_start = weeks_since_test_start,
    # additional region information
    region_name = region_name,
    region_level = region_level,
    country_iso = country_iso,
    country_name = country_name,
    hemisphere = hemisphere,
    continent = continent,
    # flags
    cv_sample = cv_sample,
    cv_full_series = cv_full_series
  ) %>%
  arrange(cv_id, obs_id)

# Validation plots ------------------------------------------------

# plot training-test split
fig$training_test_split <-
  dat$ready_for_export %>%
  mutate(
    isna = is.na(deaths_observed) | is.na(personweeks) | is.na(holiday) | is.na(temperature)
  ) %>%
  filter(age_group == '[85,Inf)', sex == 'Male') %>%
  ggplot(aes(x = date, y = cv_id)) +
  geom_path(
    aes(color = cv_sample, size = isna,
        group = interaction(cv_id, cv_sample)),
    alpha = 1
  ) +
  facet_wrap(~region_iso, ncol = 5) +
  scale_x_date(date_breaks = '2 year', date_labels = '%y', expand = c(0, 0)) +
  scale_y_continuous(breaks = 0:5, labels = c('0 (Total)', 1:5)) +
  labs(x = 'Year', y = 'CV ID') +
  guides(color = 'none', size = 'none') +
  scale_color_manual(values = c(figspec$colors$sample)) +
  scale_size_manual(values = c(`TRUE` = 0.5, `FALSE` = 2)) +
  labs(
    title = 'Data coverage and training-test split',
    subtitle = 'Test grey, training red. Thin lines indicate NAs in death, exposure, holiday or temperature variables.'
  ) +
  figspec$MyGGplotTheme(panel_border = TRUE, grid = 'x', minor_grid = 'x')
fig$training_test_split

# Export ----------------------------------------------------------

saveRDS(dat$ready_for_export, file = path$mocy_cv)
