# Evaluate Grid Search
# Fit Annual-CONUS BNE
# BNE Uncertainty Analysis 
# Bayesian Nonparametric Ensemble 
# Sebastian T. Rowland 
# Last updated Oct 24, 2021

####***********************
#### Table of Contents ####
####***********************

#### ------------------------- ####
####  Table of Contents  ####
#### ------------------------- ####

#  0: preparation 
#  1: get monthly means
#  2: save

#### ---------------- ####
####  0: preparation  ####
#### ---------------- ####

# 0a Load package required for this script
if(!exists('ran_a_00')){
  here::i_am('README.md')
  source(here::here('scripts', 'a_set_up', 
                    'a_00_import_packages_set_global_objects.R'))
}

if(!exists('ran_a_00_uncert')){
  here::i_am('README.md')
  source(here::here('str_uncert_analysis', 'code', 
                    '0_00_config_env_uncert_analysis.R'))
}

#### --------------------- ####
####  1: get annual means  ####
#### --------------------- ####

# 1.a bring in data
ev <- read_csv(here::here('str_uncert_analysis', 
                     'data', 'external_validation', 'inputs', 
                     'ev_data_unassigned.csv'), col_types = 'nncncc')

# 1.b compute monthly means
# 1.b.i determine date components of observation 
ev <- ev %>% 
  mutate(ddate = parse_date_time(ddate, 'ymd HMS')) %>% 
  mutate(yyyy = year(ddate), mm = month(ddate), daysInMonth = days_in_month(ddate)) %>% 
  mutate(daysInYear = if_else(yyyy %in% c(2008, 2012, 2016), 366, 365))
# 1.b.ii compute mean for each month
ev.mean.mm <- ev %>% 
  filter(project != 'nyccas') %>%
  group_by(project, name, lat, lon, yyyy, mm, daysInMonth, daysInYear) %>% 
  summarize(mean_obs = mean(obs)) %>% 
  ungroup()

ev.mean.mm <- ev %>% 
  filter(project == 'nyccas') %>%
  group_by(project, name, yyyy, mm, daysInMonth, daysInYear) %>% 
  summarize(mean_obs = mean(obs), 
            lat = mean(lat), 
            lon = mean(lon))  %>% 
  bind_rows(ev.mean.mm) %>% 
  ungroup()

# 1.c restrict to site-year combinations where we have at least one observation per month
# 1.c.i identify site-year combos with at least one obs per month
ev.12mo <- ev.mean.mm %>% 
  group_by(name, yyyy) %>% 
  summarize(Count = n()) %>% 
  filter(Count == 12) %>% 
  mutate(id = paste0(name, '_', yyyy))

# 1.c.ii restrict
ev.mean.mm <- ev.mean.mm %>% 
  mutate(id = paste0(name, '_', yyyy)) %>% 
  filter(id %in% ev.12mo$id)

# 1.d get annual means 
ev.mean.ann <- ev.mean.mm %>% 
  filter(project != 'nyccas') %>%
  mutate(mean_obs_component = mean_obs*daysInMonth/daysInYear) %>% 
  group_by(name, yyyy, lat, lon) %>% 
  summarize(obs = sum(mean_obs_component))

# we need to treat nyccas uniquely, because it is composed of multiple locations 
# for each year - we average measurements aross locations.
ev.mean.ann <- ev.mean.mm %>% 
  filter(project == 'nyccas') %>%
  mutate(mean_obs_component = mean_obs*daysInMonth/daysInYear) %>% 
  group_by(name, yyyy) %>% 
  summarize(obs = sum(mean_obs_component), 
            lat = mean(lat), 
            lon = mean(lon))  %>% 
  bind_rows(ev.mean.ann)

#### --------- ####
####  2: save  ####
#### --------- ####

# 2.a save as csv 
ev.mean.ann %>% 
  write_csv(here::here('str_uncert_analysis', 
                          'data', 'external_validation', 'inputs', 
                          'ev_data_unassigned_annual.csv'))
