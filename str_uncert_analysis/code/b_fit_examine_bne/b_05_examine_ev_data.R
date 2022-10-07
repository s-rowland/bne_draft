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


# isolate to the three wonky data points 
ev.wonky <- ev %>% 
  filter(name == 'MAP' & yyyy == 2012 | 
           name == 'EDI' & yyyy == 2012 | 
           name == 'Saint Lawrence County' & yyyy == 2014)

# look at MAP 
ev.map <- ev.wonky %>% 
  filter(name == 'Saint Lawrence County')

a <- ev %>% 
  filter(name == 'MAP' & yyyy == 2010)
