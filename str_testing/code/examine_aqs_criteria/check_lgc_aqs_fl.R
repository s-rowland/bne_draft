ann <- read_csv(here::here('BNE_inputs', 'ground_truth', 'formatted', 
                           'lgc_annual_data_2000-2016.csv'))

ann.2011 <- ann %>% filter(Year == 2011)
ann.2010 <- ann %>% filter(Year == 2010)

ann.2010 <- ann.2010 %>% filter(State.Name == 'Florida')
ann.2011 <- ann.2011 %>% filter(State.Name == 'Florida')

fl <- ann %>% 
  #filter(State.Name == 'Florida') %>% 
  group_by(Year) %>% 
  summarize(count = sum(State.Name == 'Florida'))

ma <- ann %>% 
  #filter(State.Name == 'Florida') %>% 
  group_by(Year) %>% 
  summarize(count = sum(State.Name == 'Maine'))
