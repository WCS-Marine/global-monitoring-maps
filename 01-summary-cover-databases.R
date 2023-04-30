library(tidyverse)
library(janitor)
library(here)


#load final CC exports from Julia
data <- read_csv(here("data/total_cc.csv")) %>% 
  clean_names()
data

data %>% 
  tabyl(db) %>% 
  arrange(-n)

data %>% 
  tabyl(country, cc_site)

data %>% 
  tabyl(db, cc_site)





#how many object id pixels have multiple monitoring efforts? 
data %>% 
  group_by(objectid) %>% 
  summarize(n = paste(db, collapse = ", ")) %>% 
  tabyl(n)



#objectid is 5x5km andrello pixel, right? 
data %>% 
  select(objectid) %>% 
  distinct(objectid) %>% 
  arrange(objectid) %>% 
  summarize(max(objectid)) #max is 54,596 = number of pixels in andrello et al. 2022


data %>% 
  select(objectid) %>% 
  distinct() %>% 
  nrow()

data %>% 
  tabyl(cc_site)

data %>% 
  tabyl(db) %>% 
  arrange(-n)



#summaries
data %>% 
  select(objectid, 
         cc_site) %>% 
  distinct() %>% 
  tabyl(cc_site) #3% of coral reef 5-km pixels are surveyed

#by country, total pixels
data %>% 
  select(country, 
         objectid, 
         cc_site) %>% 
  filter(cc_site == 1) %>% 
  tabyl(country) %>% 
  arrange(-n)

#by country, proportion of pixels
#0 cc_sites don't have countries
data %>% 
  filter(cc_site == 0)


  