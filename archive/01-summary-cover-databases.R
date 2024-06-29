library(tidyverse)
library(janitor)
library(here)


#load final CC exports from Julia
data <- read_csv(here("data", "total_cc.csv")) %>% 
  clean_names()



data

data %>% 
  tabyl(db) %>% 
  arrange(-n)

data %>% 
  tabyl(country, cc_site)

data %>% 
  tabyl(db, cc_site)





#how many object id pixels are monitored in each database? 
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

#db_summary <- 
  data %>%  
  #tabyl(db, show_na = FALSE) %>% 
  tabyl(db, show_na = TRUE) %>% 
  arrange(-n) 

#write_csv(db_summary, "db_summary.csv")



??tabyl


 #summaries
data %>% 
  select(objectid, 
         cc_site) %>% 
  distinct() %>% 
  tabyl(cc_site) #11.9% of the world's coral reefs (assessed in 5-km pixels) have survey data in moniotoring databases

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
# data %>% 
#   filter(cc_site == 0)


  