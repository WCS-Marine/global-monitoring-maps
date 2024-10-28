#This script compiles a set of global coral reef monitoring site coordinations from major monitoring efforts
library(here)
library(sf)
library(tidyverse)
library(janitor)




#load Andrello 54,596 reef cell polygons
load(here("data", "wcs-local-reef-pressures", "allreefs_WGS84.RData"))

allreefs <- allreefs %>% 
  select(OBJECTID, geometry)

allreefs

#compile survey site lat-longs

# Load coral cover sites from Whitney compilation (4,766 sites) ---- 
cc_sites <- read_csv(here("data","cc_sites.csv"))
cc_sites

#remove MERMAID - replace with more recent MERMAID
cc_sites <- cc_sites %>% 
  filter(db != "mermaid") 

# List # of sites per database included (n = 4766 sites)
cc_sites %>% 
  tabyl(db) %>% 
  adorn_totals()

# more recent MERMAID sites (Oct 27, 2024)
#install.packages("remotes")

#remotes::install_github("data-mermaid/mermaidr")
library(mermaidr)

sites <- mermaid_get_sites()
sites

mermaid_sites <- sites %>% 
  select(country, 
         name, 
         latitude, 
         longitude) %>% 
  distinct() %>% 
  mutate(db = "mermaid") %>% 
  rename("site" = name) %>% 
  select(db, 
         country:longitude)

#Reef Life Survey sites
reef_life <- read_csv(here("data", 
                           "reef_life_site_info.csv"))

rls <- reef_life %>% 
  mutate(db = "rls") %>% 
  select(db, 
         country, 
         site_name, 
         latitude, 
         longitude) %>% 
  rename("site" = site_name)

rls

# Reef Check
reef_check <- read_csv(here("data", 
                            "reef_check_all.csv")) %>% 
  clean_names()

names(reef_check)
head(reef_check)

reef_check <- reef_check %>% 
  mutate(db = "reefcheck") %>% 
  select(db, 
         #country, 
         location, 
         reef_check_id,
         lat, 
         long) %>% 
  rename("country" = location, 
         "site" = reef_check_id, 
         "latitude" = lat, 
         "longitude" = long)

reef_check


# GCRMN
gcrmn <- read_csv(here("data", 
                       "gcrmn_site_coords.csv"))

gcrmn 

gcrmn <- gcrmn %>% 
  mutate(db = "gcrmn") %>% 
  select(db, 
         lat, 
         lon) %>% 
  rename("latitude" = lat, 
         "longitude" = lon)

gcrmn


# Nat Geo Pristine Seas
nat_geo <- read_csv(here("data", 
                         "nat_geo.csv")) %>% 
  clean_names()

nat_geo <- nat_geo %>% 
  mutate(db = "nat_geo") %>% 
  select(db, 
         reef,
         lat, 
         long) %>% 
  rename("country" = reef, 
         "latitude" = lat, 
         "longitude" = long)

nat_geo

#ReefCloud
reefcloud <- read_csv(here("data","ReefCloud_Sites_Sep2024.csv")) %>% 
  clean_names()

reefcloud <- reefcloud %>% 
  mutate(db = "reefcloud") %>% 
  select(db, 
         sovereign1,
         latitude, 
         longitude) %>% 
  rename("country" = sovereign1)
 
reefcloud 


#compile all datasets 
all_surveys <- cc_sites %>% 
  bind_rows(gcrmn) %>% 
  bind_rows(mermaid_sites) %>% 
  bind_rows(nat_geo) %>% 
  bind_rows(reef_check) %>% 
  bind_rows(rls) %>% 
  bind_rows(reefcloud) %>% 
  distinct()

all_surveys

all_surveys %>% 
  tabyl(db) %>% 
  arrange(-n)

#convert to sf / spatial

all_surveys_sf <- all_surveys %>% 
  st_as_sf(coords = c("longitude","latitude"), 
           crs = 4326, remove = F) 

# compareCRS(allreefs, all_surveys_sf)
# ??compareCRS

#overlap with 54,596 all reefs
#overlap is df with each lat-long site mapped to an OBJECTID 5-km reef cell

overlap <- st_join(allreefs, all_surveys_sf) %>% 
  as_tibble() %>% 
  mutate(OBJECTID = as.integer(OBJECTID))
  #drop_na(OBJECTID)

overlap

#overlap includes all 54,596 distinc OBJECTIDs

head(overlap)
overlap %>%  
  select(OBJECTID) %>% 
  distinct(OBJECTID)

#summary! 
n_surveys_per_cell <- overlap %>% 
  filter(!is.na(db)) %>% 
  group_by(OBJECTID) %>%
  count(db)

n_surveys_per_cell

overlap %>% 
  filter(OBJECTID == 14) #e.g., Bermuda has 10 gcrmn surveys and 1 uq_photoquadrat survey

#summarize n_db per cell
cells_with_surveys <- n_surveys_per_cell %>% 
  group_by(OBJECTID) %>% 
  count() %>% 
  arrange(OBJECTID)

cells_with_surveys
nrow(cells_with_surveys)
#before reefcloud, 6,559 cells have at least one survey from a db
#now, 7611 cells have at least one suvey

#13.9 % of 54,596 cells
nrow(cells_with_surveys) / 54596

overlap_summary <- overlap %>% 
  tabyl(OBJECTID) %>% 
  arrange(-n)

overlap_summary

hist(overlap_summary$n)
summary(overlap_summary$n)

overlap_summary %>% 
  filter(n > 5000) #8518 OBJECT IDs have NA

#summarize overlap by db
overlap %>% 
  tabyl(db) %>% 
  adorn_totals() %>% 
  adorn_pct_formatting()

#save compiled all sites with lat longs
write_csv(all_surveys, 
          here("data", "all-survey-lat-longs.csv"))


# Static World Map
theme_set(theme_bw())
#install.packages("rnaturalearthdata")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

all_surveys_sf

#plot all sites by database
#note includes sites in temperate (Canada) and some typos in continents
#clip only to lat-longs in reef cells? 
#add reef cells as under-layer? 

ggplot(data = world) +
  geom_sf() +
  # coord_sf(default_crs = sf::st_crs(4326), expand = FALSE) +
  #  annotation_scale(plot_unit = "km", location = "bl", width_hint = 0.2) +
  #annotation_north_arrow(location = "bl", which_north = "true", 
                         #pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         #style = north_arrow_fancy_orienteering) +
  geom_sf(data = all_surveys_sf, aes(fill = db), 
          size = 1, shape = 21, show.legend = "point") +
  scale_fill_viridis_d(alpha = 0.4) +
  scale_color_viridis_d(alpha = 0.4) +
  labs(x="Longitude", y="Latitude", color = "Databases", 
       fill = element_blank()) +
  labs(x = element_blank(), y = element_blank())

ggsave("Static-CC-Map.pdf", height = 5, width = 10)

#update with Whitney code to show reef cell boxes with dots inside? 


# ggplot(data = world) +
#   geom_sf() +
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
#                          style = north_arrow_fancy_orienteering) +
#   geom_sf(data = sf_ng_overlap, aes(fill = sf_ng_overlap$db, 
#                                        color = sf_ng_overlap$db, 
#                                        order = sf_ng_overlap$db), 
#           size = 1, show.legend = "point") +
#   scale_fill_manual(values=alpha(c("#fde725","#a0da39","#4ac16d","#1fa187",
#                                    "#277f8e","#365c8d","#46327e","#440154"), 0.4), 
#                     na.value = alpha("#D3D3D3", 0.2)) +
#   scale_color_manual(values=alpha(c("#fde725","#a0da39","#4ac16d","#1fa187",
#                                     "#277f8e","#365c8d","#46327e","#440154"), 0.4), 
#                      na.value = alpha("#D3D3D3", 0.2)) +
#   labs(x="Longitude", y="Latitude", color = "Databases", fill = element_blank()) +
#   labs(x = element_blank(), y = element_blank())
# 
# ggsave("Static-CC-Map2.pdf", height = 5, width = 10)
# 
# # Pacific Map
# library(maps)
# library(maptools)
# 
# pt1 = st_point(c(-170, 50))
# pt2 = st_point(c(170, 50))
# (sfc = st_sfc(pt1, pt2))
# 
# sfc = st_set_crs(sfc, 4326)
# st_shift_longitude(sfc)
# 
# d = st_as_sf(data.frame(id = 1:2, geometry = sfc))
# st_shift_longitude(d)
# 
# overlap_shift <- st_shift_longitude(sf_ng_overlap)
# 
# ggplot() +
#   geom_sf() +
#   geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), 
#                aes(x=long, y = lat, group=group))+
#   annotation_north_arrow(location = "bl", which_north = "true",
#                          height = unit(0.5, "cm"),
#                          width = unit(0.5, "cm"),
#                          style = north_arrow_fancy_orienteering) +
#   geom_sf(data = overlap_shift, aes(fill = overlap_shift$db, 
#                                     color = overlap_shift$db), size = 1, 
#           show.legend = "point") +
#   scale_fill_viridis_d(alpha = 0.4) +
#   scale_color_viridis_d(alpha = 0.4) +
#   labs(x="Longitude", y="Latitude", color = "Databases", fill = element_blank()) +
#   labs(x = element_blank(), y = element_blank())
# 
# ggsave("Pacific-Map.pdf", height = 4, width = 6)

