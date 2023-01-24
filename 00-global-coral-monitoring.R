library(raster)
library(rgdal)
library(rgeos)
library(here)
library(sf)
library(matrixStats)
library(tidyverse)
library(corrgram)
library(cowplot)
library(googleway)
library(ggrepel)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(janitor)
library(RColorBrewer)
library(leaflet)
library(leaflet.esri)
library(htmlwidgets)
library(readxl)
library(janitor)
library(biogeo)

allreefs_info <- read_excel(here("data","wcs-local-reef-pressures",
                                 "key.xlsx"))

allreefs_info

# load dataset (original allreefs.Rdata transformed using "01-format-allreefs.R")

load(here("data", "wcs-local-reef-pressures", "allreefs_WGS84.RData"))

allreefs <- allreefs %>% 
  select(OBJECTID, geometry)

allreefs

# Load coral cover sites (4,766 sites) ---- 
cc_sites <- read_csv(here("data","cc_sites.csv"))

# List # of sites per database included (n = 4766 sites)
cc_sites %>% tabyl(db) %>% adorn_totals()

cc_sites_sf <- cc_sites %>% 
  st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = F) 

compareCRS(allreefs, cc_sites_sf)

cc_lst <- st_intersects(allreefs, cc_sites_sf, prepared = TRUE)

cc_sites_lrp <- st_join(cc_sites_sf, allreefs) %>% 
  as_tibble() %>% 
  drop_na(OBJECTID)

cc_sites_lrp %>% 
  tabyl(db) %>% 
  adorn_totals()

lrp_cc <- cc_sites_lrp %>% 
  mutate(cc_site = 1) %>%
  select(OBJECTID, db, country, cc_site) %>% 
  # drop any duplicates
  # NOTE: loose information on multiple db's per quadrat
  distinct(OBJECTID, cc_site, .keep_all = T) %>% # <- put back in ???
  full_join(allreefs, by = "OBJECTID") %>% 
  mutate(cc_site = replace_na(cc_site, 0))

lrp_cc_sf <- st_as_sf(lrp_cc)

# Add reef life
reef_life <- read_csv(here("data", "reef_life_site_info.csv"))

reef_life_sf <- reef_life %>% st_as_sf(coords = c("longitude","latitude"), 
                                       crs = 4326, remove = F) 

compareCRS(allreefs, reef_life_sf)

reef_life_lrp <- st_join(reef_life_sf, allreefs) %>% 
  as_tibble() %>% 
  drop_na(OBJECTID)

compareCRS(lrp_cc_sf, reef_life_sf)

lst <- st_intersects(lrp_cc_sf, reef_life_sf, prepared = TRUE)
overlap <- lengths(lst) > 0
rl_overlap <- lrp_cc %>%
  mutate(reef_life = overlap)
rl_overlap <- rl_overlap %>% mutate(reef_life = as.character(reef_life))
rl_overlap$reef_life

rl_overlap <- rl_overlap %>% mutate(reef_life = recode(reef_life, 
                                                       "TRUE" = "reef_life",
                                                       "FALSE" = "NA"))
rl_overlap <- rl_overlap %>%  mutate(across(c(reef_life), na_if, "NA"))

total_rl_overlap <- rl_overlap %>% mutate(db = coalesce(db, reef_life))
total_rl_overlap %>% tabyl(db) %>% adorn_totals()

sf_rl_overlap <- total_rl_overlap %>% st_as_sf() %>% st_transform(crs = 4326)

# Add reef check
reef_check <- read_csv(here("data", "reef_check_all.csv"))

reef_check_sf <- reef_check %>% st_as_sf(coords = c("Long","Lat"), crs = 4326, 
                                         remove = F) 

compareCRS(allreefs, reef_check_sf)

reef_check_lrp <- st_join(reef_check_sf, allreefs) %>% 
  as_tibble() %>% 
  drop_na(OBJECTID)

compareCRS(sf_rl_overlap, reef_check_sf)

lst_rc <- st_intersects(sf_rl_overlap, reef_check_sf, prepared = TRUE)
overlap_rc <- lengths(lst_rc) > 0
rc_overlap <- sf_rl_overlap %>%
  mutate(reef_check = overlap_rc)
rc_overlap <- rc_overlap %>% mutate(reef_check = as.character(reef_check))
rc_overlap$reef_check

rc_overlap <- rc_overlap %>% mutate(reef_check = recode(reef_check, 
                                                        "TRUE" = "reef_check",
                                                        "FALSE" = "NA"))
rc_overlap$reef_check

rc_overlap <- rc_overlap %>%  mutate(across(c(reef_check), na_if, "NA"))

total_rc_overlap <- rc_overlap %>% mutate(db = coalesce(db, reef_check))
total_rc_overlap %>% tabyl(db) %>% adorn_totals()

sf_rc_overlap <- total_rc_overlap %>% st_as_sf() %>% st_transform(crs = 4326)

sf_rc_overlap %>% tabyl(db) %>% adorn_totals()

# Add GCRMN
gcrmn <- read_csv(here("data", "gcrmn_site_coords.csv"))

gcrmn_sf <- gcrmn %>% select(c(lat, lon)) %>% st_as_sf(coords = c("lon","lat"), 
                                                       crs = 4326, remove = F) 

compareCRS(allreefs, gcrmn_sf)

gcrmn_lrp <- st_join(gcrmn_sf, allreefs) %>% 
  as_tibble() %>% 
  drop_na(OBJECTID)

compareCRS(sf_rc_overlap, gcrmn_sf)

lst_gcrmn <- st_intersects(sf_rc_overlap, gcrmn_sf, prepared = TRUE)
overlap_gcrmn <- lengths(lst_gcrmn) > 0
gcrmn_overlap <- sf_rc_overlap %>%
  mutate(gcrmn = overlap_gcrmn)
gcrmn_overlap <- gcrmn_overlap %>% mutate(gcrmn = as.character(gcrmn))
gcrmn_overlap$gcrmn

gcrmn_overlap <- gcrmn_overlap %>% mutate(gcrmn = recode(gcrmn, 
                                                        "TRUE" = "gcrmn",
                                                        "FALSE" = "NA"))
gcrmn_overlap$gcrmn

gcrmn_overlap <- gcrmn_overlap %>%  mutate(across(c(gcrmn), na_if, "NA"))

total_gcrmn_overlap <- gcrmn_overlap %>% mutate(db = coalesce(db, gcrmn)) %>% 
  mutate(cc_site = case_when(reef_life == "reef_life" | reef_check == "reef_check" |
                               gcrmn == "gcrmn" | cc_site == 1 ~ 1,
                          TRUE ~ cc_site)) %>% 
  dplyr::select(-c(reef_life, reef_check, gcrmn))
total_gcrmn_overlap %>% tabyl(db) %>% adorn_totals()

sf_gcrmn_overlap <- total_gcrmn_overlap %>% st_as_sf() %>% 
  st_transform(crs = 4326)

tabyl_gcrmn <- sf_gcrmn_overlap %>% tabyl(db) %>% adorn_totals() %>% 
  adorn_pct_formatting()

#total_gcrmn_overlap %>% write.csv(here("data", "total_cc.csv"))
#tabyl_gcrmn %>% write.csv(here("data", "percent_cc.csv"))
#sf_gcrmn_overlap %>% write_rds(here("data", "total_cc_sf.RDS"))

# Add nat geo
## change all reef life to nat geo
nat_geo <- read_csv(here("data", "nat_geo.csv"))

nat_geo_sf <- nat_geo %>% st_as_sf(coords = c("Long","Lat"), 
                                       crs = 4326, remove = F) 

compareCRS(allreefs, nat_geo_sf)

nat_geo_lrp <- st_join(nat_geo_sf, allreefs) %>% 
  as_tibble() %>% 
  drop_na(OBJECTID)

compareCRS(sf_gcrmn_overlap, nat_geo_sf)

ng_lst <- st_intersects(sf_gcrmn_overlap, nat_geo_sf, prepared = TRUE)
overlap_ng <- lengths(ng_lst) > 0
ng_overlap <- sf_gcrmn_overlap %>%
  mutate(nat_geo = overlap_ng)
ng_overlap <- ng_overlap %>% mutate(nat_geo = as.character(nat_geo))
ng_overlap$nat_geo

ng_overlap <- ng_overlap %>% mutate(nat_geo = recode(nat_geo, 
                                                       "TRUE" = "nat_geo",
                                                       "FALSE" = "NA"))
ng_overlap <- ng_overlap %>%  mutate(across(c(nat_geo), na_if, "NA"))

total_ng_overlap <- ng_overlap %>% mutate(db = coalesce(db, nat_geo))
total_ng_overlap %>% tabyl(db) %>% adorn_totals()

sf_ng_overlap <- total_ng_overlap %>% st_as_sf() %>% st_transform(crs = 4326)

tabyl_ng <- sf_ng_overlap %>% tabyl(db) %>% adorn_totals() %>% 
  adorn_pct_formatting()

#total_ng_overlap %>% write.csv(here("data", "total_cc.csv"))
#tabyl_ng %>% write.csv(here("data", "percent_cc.csv"))
#sf_ng_overlap %>% write_rds(here("data", "total_cc_sf.RDS"))


# Static World Map
theme_set(theme_bw())

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  # coord_sf(default_crs = sf::st_crs(4326), expand = FALSE) +
  #  annotation_scale(plot_unit = "km", location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = sf_ng_overlap, aes(fill = sf_ng_overlap$db, 
                                       color = sf_ng_overlap$db), 
          size = 1, show.legend = "point") +
  scale_fill_viridis_d(alpha = 0.4) +
  scale_color_viridis_d(alpha = 0.4) +
  labs(x="Longitude", y="Latitude", color = "Databases", 
       fill = element_blank()) +
  labs(x = element_blank(), y = element_blank())

#ggsave("Static-CC-Map.pdf", height = 5, width = 10)


ggplot(data = world) +
  geom_sf() +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = sf_ng_overlap, aes(fill = sf_ng_overlap$db, 
                                       color = sf_ng_overlap$db, 
                                       order = sf_ng_overlap$db), 
          size = 1, show.legend = "point") +
  scale_fill_manual(values=alpha(c("#fde725","#a0da39","#4ac16d","#1fa187",
                                   "#277f8e","#365c8d","#46327e","#440154"), 0.4), 
                    na.value = alpha("#D3D3D3", 0.2)) +
  scale_color_manual(values=alpha(c("#fde725","#a0da39","#4ac16d","#1fa187",
                                    "#277f8e","#365c8d","#46327e","#440154"), 0.4), 
                     na.value = alpha("#D3D3D3", 0.2)) +
  labs(x="Longitude", y="Latitude", color = "Databases", fill = element_blank()) +
  labs(x = element_blank(), y = element_blank())

#ggsave("Static-CC-Map2.pdf", height = 5, width = 10)

# Pacific Map
library(maps)
library(maptools)

pt1 = st_point(c(-170, 50))
pt2 = st_point(c(170, 50))
(sfc = st_sfc(pt1, pt2))

sfc = st_set_crs(sfc, 4326)
st_shift_longitude(sfc)

d = st_as_sf(data.frame(id = 1:2, geometry = sfc))
st_shift_longitude(d)

overlap_shift <- st_shift_longitude(sf_ng_overlap)

ggplot() +
  geom_sf() +
  geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), 
               aes(x=long, y = lat, group=group))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(0.5, "cm"),
                         width = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  geom_sf(data = overlap_shift, aes(fill = overlap_shift$db, 
                                    color = overlap_shift$db), size = 1, 
          show.legend = "point") +
  scale_fill_viridis_d(alpha = 0.4) +
  scale_color_viridis_d(alpha = 0.4) +
  labs(x="Longitude", y="Latitude", color = "Databases", fill = element_blank()) +
  labs(x = element_blank(), y = element_blank())

ggsave("Pacific-Map.pdf", height = 4, width = 6)
