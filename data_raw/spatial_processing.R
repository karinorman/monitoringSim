library(dplyr)
library(tidyr)
library(sf)

#### Process Spatial Data ####

# sf::sf_use_s2(FALSE)

## regional boundaries ##

# Canada
cdn <- getData("GADM",country="can",level=0) %>%
  st_as_sf() %>%
  st_make_valid()

usethis::use_data(cdn)

# Quebec
cdn_regions <- getData("GADM",country="can",level=1) %>%
  st_as_sf()

qc <- cdn_regions %>% filter(NAME_1 == "Québec") %>%
  st_make_valid()

usethis::use_data(cdn)
##############################

#### IUCN data ####

#read in IUCN data
ranges <- st_read(here::here("data_store/iucn_terrestrial_mammals/MAMMALS_TERRESTRIAL_ONLY.shp")) %>%
  rename(order = "order_") %>%
  st_make_valid()

# Bat data
bats <- ranges %>% filter(order == "CHIROPTERA") %>%
  st_make_valid()

#crop bats to canada
cdn_bats <- st_intersection(bats, cdn)

# crop to QC
qc_bats <- st_intersection(bats, qc)
usethis::use_data(qc_bats)

#get canadian mammals, use buffer to fix self-intersection errors
ranges_buff <- st_buffer(ranges, 0)
qc_mammals <- st_intersection(ranges_buff, qc)

usethis::use_data(qc_mammals)
##################

#### WWF ecoregions ####
ecoreg <- st_read(here::here("data_store/wwf_ecoregions/wwf_terr_ecos.shp"))
