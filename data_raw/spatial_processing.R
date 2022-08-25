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
cdn_regions <- raster::getData("GADM",country="can",level=1) %>%
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
ecoreg <- st_read(here::here("data/wwf_ecoregions/wwf_terr_ecos.shp"))

########################

#### Landcover data ####

#Reproject raster using GDAL from the terminal, so much faster
#gdalwarp -t_srs EPSG:5070 CAN_NALCMS_2015_v2_land_cover_30m/CAN_NALCMS_2015_v2_land_cover_30m.tif can_lc_proj.tif

########################

#### WorldClim #########

# download worldclim data
getData("worldclim", var = "tmax", res = 2.5, path = here::here("data"))
getData("worldclim", var = "tmin", res = 2.5, path = here::here("data"))
getData("worldclim", var = "prec", res = 2.5, path = here::here("data"))
getData("worldclim", var = "bio", res = 2.5, path = here::here("data"))

# get list of raster layer files
wc_files <- list.files(here::here("data/wc2-5"), pattern = "*.bil$")

# reproject all the raster files using GDAL from the terminal
folder <- "wc2-5_proj"
dir.create(here::here("data", folder))

for (i in wc_files){
  cmd <- paste0("gdalwarp -t_srs EPSG:5070 ", here::here("data/wc2-5", i), " ",
                here::here("data", folder, i))
  system(cmd)
}

# create raster stack of projected files
clim_stack <- stack(here::here("data/wc2-5_proj", wc_files))

usethis::use_data(clim_stack)
#######################
