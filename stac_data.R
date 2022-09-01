#### STAC access ####
#https://github.com/ReseauBiodiversiteQuebec/stac-catalogue/tree/main/vignettes
#devtools::install_github("ReseauBiodiversiteQuebec/stac-catalogue")
library("stacatalogue")
library(rstac)
library(tibble)
library(gdalcubes)
library(httr)
library(sf)
library(dplyr)
library(purrr)

# look at all the data sources available on the STAC
stac_path <- "https://io.biodiversite-quebec.ca/stac/"
stac(stac_path) %>%
  collections() %>%
  get_request()

# metadata for a single data source
stac(stac_path) %>%
  collections('ghmts') %>%
  get_request()

#list of datasources that we want
# also landcover from "esacci-lc" but it behaves differently, so let's not deal with it now
# maybe of interest but not sure? "ghmts", "stressors_qc", "silvis",
sources <- list("chelsa-clim", "soilgrids", "earthenv_topography",
                "earthenv_habitat_heterogeneity")#, "ghmts",  "stressors_qc")

source_meta <- purrr::map_dfr(sources, ~get_info_collection(stac_path =
                                                              "http://io.biodiversite-quebec.ca/stac/",
                                                            collections = c(.x),
                                                            bbox = NULL) %>%
                                within(., rm("layers")) %>%
                                as_tibble(.) %>%
                                mutate(source = .x))

# or get info for a single source
info_chelsa_climate<- get_info_collection(stac_path =
                                            "http://io.biodiversite-quebec.ca/stac/",
                                          collections = c('chelsa-clim'),
                                          bbox = NULL)


srs_cube <- "EPSG:6623"
bounds <- c(xmin = -80.0, xmax = -50.0,
            ymax =  65.0, ymin = 40.0 )

bbox <- bounds %>%
  sf::st_bbox(crs = 4326) %>%
  sf::st_as_sfc() %>%
  sf::st_transform(crs=srs_cube) %>%
  sf::st_bbox()

# load(here::here("data/qc.rda"))
# st_bbox(qc)

get_STAClayer <- function(collection_name, asset_name, t0, t1,
                          file_path, bbox, cube_projection){

  print(paste0(collection_name, asset_name))

  it_obj <- stac(stac_path) %>%
    stac_search(bbox=c(bounds['xmin'],bounds['ymin'],bounds['xmax'],bounds['ymax']),
                collections=c(collection_name),limit=5000) %>% get_request()

  #id and asset name should match, but for some sources the name is only stored in the id,
  # which `stac_image_collection()` doesn't like, so have to rename the assets
  # this works for escci-lc, ghmts, chelsa-clim, earthenv, and silvis
  # approach other layers with caution
  for (i in 1:length(it_obj$features)){
    name <- it_obj$features[[i]]$id
    names(it_obj$features[[i]]$assets) <- name
  }

  v <- cube_view(srs = cube_projection,
                extent = list(t0 = t0, t1 = t1,
                              left = bbox['xmin'], right = bbox['xmax'],
                              top = bbox['ymax'], bottom = bbox['ymin']),
                dx = 1000, dy = 1000, dt = "P1Y",aggregation = "mean", resampling = "mode")

  st<-stac_image_collection(it_obj$features,asset_names=c(asset_name))

  gdalcubes_options(parallel= TRUE)

  rc <- raster_cube(st, v)
  rc %>% write_tif(dir = file_path, prefix = paste0(collection_name, asset_name, sep = "_"))
}


# get_STAClayer(bbox = bounds, cube_projection = srs_cube, collection_name = "esacci-lc", asset_name = "esacci-lc-2020",
#                           file_path = path, t0 = "2020-01-01", t1 = "2020-12-31")

# get list of all the variables we want for many-layered datasets
chelsa_vars <- paste0("bio", 1:19)

layers_df <- tibble(
  collection_name = c("esacci-lc", "ghmts", rep("chelsa-clim", length(chelsa_vars)), rep("earthenv_topography", 2), rep("silvis", 2)),
  asset_name = c("esacci-lc-2020", "GHMTS", chelsa_vars, "slope", "elevation", "NDVI16_cumulative", "NDVI16_seasonality"),
  t0 = c("2020-01-01", "2016-01-01", rep("1981-01-01", length(chelsa_vars)), rep("2010-01-01", 2), rep("2003-01-01", 2)),
  t1 = c("2020-12-31", "2016-12-31", rep("1981-01-01", length(chelsa_vars)), rep("2010-01-01", 2), rep("2003-01-01", 2))
)

path <- here::here("data/env_layers")
pmap(layers_df, get_STAClayer, file_path = path, bbox = bbox, cube_projection = srs_cube)

# read all layers in as a raster stack
layer_files <- paste(path, list.files(path), sep = "/")
env_stack <- raster::stack(layer_files)

#get points
load(here::here("data/qc_cand_points.rda"))
qc_cand_proj <- qc_cand_points %>%
  st_transform(crs = 6623)

env_pts_data <- raster::extract(env_stack, qc_cand_proj, df = TRUE) %>%
  left_join(qc_cand_points, by = c("ID" = "id"))

usethis::use_data(env_pts_data)
