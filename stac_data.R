#### STAC access ####
#devtools::install_github("ReseauBiodiversiteQuebec/stac-catalogue")
library("stacatalogue")
library("rstac")
library("tibble")
library(sf)
library(dplyr)

# look at all the data sources available on the STAC
stac_path <- "https://io.biodiversite-quebec.ca/stac/"
stac(stac_path) %>%
  collections() %>%
  get_request()

#list of datasources that we want
sources <- list("chelsa-clim", "soilgrids", "earthenv_topography",
                "earthenv_habitat_heterogeneity")
# also landcover from "esacci-lc" but it behaves differently, so let's not deal with it now
# maybe of interest but not sure? "ghmts", "stressors_qc", "silvis",


#get study extent
load(here::here("data/qc.rda"))
#qc_proj <- st_transform(qc, crs = 5070)

# reproject to data cube projection
srs_cube <- "EPSG:6623"
bbox <- stacatalogue::shp_to_bbox(qc,
                                  proj_to = srs_cube)

# get potential sample points
load(here::here("data/qc_cand_points.rda"))
qc_pts <- stacatalogue::project_coords(qc_cand_points %>%
                                         dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                                       lat = sf::st_coordinates(.)[,2]) %>%
                                         st_drop_geometry(),
                                       lon = "lon",
                                       lat = "lat",
                                       proj_from = "EPSG:5070",
                                       proj_to = srs_cube)

#### Exploring data #####
# let's get a dataframe of datasources at variables
source_meta <- purrr::map_dfr(sources, ~get_info_collection(stac_path =
                                                         "http://io.biodiversite-quebec.ca/stac/",
                                                       collections = c(.x),
                                                       bbox = NULL) %>%
                                within(., rm("layers")) %>%
                                as_tibble(.) %>%
                                mutate(source = .x))


info_chelsa_climate<- get_info_collection(stac_path =
                                             "http://io.biodiversite-quebec.ca/stac/",
                                           collections = c('chelsa-clim'),
                                           bbox = NULL)

# # get cube of all chelsea climate variables
chelsea_cube <- load_cube(stac_path = stac_path,
                          collections = c("chelsa-clim"),
                          bbox = bbox,
                          limit = 5000,
                          srs.cube = srs_cube,
                          variable = info_chelsa_climate$variables,
                          spatial.res = 2000, # in meters
                          temporal.res = "P1Y",
                          aggregation = "mean",
                          resampling = "near")

# get list of cubes from the sources we want
cube_list <- purrr::map(sources, ~load_cube(stac_path = stac_path,
                                           collections = c(.x),
                                           bbox = bbox,
                                           limit = 10000,
                                           srs.cube = srs_cube,
                                           #variable = info_chelsa_climate$variables,
                                           #spatial.res = 2000, # in meters
                                           temporal.res = "P1Y",
                                           aggregation = "mean",
                                           resampling = "near"))

test_values <- gdalcubes::extract_geom(cube_list[[1]], sf::st_as_sf(qc_pts[1:10,]))

