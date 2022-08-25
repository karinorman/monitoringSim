library("gdalcubes")
library("rstac")
library("tibble")

s = stac("https://io.biodiversite-quebec.ca/stac/")
srs_cube <- "EPSG:6623"
bounds <- c(xmin = -80.0, xmax = -50.0,
            ymax =  65.0, ymin = 45.0 )

bbox <- bounds %>%
  sf::st_bbox(crs = 4326) %>%
  sf::st_as_sfc() %>%
  sf::st_transform(crs=srs_cube) %>%
  sf::st_bbox()


### Land cover data

it_obj <- s %>%
  stac_search(bbox=c(bounds['xmin'],bounds['ymin'],bounds['xmax'],bounds['ymax']),
              collections=c("esacci-lc"),limit=5000) %>% get_request()

st<-stac_image_collection(it_obj$features,asset_names=c("esacci-lc-2020"))

v = cube_view(srs = srs_cube,
              extent = list(t0 = "2020-01-01", t1 = "2020-12-31",
                            left = bbox['xmin'], right = bbox['xmax'],
                            top = bbox['ymax'], bottom = bbox['ymin']),
              dx = 1000, dy = 1000, dt = "P1Y",aggregation = "mean", resampling = "mode")

gdalcubes_options(parallel= TRUE)

rc <- raster_cube(st, v)

rc  %>% plot()

rc %>% write_tif(dir = here::here("juliaPackage/"), prefix = "esacci_lc")

#### Chelsa Clim ####
it_obj <- s %>%
  stac_search(bbox=c(bounds['xmin'],bounds['ymin'],bounds['xmax'],bounds['ymax']),
              collections=c("chelsa-clim"),limit=5000) %>% get_request()

st_bio1 <-stac_image_collection(it_obj$features,asset_names=c("bio1"))
st_bio12 <-stac_image_collection(it_obj$features,asset_names=c("bio12"))

v = cube_view(srs = srs_cube,
              extent = list(t0 = "1981-01-01", t1 = "1981-01-01",
                            left = bbox['xmin'], right = bbox['xmax'],
                            top = bbox['ymax'], bottom = bbox['ymin']),
              dx = 1000, dy = 1000, dt = "P1Y",aggregation = "mean", resampling = "mode")

gdalcubes_options(parallel= TRUE)

rc_bio1 <- raster_cube(st_bio1, v)
rc_bio12 <- raster_cube(st_bio12, v)

rc_bio12  %>% plot()

rc_bio1 %>% write_tif(dir = here::here("juliaPackage/"), prefix = 'chelsa_clim_bio1')
rc_bio12 %>% write_tif(dir = here::here("juliaPackage/"), prefix = 'chelsa_clim_bio12')


### Global Human Modification of Terrestrial Systems ###

it_ghmts <- s %>%
  stac_search(bbox=c(bounds['xmin'],bounds['ymin'],bounds['xmax'],bounds['ymax']),
              collections=c("ghmts"),limit=5000) %>% get_request()

st_ghmts<-stac_image_collection(it_ghmts$features,asset_names=c("GHMTS"))

v = cube_view(srs = srs_cube,
              extent = list(t0 = "2016-01-01", t1 = "2016-12-31",
                            left = bbox['xmin'], right = bbox['xmax'],
                            top = bbox['ymax'], bottom = bbox['ymin']),
              dx = 1000, dy = 1000, dt = "P1Y",aggregation = "mean", resampling = "mode")

gdalcubes_options(parallel= TRUE)

rc <- raster_cube(st_ghmts, v)

rc  %>% plot()

rc %>% write_tif(dir = here::here("juliaPackage/"), prefix = "ghmts")
