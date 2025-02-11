## Load required packages
library("terra")
library("raster")
library("dplyr")
library("stacatalogue")
library("gdalcubes")
library("RCurl")
options(timeout = max(60000000, getOption("timeout")))


### Get Data ###

# Get bounding box
bounds <- c(xmin = -80.0, xmax = -50.0,
            ymax =  65.0, ymin = 40.0)

srs_cube <- "EPSG:5070"

bbox <- bounds %>%
  sf::st_bbox(crs = 4326) %>%
  sf::st_as_sfc() %>%
  sf::st_transform(crs=srs_cube) %>%
  sf::st_bbox()

# n_year <- as.integer(substr(input$t1, 1, 4)) - as.integer(substr(input$t0, 1, 4)) + 1
# temporal_res <- paste0("P", n_year, "Y")


lc_raster <- stacatalogue::load_prop_values(stac_path = "https://io.biodiversite-quebec.ca/stac/",
                                collections = "esacci-lc",
                              bbox = bbox,
                               srs.cube = srs_cube,
                               limit = 5000,
                                t0 = "2010-01-01",
                                t1 = "2010-12-31",
                                spatial.res = 250, # in meters
                                prop = TRUE,
                                prop.res = 1000,
                                select_values = c(60,70,210),
                                temporal.res =  "P1Y")

### Binary Layer ###

source(here::here("data_raw/binaryLayerFunc.R"))
select_class <- [60, 210]
threshold_prop <- 0.8


# Running binary function
lc_binary <- binary_layer(lc_raster, select_class, threshold_prop)


# for(i in 1:length(names(lc_raster))){
#   raster::writeRaster(x = lc_raster[[i]],
#                       paste0(outputFolder, "/", names(lc_raster[[i]]), ".tif"),
#                       format='COG',
#                       options=c("COMPRESS=DEFLATE"),
#                       overwrite = TRUE)
# }
#
#
#
# lc_classes <- list.files(outputFolder, pattern="*.tif$", full.names = T)
#
# output <- list("output_tif" = lc_classes)
# jsonData <- toJSON(output, indent=2)
# write(jsonData, file.path(outputFolder,"output.json"))

