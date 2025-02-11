library("raster")
library("dplyr")

source(here::here("data_raw/binaryLayerFunc.R"))
lc_classes <- input$lc_classes
select_class <- [60, 210]
threshold_prop <- 0.8


# Running binary function
lc_binary <- binary_layer(lc_classes, select_class, threshold_prop)


# Saving rasters

for(i in 1:length(names(lc_binary))){
raster::writeRaster(x = lc_binary[[i]],
                    paste0(outputFolder, "/", names(lc_binary[[i]]), ".tif"),
                    format='COG',
                    options=c("COMPRESS=DEFLATE"),
                    overwrite = TRUE
)
}

print(list.files(outputFolder, pattern = ".tif$", full.names = T))

lc_binary_layer <- list.files(outputFolder, pattern="*.tif$", full.names = T)

output <- list("output_binary" = lc_binary_layer)
jsonData <- toJSON(output, indent=2)
write(jsonData, file.path(outputFolder,"output.json"))


