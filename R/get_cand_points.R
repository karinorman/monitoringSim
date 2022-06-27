
#function for get a set of candidate points for the landscape, adapted from `grts_stratum`
#' Create dataframe of candidate points.
#'
#' @param study_frame Sf dataframe of the sample area.
#' @param n Number of sites in the final monitoring network.
#' @param pt_density Density of potential points on the landscape.
#'
#' @return sf dataframe of potential sites

get_cand_points <- function(study_frame, n, pt_density = 10){
  # this code is adapted from the spsurvey::grts_stratum function that is not exported, but selects the points for
  # point, linear, and polygon inputs. In the GRTS function, points are generated independently for each stratum
  # as a function of stratum sample size

  #define how many candidate points we want
  n_size <- as.integer(ceiling(pmin(1e+09, pt_density * n)))

  #if the spatial object is a raster, create a polygon to sample from
  if (class(study_frame)[1] == "RasterLayer"){
    samp_obj <- st_bbox(study_frame) %>%
      st_as_sfc()
  } else samp_obj <- study_frame

  #get random points
  cand_points <- st_sample(samp_obj, size = n_size, type = "hexagonal",
                           exact = TRUE) %>%
    #st_as_sf(as.data.frame(.), crs = st_crs(study_frame)) %>%
    st_as_sf(., crs = st_crs(study_frame)) %>%
    st_cast(to = "POINT") %>%
    filter(!st_is_empty(.)) %>%
    mutate(id = row_number())# %>%
  #st_join(study_frame) #get metadata from the polygon
}
