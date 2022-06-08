#' Equal probability monitoring networks
#'
#' @param sf_df sf dataframe defining the sample frame
#' @param n number of sites to be selected
#'
#' @return list of point indexes for each algorithm
get_eqprob <- function(sf_df, n) {

  N <- dim(sf_df)[1]
  pik <- rep(n/N,N) #vector of inclusion probabilities

  x <- as.matrix(cbind(sf::st_coordinates(sf_df)[,1],
                       sf::st_coordinates(sf_df)[,2]))

  # #get matrix of vertices for a bounding box of the sample area
  # bb <- st_bbox(sf_df)
  # bmat <- matrix(c(bb$xmin, bb$ymin,
  #                  bb$xmin, bb$ymax,
  #                  bb$xmax, bb$ymax,
  #                  bb$xmax, bb$ymax),
  #                ncol = 2, byrow = TRUE)

  #Simple random sample
  srs <- srswor(n, N)

  #GRTS
  grts_fit <- grts(sf_df, n_base = n)

  #Cube sampling
  cube_fit <- samplecube(x, pik, comment = TRUE, method = 1)

  #SCPS
  scps_fit <- scps(pik, x)

  #LPM
  lpm1_fit <- lpm1(pik, x)
  lpm2_fit <- lpm2(pik, x)

  # #BAS
  # bas_fit <- quasiSamp(n, dimension = 2, potential.sites = x,
  #                      study.area = bmat, inclusion.probs = pik)

  list(srs = which(srs == 1), grts = grts_fit$sites_base$id, cube = which(cube_fit == 1),
       scps = scps_fit, lpm1 = lpm1_fit, lpm2 = lpm2_fit) %>%
    indexlist_to_mat() %>%
    as.data.frame() %>%
    mutate(id = row_number()) %>%
    left_join(sf_df)
}

indexlist_to_mat <- function(x){
  n.cols <- length(x)
  n.ids <- sapply(x,length)
  n.rows <- max(unlist(x))
  out <- matrix(0,nrow=n.rows,ncol=n.cols)

  id <- unlist(x)+rep(0:(n.cols-1),n.ids)*n.rows
  out[id] <- 1
  colnames(out) <- names(x)

  return(out)
}
