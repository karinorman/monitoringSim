#' Equal probability monitoring networks
#'
#' @param sf_df sf dataframe defining the sample frame
#' @param n number of sites to be selected
#'
#' @return list of point indexes for each algorithm
get_eqprob <- function(sf_df, pt_mat, n, pik) {

  #Simple random sample
  srs <- sampling::srswor(n, N)

  #GRTS
  grts_fit <- spsurvey::grts(sf_df, n_base = n)

  #Cube sampling
  cube_fit <- BalancedSampling::cube(pik, pt_mat)

  #SCPS
  scps_fit <- BalancedSampling::scps(pik, pt_mat)

  #LPM
  lpm1_fit <- BalancedSampling::lpm1(pik, pt_mat)
  lpm2_fit <- BalancedSampling::lpm2(pik, pt_mat)

  # #BAS
  # bas_fit <- quasiSamp(n, dimension = 2, potential.sites = pt_mat,
  #                      study.area = bmat, inclusion.probs = pik)

  list(srs = which(srs == 1), grts = grts_fit$sites_base$id, cube = cube_fit,
       scps = scps_fit, lpm1 = lpm1_fit, lpm2 = lpm2_fit) %>%
    map_dfr(., ~as.data.frame(.x), .id = "algorithm") %>%
    rename(id = `.x`) %>%
    left_join(sf_df)
}


