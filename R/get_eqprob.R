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
    purrr::map_dfr(., ~as.data.frame(.x), .id = "algorithm") %>%
    rename(id = `.x`) %>%
    left_join(sf_df)
}

#' Simulate Equal probability reps
#'
#' @param n number of sites to be selected
#' @param N total number of potential sites
#' @param pt_df sf dataframe of potential sites
#' @param nreps number of simulated replicates
#'
#' @return returns a long-form dataframe of sample points for each replicate and algorithm
get_eqprop_reps <- function(n, N, pt_df, nreps) {

  pik <- rep(n/N,N) #vector of inclusion probabilities
  pt_mat <- as.matrix(cbind(sf::st_coordinates(pt_df)[,1],
                            sf::st_coordinates(pt_df)[,2]))

  purrr::map_dfr(1:nreps,
                 function(rep) {
                   print(c(rep, n))
                   get_eqprob(sf_df = pt_df, pt_mat = pt_mat,
                              n = n, pik = pik) %>%
                     mutate(rep = rep)
                 })
}
