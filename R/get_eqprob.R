#' Equal probability monitoring networks
#'
#' @param pt_df sf dataframe defining the sample frame
#' @param n number of sites to be selected
#'
#' @return list of point indexes for each algorithm
get_eqprob <- function(pt_df, n, pik, aux_df) {

  #Simple random sample
  srs <- sampling::srswor(n, N)

  #GRTS
  grts_fit <- spsurvey::grts(pt_df, n_base = n)



    pt_mat <- as.matrix(cbind(sf::st_coordinates(pt_df)[,1],
                              sf::st_coordinates(pt_df)[,2]))

  #Cube sampling
  cube_fit <- BalancedSampling::cube(pik, pt_mat)

  #SCPS
  scps_fit <- BalancedSampling::scps(pik, pt_mat)

  #LPM
  lpm1_fit <- BalancedSampling::lpm1(pik, pt_mat)
  lpm2_fit <- BalancedSampling::lpm2(pik, pt_mat)

  if(!is.null(aux_df)){

    pt_aux <- pt_df %>%
      mutate(lat = sf::st_coordinates(pt_df)[,1],
             lon = sf::st_coordinates(pt_df)[,2]) %>%
      st_drop_geometry() %>%
      select(lat, lon, id) %>%
      left_join(aux_df) %>%
      as.matrix()

    #Cube sampling
    cube_fit_aux <- BalancedSampling::cube(pik, pt_aux)

    #SCPS
    scps_fit_aux <- BalancedSampling::scps(pik, pt_aux)

    #LPM
    lpm1_fit_aux <- BalancedSampling::lpm1(pik, pt_aux)
    lpm2_fit_aux <- BalancedSampling::lpm2(pik, pt_aux)

    results <- list(srs = which(srs == 1), grts = grts_fit$sites_base$id,
                    cube = cube_fit, scps = scps_fit, lpm1 = lpm1_fit, lpm2 = lpm2_fit,
                    cube_aux = cube_fit_aux, scps_aux = scps_fit_aux, lpm1_aux = lpm1_fit_aux,
                    lpm2_aux = lpm2_fit_aux) %>%
      purrr::map_dfr(., ~as.data.frame(.x), .id = "algorithm") %>%
      rename(id = `.x`) %>%
      left_join(pt_df)

    return(results)
  }

  results <- list(srs = which(srs == 1), grts = grts_fit$sites_base$id, cube = cube_fit,
       scps = scps_fit, lpm1 = lpm1_fit, lpm2 = lpm2_fit) %>%
    purrr::map_dfr(., ~as.data.frame(.x), .id = "algorithm") %>%
    rename(id = `.x`) %>%
    left_join(pt_df)

  return(results)
}

#' Simulate Equal probability reps
#'
#' @param n number of sites to be selected
#' @param N total number of potential sites
#' @param pt_df sf dataframe of potential sites
#' @param nreps number of simulated replicates
#'
#' @return returns a long-form dataframe of sample points for each replicate and algorithm
get_eqprop_reps <- function(n, N, pt_df, nreps, aux_df = NULL) {

  pik <- rep(n/N,N) #vector of inclusion probabilities

  purrr::map_dfr(1:nreps,
                 function(rep) {
                   print(c(rep, n))
                   get_eqprob(pt_df = pt_df, n = n,
                              pik = pik, aux_df = aux_df) %>%
                     mutate(rep = rep)
                 })
}
