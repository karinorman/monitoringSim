#' Equal probability monitoring networks with stratification
#'
#' @param pt_df sf dataframe of potential sample points
#' @param pik vector of inclusion probabilities
#' @param n sample size
#' @param strata_n named list of samples for each stratum
#' @param strata_col unquoted column name for strata labels
#'
#' @return returns a dataframe of points for each algorithm
get_eqprob_strat <- function(pt_df, pik, n, strata_n, strata_col, aux_df){
  
  strata_col_string <- select(pt_df, {{strata_col}}) %>%
    st_drop_geometry() %>%
    colnames()
  
  #simple random sampling stratified
  srs <- sampling::strata(data = pt_df %>% select({{strata_col}}, id) %>% arrange({{strata_col}}), stratanames = c(strata_col_string),
                          size = strata_n, method = "srswor")
  
  # GRTS
  grts_strat <- spsurvey::grts(pt_df %>% select({{strata_col}}), n_base = strata_n, stratum_var = strata_col_string)
  
  
  
  # CUBE
  # matrix of coordinates
  pt_mat <- as.matrix(cbind(sf::st_coordinates(pt_df)[,1],
                            sf::st_coordinates(pt_df)[,2]))
  
  #cube_strat <- BalancedSampling::cubestratified(prob = pik, x = pt_mat, integerStrata = pull(pt_df, {{strata_col}}))
  
  # try_num = 0
  # while(try_num != 500){
  #   try_out <- try( BalancedSampling::cubestratified(prob = pik, x = pt_mat, integerStrata = pull(pt_df, {{strata_col}})))
  #   
  #   if (class(try_out) == "try-error"){
  #     try_num <- try_num + 1
  #     print(paste("trying sample size", n, ", attempt", try_num))
  #   } else {
  #     cube_strat <- try_out
  #     break
  #   }
  # }
  
  # SCPS
  scps_strat <- pt_df %>%
    group_by({{strata_col}}) %>%
    group_map(~{
      prob <- .x$inc_prob
      x <- as.matrix(sf::st_coordinates(.x)[,1], sf::st_coordinates(.x)[,2])
      
      return(BalancedSampling::scps(prob, x))
    }) %>% unlist()
  
  
  lpm1_strat <- pt_df %>%
    group_by({{strata_col}}) %>%
    group_map(~{
      prob <- .x$inc_prob
      x <- as.matrix(sf::st_coordinates(.x)[,1], sf::st_coordinates(.x)[,2])
      
      return(BalancedSampling::lpm1(prob, x))
    }) %>% unlist()
  
  lpm2_strat <- pt_df %>%
    group_by({{strata_col}}) %>%
    group_map(~{
      prob <- .x$inc_prob
      x <- as.matrix(sf::st_coordinates(.x)[,1], sf::st_coordinates(.x)[,2])
      
      return(BalancedSampling::lpm2(prob, x))
    }) %>% unlist()
  
  #SCPS Unequal stratification
  scps_uneq <- BalancedSampling::scps(pik, pt_mat)
  
  #LPM Unequal stratification
  lpm1_uneq <- BalancedSampling::lpm1(pik, pt_mat)
  lpm2_uneq <- BalancedSampling::lpm2(pik, pt_mat)
  
  if(!is.null(aux_df)){
    
    pt_df_aux <- pt_df %>%
      mutate(lat = sf::st_coordinates(pt_df)[,1],
             lon = sf::st_coordinates(pt_df)[,2]) %>%
      st_drop_geometry() %>%
      #select(lat, lon, id) %>%
      left_join(aux_df)
    
    # CUBE
    # matrix of coordinates
    # cube_strat_aux <- BalancedSampling::cubestratified(prob = pik, x = as.matrix(select(pt_df_aux, lat, lon, starts_with("L"))),
    #                                                    integerStrata = pull(pt_df, {{strata_col}}))
    
    # try_num = 0
    # while(try_num != 500){
    #   try_out <- try(BalancedSampling::cubestratified(prob = pik, x = as.matrix(select(pt_df_aux, lat, lon, starts_with("L"))),
    #                                                   integerStrata = pull(pt_df, {{strata_col}})))
    #   if (class(try_out) == "try-error"){
    #     try_num <- try_num + 1
    #     print(paste("trying sample size", n, ", attempt", try_num))
    #   } else {
    #     cube_strat_aux <- try_out
    #     break
    #   }
    # }
    # SCPS
    scps_strat_aux <- pt_df_aux %>%
      group_by({{strata_col}}) %>%
      group_map(~{
        prob <- .x$inc_prob
        x <- as.matrix(select(.x, lat, lon, starts_with("L")))
        
        return(BalancedSampling::scps(prob, x))
      }) %>% unlist()
    
    lpm1_strat_aux <- pt_df_aux %>%
      group_by({{strata_col}}) %>%
      group_map(~{
        prob <- .x$inc_prob
        x <- as.matrix(select(.x, lat, lon, starts_with("L")))
        
        return(BalancedSampling::lpm1(prob, x))
      }) %>% unlist()
    
    lpm2_strat_aux <- pt_df_aux %>%
      group_by({{strata_col}}) %>%
      group_map(~{
        prob <- .x$inc_prob
        x <- as.matrix(select(.x, lat, lon, starts_with("L")))
        
        return(BalancedSampling::lpm2(prob, x))
      }) %>% unlist()
    
    pt_aux <- select(pt_df_aux, lat, lon, starts_with("L"))
    
    #SCPS Unequal stratification
    scps_uneq_aux <- BalancedSampling::scps(pik, pt_aux)
    
    #LPM Unequal stratification
    lpm1_uneq_aux <- BalancedSampling::lpm1(pik, pt_aux)
    lpm2_uneq_aux <- BalancedSampling::lpm2(pik, pt_aux)
    
    results <- list(srs = srs$ID_unit, grts = grts_strat$sites_base$id,
                    #cube = which(cube_strat == 1), 
                    scps = scps_strat,
                    lpm1 = lpm1_strat, lpm2 = lpm2_strat,
                    #cube_aux = which(cube_strat_aux == 1), 
                    scps_aux = scps_strat_aux,
                    lpm1_aux = lpm1_strat_aux, lpm2_aux = lpm2_strat_aux,
                    scps_uneq = scps_uneq, lpm1_uneq = lpm1_uneq, lpm2_uneq = lpm2_uneq,
                    scps_uneq_aux = scps_uneq_aux, lpm1_uneq_aux = lpm1_uneq_aux, lpm2_uneq_aux = lpm2_uneq_aux) %>%
      map_dfr(., ~as.data.frame(.x), .id = "algorithm") %>%
      rename(tempid = `.x`) %>%
      left_join(pt_df) %>%
      select(-tempid)
    
    return(results)
    
  }
  
  results <- list(srs = srs$ID_unit, grts = grts_strat$sites_base$id, cube = which(cube_strat == 1),
                  scps = scps_strat, lpm1 = lpm1_strat, lpm2 = lpm2_strat) %>%
    map_dfr(., ~as.data.frame(.x), .id = "algorithm") %>%
    rename(tempid = `.x`) %>%
    left_join(pt_df) %>%
    select(-tempid)
  
  return(results)
}

#' Simulate stratified equal probability reps
#'
#' @param n sample size
#' @param strat_df dataframe of potential points including strata labels and percent area for each stratum
#' @param nreps number of replicates
#' @param strata_col unquoted column name for strata labels
#' @param area_col unqoted column name for the area variable
get_strata_reps <- function(n, strat_df, nreps, strata_col, area_col, aux_df = NULL) {

  #get count of number of potential points in each stratum (N_stratum)
  stratum_counts <- strat_df %>%
    count({{strata_col}}, name = "stratumN") %>%
    st_drop_geometry()

  num_strata <- dim(stratum_counts)[1]

  # get inclusion probabilities for samples proportional to stratum size
  #equal_samp <- rep(n/num_strata, num_strata)
  prop_samp <- strat_df %>%
    select({{strata_col}}, {{area_col}}) %>%
    st_drop_geometry() %>%
    distinct() %>%
    mutate(sample_size = as.integer(round({{area_col}} * n))) %>% # get number of samples per strata
    left_join(stratum_counts) %>%
    mutate(inc_prob = sample_size/stratumN) # calculate inclusion probabilities, N here is N_stratum, all potential points within a stratum, not total N

  # add inclusion probabilities and sample sizes back to the original dataframe
  ecoregion_pts <- strat_df %>%
    left_join(prop_samp %>%
                select(-{{area_col}}) %>%
                distinct())

  # get sample size for each strata
  strata_df <- ecoregion_pts %>%
    st_drop_geometry() %>%
    select({{strata_col}}, sample_size) %>%
    distinct()

  # dataframe of stratum with no samples allocated
  missing_ecoregion <- strata_df %>% filter(sample_size == 0)

  # warn that some ecoregions don't have samples, drop those samples from the dataframe
  if(dim(missing_ecoregion)[1] > 0){
    warning(paste0("There are ", dim(missing_ecoregion)[1], " ecoregions that have not been allocated any samples for sample size ", n, "."))

    inc_ecoreg_pts <- ecoregion_pts %>%
      filter(!{{strata_col}} %in% pull(missing_ecoregion, {{strata_col}}))

    strata_df <- filter(strata_df, sample_size != 0)
  } else inc_ecoreg_pts <- ecoregion_pts

  # calculate the bespoke things each algorithm needs

  # named list of sample sizes
  strata_n <- strata_df %>%
    arrange({{strata_col}}) %>%
    tibble::deframe()

  # vector of inclusion probabilities
  pik <- inc_ecoreg_pts$inc_prob

  # make sure there's a complete ID column even if we had to drop data
  inc_ecoreg_pts <- inc_ecoreg_pts %>%
    mutate(tempid = row_number())

  sim_reps <- purrr::map_dfr(1:nreps,
                 function(rep) {
                   get_eqprob_strat(pt_df = inc_ecoreg_pts, pik = pik,
                                    n = n, strata_n = strata_n, strata_col = {{strata_col}},
                                    aux_df = aux_df) %>%
                     mutate(rep = rep)
                 }) %>% mutate(sample_size = n)

  return(list(sim_reps = sim_reps, ecoreg_inclusion_prob = mutate(ecoregion_pts, sample_size = n)))
}
