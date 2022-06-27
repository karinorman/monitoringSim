##### FIX ME: I started adapting this to a mapping approach but I don't want to do it anymore... ####
#' Spatial Balance
#'
#' Calculates the spatial balance of design sites using Voronoi polygons. Adapted from Benedetti 2015.
#'
#' @param dist_mat Distance matrix for sites in the sample frame.
#' @param pik Probability of inclusion for all sites in sample frame.
#' @param algorithm_name name of algorithm to calculate metric for
#' @param sample_size sample size to calculate metric for
#' @param rep_id rep to calculate metric for
#' @param index_df dataframe that contains index data
#'
#' @return Returns the value of the spatial balance metric.
#'
spbalance <- function(algorithm_name, sample_size, rep_id, index_df, dist_mat, pik) {

  N = length(pik)
  n = length(samp_index)
  index = 1:N
  incl = rep(0,n)
  for(i in 1:N) {
    near <- which(dist_mat[i,samp_index] == min(dist_mat[i,samp_index]))
    incl[near] = incl[near] + pik[i]
  }
  var(incl)
}
