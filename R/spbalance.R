#' Spatial Balance
#'
#' Calculates the spatial balance of design sites using Voronoi polygons. Adapted from Benedetti 2015.
#'
#' @param dist_mat Distance matrix for sites in the sample frame.
#' @param pik Probability of inclusion for all sites in sample frame.
#' @param samp_index vector of id's defining the sample.
#'
#' @return Returns the value of the spatial balance metric.
#'
spbalance <- function(dist_mat, pik, samp_index) {

  if(dim(dist_mat)[1] != dim(dist_mat)[2]){
    stop("The distance matrix has to be N x N.")
  }
  if(length(pik) != dim(dist_mat)[1]){
    warning("The vector pi has length different from population size.")
  }

  N = length(pik)
  n = length(samp_index)
  index = 1:N
  incl = rep(0,n)
  for(i in 1:N) {
    near <- which(dist_mat[i,samp_index] == min(dist_mat[i,samp_index]))
    incl[near] = incl[near] + pik[i]
  }
  return(incl)
}
