% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spbalance.R
\name{spbalance}
\alias{spbalance}
\title{Spatial Balance}
\usage{
spbalance(dist_mat, pik, samp_index)
}
\arguments{
\item{dist_mat}{Distance matrix for sites in the sample frame.}

\item{pik}{Probability of inclusion for all sites in sample frame.}

\item{samp_index}{Vector of indexes of included sites.}
}
\value{
Returns the value of the spatial balance metric.
}
\description{
Calculates the spatial balance of design sites using Voronoi polygons. Adapted from Benedetti 2015.
}
