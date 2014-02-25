\name{feature_cluster}
\alias{feature_cluster}
\title{Cluster features.}
\usage{
feature_cluster(x, epsilon)
}
\arguments{
  \item{x}{[\code{\link{tsp_instance}}]\cr TSP instance.}

  \item{epsilon}{[\code{numeric(1)}]\cr Probability in
  [0,1]. Used to compute the reachability distance for the
  underlying \code{\link[fpc]{dbscan}} clustering
  algorithm.}
}
\value{
[\code{list}].
}
\description{
Determines the number of clusters and the mean distances
from all cities in a cluster to its centroid.
}

