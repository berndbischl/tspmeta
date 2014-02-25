\name{greedy_point_matching}
\alias{greedy_point_matching}
\title{Greedy point matching}
\usage{
greedy_point_matching(x, y)
}
\arguments{
  \item{x}{[\code{tsp_instance}]\cr First TSP instance}

  \item{y}{[\code{tsp_instance}]\cr Second TSP instance}
}
\value{
[\code{matrix}] Numeric matrix of point indices with
shortest distance.
}
\description{
Pairs of cities are matched in a greedy fashion for
morphing, first the closest pair w.r.t. euclidean distance,
then the clostest pair of the remaining cities, and so on.
}

