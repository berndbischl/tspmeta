\name{fast_two_opt}
\alias{fast_two_opt}
\title{Runs 2-Opt local search on TSP instance.}
\usage{
fast_two_opt(x, initial_tour)
}
\arguments{
  \item{x}{[\code{\link{tsp_instance}}]\cr TSP instance.}

  \item{initial_tour}{[\code{numeric}]\cr Initial tour.}
}
\value{
[\code{\link[TSP]{TOUR}}] TOUR object from package TSP,
containing order of cities, tour length and method name
that generated this solution.
}
\description{
Runs 2-Opt local search on TSP instance.
}

