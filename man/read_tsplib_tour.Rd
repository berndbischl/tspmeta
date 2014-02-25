\name{read_tsplib_tour}
\alias{read_tsplib_tour}
\title{Read in a TSPLIB style Traveling Salesman Problem tour from a file}
\usage{
read_tsplib_tour(path)
}
\arguments{
  \item{path}{[\code{character(1)}]\cr Filename of file
  containing a TSP tour.}
}
\value{
[\code{\link[TSP]{TOUR}}] TOUR object from package TSP,
containing order of cities, tour length and method name
that generated this solution.
}
\description{
Read in a TSPLIB style Traveling Salesman Problem tour from
a file
}

