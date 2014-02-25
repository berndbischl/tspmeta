\name{random_instance}
\alias{random_instance}
\title{Generates a random TSP instance by scattering random points in a hypercube.}
\usage{
random_instance(size, d = 2, lower = 0, upper = 1)
}
\arguments{
  \item{size}{[\code{integer(1)}]\cr Number of cities.}

  \item{d}{[\code{integer(1)}]\cr Space dimensionality,
  e.g. 2D.  Default is 2D.}

  \item{lower}{[\code{numeric(1)}]\cr Lower box constraint
  for hypercube.  Default is 0.}

  \item{upper}{[\code{numeric(1)}]\cr upper box constraint
  for hypercube.  Default is 1.}
}
\value{
[\code{\link{tsp_instance}}].
}
\description{
Generates a random TSP instance by scattering random points
in a hypercube.
}

