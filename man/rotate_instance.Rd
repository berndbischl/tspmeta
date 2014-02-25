\name{rotate_instance}
\alias{rotate_instance}
\title{Rotate the cities of a TSP instance around a point.}
\usage{
rotate_instance(instance, angle, center)
}
\arguments{
  \item{instance}{[\code{\link{tsp_instance}}]\cr TSP
  instance.}

  \item{angle}{[\code{numeric}]\cr Angle by which to rotate
  the coordinates. In radians.}

  \item{center}{[\code{numeric}]\cr Point around which to
  rotate the cities. If missing, defaults to the center of
  mass of the cities.}
}
\value{
[\code{\link{tsp_instance}}] New TSP instance.
}
\description{
Rotate the cities of a TSP instance around a point.
}

