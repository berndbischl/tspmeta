\name{numvec_feature_statistics}
\alias{numvec_feature_statistics}
\title{Computes statistics from a vector of of values.}
\usage{
numvec_feature_statistics(x, name, na.rm = TRUE)
}
\arguments{
  \item{x}{[\code{numeric}]\cr Numeric vector.}

  \item{name}{[\code{numeric}]\cr Prefix name for elements
  in result list.}

  \item{na.rm}{[\code{logical(1)}]\cr Should NAs in
  \code{x} be removed?  Default is \code{TRUE}.}
}
\value{
[\code{list}] Elements are named <name_statistic>.
}
\description{
E.g. computes features from distribution of distance.
Computed statistics: min, median, mean, max, sd, span,
coeff_of_var.
}

