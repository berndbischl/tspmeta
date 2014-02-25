\name{features}
\alias{features}
\title{Calculates list of all TSP features for an instance.}
\usage{
features(x, rescale = TRUE)
}
\arguments{
  \item{x}{[\code{\link{tsp_instance}}]\cr TSP instance}

  \item{rescale}{[\code{logical(1)}]\cr Rescale \code{x} to
  \eqn{[0,1]^2} before calculation of features?  Default is
  \code{TRUE}.}
}
\value{
[\code{list}].
}
\description{
Calculates list of all TSP features for an instance.
}
\examples{
x = random_instance(10)
print(features(x))
}
\seealso{
\code{\link{feature_angle}},
\code{\link{feature_centroid}},
\code{\link{feature_cluster}},
\code{\link{feature_bounding_box}},
\code{\link{feature_chull}},
\code{\link{feature_distance}},
\code{\link{feature_modes}}, \code{\link{feature_mst}},
\code{\link{feature_nnds}}
}

