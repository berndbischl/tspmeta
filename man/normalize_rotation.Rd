\name{normalize_rotation}
\alias{normalize_rotation}
\title{Normalize an instance w.r.t. its rotation.}
\usage{
normalize_rotation(instance)
}
\arguments{
  \item{instance}{[\code{tsp_instance}]\cr}
}
\value{
A rotated \code{tsp_instance}.
}
\description{
Normalization is performed by aligning the main axis of the
cities with the X axis.
}
\seealso{
\code{\link{normalization_angle}}
}

