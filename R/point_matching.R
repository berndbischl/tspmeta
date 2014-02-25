#' Greedy point matching
#' 
#' Pairs of cities are matched in a greedy fashion for morphing,
#' first the closest pair w.r.t. euclidean distance, then the clostest
#' pair of the remaining cities, and so on.
#' 
#' @param x [\code{tsp_instance}]\cr
#'   First TSP instance
#' @param y [\code{tsp_instance}]\cr
#'   Second TSP instance
#' @return [\code{matrix}]
#'   Numeric matrix of point indices with shortest distance.
#' @export
greedy_point_matching = function(x, y) {
  checkArg(x, "tsp_instance")
  checkArg(y, "tsp_instance")
  x = x$coords
  y = y$coords
  # all distances to y_i's from each point in x    
  lists_dists = lapply(1:nrow(x), function(i) eucl_distance(x[i,], y))
  len = length(lists_dists)
    
  # initialize index matrix
  ind = matrix(c(1:len, rep(0, len)), byrow = FALSE, ncol = 2, nrow = len)        
  for (i in c(1:len)){            
    ## index of point in x for which the minimum distance to all
    ## points in y is smallest
    m  <- which.min(do.call(rbind,
                            lapply(lists_dists,
                                   function(x) min(x, na.rm = TRUE))))        
                                        # index of point in y with smallest distance 
    ind[m, 2] = which.min(lists_dists[[m]])
    
    ## discard the selected point in y                 
    for (j in c(1:len)[-m]){
      if((is.na(lists_dists[[j]][1]) == TRUE) | (is.finite(lists_dists[[j]][1]))){
        lists_dists[[j]][ind[m,2]] = NA
      }
    }
    
    ## set entry to Infinity to avoid repeated selection
    lists_dists[m] = Inf
  }
  ind
}

# Random point matching.
#
# @param i1 [\code{tsp_instance}]
# @param i2 [\code{tsp_instance}]
# @param ntries [\code{integer(1)}] \cr
#   Number of random matchings to try. The best one is returned.
# @return A matrix with 2 columns that specifies the best of the
#   \code{ntries} matching that was found.
random_point_matching <- function(i1, i2, ntries=100) {
  checkArg(i1, "tsp_instance")
  checkArg(i2, "tsp_instance")
  best_matching <- NULL
  best_matching_error <- Inf
  n <- nrow(i1$coords)
  for (i in 1:ntries) {
    matching <- cbind(1:n, sample(1:n))
    error <- matching_error(matching, i1, i2)
    if (error < best_matching_error) {
      best_matching <- matching
      best_matching_error <- error
    }
  }
  best_matching
}

# Quantify error of instance matching.
#
# Uses the sum of the squared distance between the matched cities as
# a measure for the error of the matching. If a matching is perfect,
# the error would be zero, else greater than zero.
#
# @param matching [\code{matrix}]\cr
#   A matrix with 2 columns that matches cities of \code{i1} to cities in \code{i2}.
# @param i1 [\code{tsp_instance}]
# @param i2 [\code{tsp_instance}]
# @return The error of the matching.
matching_error <- function(matching, i1, i2) {
  D <- i1$coords[matching[, 1],] - i2$coords[matching[,2],]
  sum(D*D)
}
