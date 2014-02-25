\name{tsp_generation_ea}
\alias{tsp_generation_ea}
\title{TSP generating EA.}
\usage{
tsp_generation_ea(fitness_function, pop_size = 30L, inst_size = 50L,
  generations = 100L, time_limit = 30L, uniform_mutation_rate,
  normal_mutation_rate, normal_mutation_sd, cells_round = 100L, rnd = TRUE,
  ...)
}
\arguments{
  \item{fitness_function}{[\code{function(x, ...)}]\cr
  Fitness function used to judge the fitness of a TSP
  instance.  \code{x} is a numeric matrix with 2 columns,
  containing the coordinates of a TSP instance.}

  \item{pop_size}{[\code{integer(1)}]\cr Number of TSP
  instances maintained in each population.  Default is 30.}

  \item{inst_size}{[\code{integer(1)}]\cr Number of cities
  of each TSP instance.  Default is 50.}

  \item{generations}{[\code{integer(1)}]\cr Number of
  generations.  Default is 100L.}

  \item{time_limit}{[\code{integer(1)}]\cr Time limit in
  seconds.  Default is 30.}

  \item{uniform_mutation_rate}{[\code{numeric(1)}]\cr
  Mutation probability in uniform mutation (in [0,1]).}

  \item{normal_mutation_rate}{[\code{numeric(1)}]\cr
  Mutation probability in normal mutation (in [0,1])}

  \item{normal_mutation_sd}{[\code{numeric(1)}]\cr Standard
  deviation of normal noise in normal mutation}

  \item{cells_round}{[\code{numeric(1)}]\cr Grid resolution
  for rounding Default is 100.}

  \item{rnd}{[\code{logical(1)}]\cr Round the coordinates
  before normal mutation.  Default is \code{TRUE}.}

  \item{...}{[any]\cr Not used.}
}
\value{
[\code{list}] List containing best individual form the last
population, its fitness value, the genrational fitness and
the last population.  Default is 50.
}
\description{
TSP generating EA.
}

