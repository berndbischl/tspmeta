context("tsp_generation_ea")

test_that("tsp_generation_ea", {
  res = tsp_generation_ea(
    fitness_function = function(x) sum(x),
    pop_size = 20,  generations = 10, 
    normal_mutation_rate = 0.1, normal_mutation_sd = 3, uniform_mutation_rate = 0.1
  )
})

