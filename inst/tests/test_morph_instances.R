context("morph_instances")

test_that("morph_instances", {
  seeds = 1:10
  alphas = c(0, 0.3, 1)
  size = 10
  for (a in alphas) {
    for (s in seeds) {
      set.seed(s)      
      x = random_instance(size = size)
      y = random_instance(size = size)
      morph_instances(x, y, alpha = a)
    }
  }
})
