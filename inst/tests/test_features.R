context("features")

test_that("features", {
	seeds = 1:3
	sizes = c(5, 10, 100)
	for (size in sizes) {
	  for (s in seeds) {
		  x = random_instance(d = 2, size = size)
		  features(x)
	  }
	}
})


