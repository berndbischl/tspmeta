context("run_solver")

if (interactive()) {
test_that("run_solver", {
  x = random_instance(size = 100)
  for (s in get_solvers()) {
    z = run_solver(x, method = s)
  }
})
}