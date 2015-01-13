library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  library(fpc)
  library(splancs)
  library(vegan)
  library(TSP)
  load_all(".")
} else {
  library(tspmeta)
}
test_dir("tests/testthat")

