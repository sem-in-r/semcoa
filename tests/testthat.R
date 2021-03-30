library(testthat)

# Only run tests from a directory
# see: # https://stackoverflow.com/questions/45587660/run-unit-tests-with-testthat-without-package
source("R/coa.R", chdir = TRUE)
testthat::test_dir("tests/testthat")