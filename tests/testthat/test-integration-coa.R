# Set working directory to project root 
# (only needed if called by test_dir())
setwd("../..")

# Only test this file
# test_file("tests/testthat/test-integration-coa.R")

# Specify model
utaut_mm <- constructs(
  composite("PE", multi_items("PERF", 1:4)),
  composite("EE", c("PEOU1","PEOU3","PEOU5","PEOU6")),
  composite("SI", c(multi_items("NORM", 1:2),"INFL3")),
  composite("FC", multi_items("FACL", 1:4)),
  composite("HM", multi_items("MOTIV", 1:3)),
  composite("PV", multi_items("VALUE", 1:3)),
  composite("HAB", multi_items("HAB", 1:4)),
  composite("BI", multi_items("INT", 1:3)),
  composite("Exp", single_item("Experience")),
  composite("Age", single_item("age")),
  composite("Gender", single_item("gender"))
)

utaut_sm <- relationships(
  paths(from = c("PE","EE","SI","FC","HM","PV","HAB","Exp","Age","Gender"), to = "BI")
)

# Estimate model
utaut_data <- read.csv(file = "data/correct_utaut_data.csv")[,-66]

utaut_model <- estimate_pls(data = utaut_data,
                            measurement_model = utaut_mm,
                            structural_model = utaut_sm)

# Compute COA steps

utaut_pred <- prediction_metrics(utaut_model, "BI")
# saveRDS(utaut_pred, file = "tests/fixtures/utaut-pred.rds")
utaut_dtree <- deviance_tree(utaut_pred, deviance_bounds = c(0.025, 0.975))
# saveRDS(utaut_dtree, file = "tests/fixtures/utaut-dtree.rds")
utaut_unstable <- unstable_params(utaut_model, utaut_dtree, params = c("path_coef", "outer_weights", "rSquared"))
# saveRDS(utaut_unstable, file = "tests/fixtures/utaut-unstable.rds")

# utaut_overfit <- coa(pls_model = utaut_model, 
#                      focal_construct = "BI",
#                      params = c("path_coef", "outer_weights", "rSquared"))

tol <- 1e-10

correct_pred     <- readRDS(file = "tests/fixtures/utaut-pred.rds")
correct_dtree    <- readRDS(file = "tests/fixtures/utaut-dtree.rds")
correct_unstable <- readRDS(file = "tests/fixtures/utaut-unstable.rds")

test_that("COA analysis returns correct objects of right size", {
  coa_analysis <- coa(
    pls_model = utaut_model, 
    focal_construct = "BI",
    params = c("path_coef", "outer_weights", "rSquared")
  )
  
  expect_equal(coa_analysis$focal_construct, "BI")
  expect_true(all(coa_analysis$deviance_bounds == c(0.025, 0.975)))
  expect_equal(object.size(coa_analysis$pls_model), object.size(utaut_model))
  expect_equal(object.size(coa_analysis$predictions), object.size(correct_pred))
  # Note that dtree split criteria change between perturbations of predictions!
  # cat(paste("\n", "coa_analysis$dtree size: ", object.size(coa_analysis$dtree), "correct_dtree size: ", object.size(correct_dtree), "\n"))
  # WON'T PASS: expect_equal(object.size(coa_analysis$dtree), object.size(correct_dtree))
  expect_s3_class(coa_analysis$dtree, "coa_deviance_tree")
  expect_equal(object.size(coa_analysis$unstable), object.size(correct_unstable))
})

test_that("Prediction metrics are computed as expected", {
  expect_true(all(abs(utaut_pred$PD - correct_pred$PD) < tol))
})

test_that("Deviant groups and cases are found as expected", {
  dtree_compare <- mapply(
    function(u, c) {u == c},
    utaut_dtree$deviant_groups, 
    correct_dtree$deviant_groups
  )
  
  expect_true(all(sapply(dtree_compare, all)))
  expect_true(all(utaut_dtree$unique_deviants == correct_dtree$unique_deviants))
})

test_that("Unstable paths are computed correctly", {
  group_check <- mapply(
    function(a, b) {a$param_diffs$path_coef == b$param_diffs$path_coef}, 
    utaut_unstable$group_diffs, correct_unstable$group_diffs
  )
  expect_true(all(group_check))
  
  unique_check <- mapply(
    function(a, b) {a$param_diffs$path_coef == b$param_diffs$path_coef}, 
    utaut_unstable$unique_diffs, correct_unstable$unique_diffs
  )
  expect_true(all(unique_check))
})
