# Load dependencies
library(seminr)
library(rpart)
library(rpart.plot)
library(rattle)

source("lib/fit_tree_library.R", chdir = TRUE)

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

# Estimate model and run deviance trees
utaut_data <- read.csv(file = "correct_utaut_data.csv")[,-66]

utaut_model <- estimate_pls(data = utaut_data,
                            measurement_model = utaut_mm,
                            structural_model = utaut_sm)


deviance_tree <- fit_rpart_tree_seminr(pls_model = utaut_model,
                                       focal_construct = "BI",
                                       cp = 0.01)

# Tests

## Fixtures:
# saveRDS(deviance_tree$tree$where, file = "tests/fixtures/utaut-devtree-where.rds")
# saveRDS(deviance_tree$PD, file = "tests/fixtures/utaut-pd.rds")

tol <- 1e-10

correct_PD <- readRDS("tests/fixtures/utaut-pd.rds")
stopifnot(abs(correct_PD[1] - deviance_tree$PD[1]) < tol)

correct_where <- readRDS("tests/fixtures/utaut-devtree-where.rds")
stopifnot(all(correct_where == deviance_tree$tree$where))

cat("\nALL TESTS PASS!\n")
