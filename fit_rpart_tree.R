# Load SEMinR library
library(seminr)
library(rpart)
source("fit_tree_library.R")
library(rpart.plot)
library(rattle)

# Creating measurement model
security_mm <- constructs(
  composite("P_FAML",  single_item("FAML1")),
  composite("P_IPC",   multi_items("PIPC", 2:3)),
  composite("P_INN",   multi_items("PINN", 1:3)),
  composite("S_PPSS",  multi_items("PPSS", 1:3), weights = mode_B),
  composite("S_PINV",  multi_items("PINV", 1:3)),
  composite("S_REP",   multi_items("PREP", 1:3)),
  composite("X_SEC",   c("ASEC_FS","NSEC_FS","PSEC_FS"), weights = mode_B),
  composite("Y_TRUST", multi_items("TRST", 1:4), weights = mode_B),
  composite("c_IEXP",  single_item("IEXP")),
  composite("c_HHINC", single_item("HHINC")),
  composite("c_GEN",   single_item("GEN")),
  composite("c_AGE",   single_item("age"))
)

# Creating structural model
security_sm <- relationships(
  paths(from = c("P_FAML", "P_IPC", "P_INN", "S_PPSS", "S_PINV", "S_REP"),   to = c("X_SEC", "Y_TRUST")),
  paths(from = "X_SEC",  to = "Y_TRUST"),
  paths(from = c("c_IEXP", "c_HHINC", "c_GEN", "c_AGE"),   to = c("X_SEC", "Y_TRUST"))
)

security <- read.csv(file = "Security4.csv")

# Estimating the full model
sec_model <- estimate_pls(data = security,
                          measurement_model = security_mm,
                          structural_model = security_sm)

# Running COA framework
deviance_tree <- fit_rpart_tree_seminr(pls_model=sec_model,
                                       focal_construct = "Y_TRUST")

# Quick test code
# source("fit_tree_library.R"); deviance_tree <- fit_rpart_tree_seminr(pls_model=sec_model, focal_construct = "Y_TRUST", cp = 0.01)

deviants <- (deviance_tree$PD)[(deviance_tree$PD > quantile(deviance_tree$PD, probs = c(0.95))) | (deviance_tree$PD < quantile(deviance_tree$PD, probs = c(0.05)))]
fancyRpartPlot(deviance_tree$tree, caption = NULL)


# Load the project data  ----
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
                                       focal_construct = "BI")

deviants <- (deviance_tree$PD)[(deviance_tree$PD > quantile(deviance_tree$PD, probs = c(0.95))) | (deviance_tree$PD < quantile(deviance_tree$PD, probs = c(0.05)))]
fancyRpartPlot(deviance_tree$tree, caption = NULL)

