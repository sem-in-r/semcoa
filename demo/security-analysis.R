# Load SEMinR library
source("R/coa.R", chdir = TRUE)

### SECURITY EXAMPLE

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

security <- read.csv(file = "data/Security4.csv")

# Estimating the full model
sec_model <- estimate_pls(data = security,
                          measurement_model = security_mm,
                          structural_model = security_sm)

# Running COA framework
sec_overfit <- coa(pls_model = sec_model, focal_construct = "Y_TRUST",
                   params = c("path_coef", "outer_weights", "rSquared"))

plot_pd(sec_overfit)
sec_overfit$dtree$deviant_groups
# $`2`
# [1] 241 279
# 
# $`12`
# [1]  49  52  57  59 138 159 183 226 317 327
# 
# $`26`
# [1] 199 267
# 
# $`29`
# [1]  67  69 137 207 257 270 367
# 
# $`15`
# [1]  17  39  66  85 148 155 193 302 361 382 385