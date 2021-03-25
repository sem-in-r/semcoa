# Composite Overfit Analysis

Package to conduct COA framework, and report and plot results.

## Required Libraries

Please install following packages:

- seminr
- rpart
- rpart.plot
- rattle

## Demos

Look at `coa_demo.R` for more examples

```R
### UTAUT EXAMPLE

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

utaut_overfit <- coa(pls_model = utaut_model, 
                     focal_construct = "BI",
                     params = c("path_coef", "outer_weights", "rSquared"))

plot_pd(utaut_overfit)
```

## Testing
(todo)
