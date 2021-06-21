
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SEMCOA - Composite Overfit Analysis for Structural Equation Models

`semcoa` is an R package to conduct the Composite Overfit Analysis (COA)
framework for Structural Equation Models (SEM), and to then report and
plot results.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("sem-in-r/semcoa")
```

## Required Libraries

Please install following packages:

-   seminr (composite model estimation)
-   rpart (decision tree)
-   maptools (plotting labels)

## Testing

Run all tests using ‘Test Package’ of RStudio, or run specific test
files using `devtools`:

``` r
test_active_file("tests/testthat/test-integration-coa.R")
test_active_file("tests/testthat/test-unit-dtree.R")
```

## Demo

Look at [demos](demos/) folder for full examples. The following is a
brief look at the code and output.

First, setup your composite construct-based model using the seminr
package for specifying structural equation models.

    #> Generating the seminr model
    #> All 216 observations are valid.

Now, we can conduct our Composite Overfit Analysis (COA). We start by
loading the `semcoa` library:

``` r
library(semcoa)
library(rpart)
library(maptools)
#> Loading required package: sp
#> Checking rgeos availability: FALSE
#>      Note: when rgeos is not available, polygon geometry     computations in maptools depend on gpclib,
#>      which has a restricted licence. It is disabled by default;
#>      to enable gpclib, type gpclibPermit()
```

Now we can analyze our estimated composite SEM model using COA:

``` r
utaut_overfit <- coa(pls_model = utaut_model, 
                     focal_construct = "BI",
                     params = c("path_coef", "rSquared"))
#> Computing predictive deviance
#> Generating Deviance Tree
#> Identifying Unstable Paths
```

You can visualize your predictive deviant cases and groups as follows:

``` r
plot_pd(utaut_overfit)
```

<img src="man/figures/README/README-utaut-pd-plot-1.png" width="100%" style="display: block; margin: auto auto auto 0;" />

``` r
# Inspect all the names of the groups
names(utaut_overfit$dtree$deviant_groups)
#> [1] "a" "b" "c" "d" "e"
```

And you can inspect more details about the groups by seeing the defining
descriptive characteristics of each group.

``` r
# Get descriptive characteristics of one of the groups
group_rules("b", utaut_overfit$dtree)
#> BI < -0.53
#> -2.45 <= FC < -0.93
#> HAB < 1.00
#> -1.06 <= PE
```

And finally, you can examine unstable paths:

``` r
# TODO
```
