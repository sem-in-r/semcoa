
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

Until this project becomes a full-fledged R package, run all tests
using:

``` r
source("tests/testthat.R")
```

Or run specific tests:

``` r
test_file("tests/testthat/test-integration-coa.R")
test_file("tests/testthat/test-unit-dtree.R")
```

## Demo

Look at [demos](demos/) folder for more examples

First, setup your composite construct-based model using the seminr
package for specifying structural equation models.

    #> Generating the seminr model
    #> All 216 observations are valid.

Now we can conduct our overfit analysis:

``` r
library(semcoa)
library(rpart)
library(maptools)

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

And you can inspect more details about the groups by inspecting their
node IDs and seeing the defining descriptive characteristics of groups.

``` r
# Get all the names of the groups
names(utaut_overfit$dtree$deviant_groups)
#> [1] "16" "72" "11" "55" "7"

# Get descriptive characteristics of one of the groups
group_rules("72", utaut_overfit$dtree)
#> BI < -0.53
#> -2.45 <= FC < -0.93
#> HAB < 1.00
#> -1.06 <= PE
```

And finally, you can examine unstable paths:

``` r
# TODO
```
