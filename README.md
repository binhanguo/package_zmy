
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zmy

<!-- badges: start -->
<!-- badges: end -->

The goal of zmy is to …

## Installation

You can install the development version of zmy like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(zmy)
#> Loading required package: caret
#> Loading required package: ggplot2
#> Warning: package 'ggplot2' was built under R version 4.2.3
#> Loading required package: lattice
#> Loading required package: glmnet
#> Warning: package 'glmnet' was built under R version 4.2.3
#> Loading required package: Matrix
#> Loaded glmnet 4.1-7
#> Loading required package: MLmetrics
#> 
#> Attaching package: 'MLmetrics'
#> The following objects are masked from 'package:caret':
#> 
#>     MAE, RMSE
#> The following object is masked from 'package:base':
#> 
#>     Recall
#> Loading required package: plyr
#> Loading required package: pROC
#> Type 'citation("pROC")' for a citation.
#> 
#> Attaching package: 'pROC'
#> The following objects are masked from 'package:stats':
#> 
#>     cov, smooth, var
#> Loading required package: randomForest
#> randomForest 4.7-1.1
#> Type rfNews() to see new features/changes/bug fixes.
#> 
#> Attaching package: 'randomForest'
#> The following object is masked from 'package:ggplot2':
#> 
#>     margin
#> Loading required package: tidyverse
#> Warning: package 'tibble' was built under R version 4.2.3
#> Warning: package 'dplyr' was built under R version 4.2.3
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.2     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.0
#> ✔ lubridate 1.9.2     ✔ tibble    3.2.1
#> ✔ purrr     1.0.1     ✔ tidyr     1.3.0
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::arrange()       masks plyr::arrange()
#> ✖ dplyr::combine()       masks randomForest::combine()
#> ✖ purrr::compact()       masks plyr::compact()
#> ✖ dplyr::count()         masks plyr::count()
#> ✖ dplyr::desc()          masks plyr::desc()
#> ✖ tidyr::expand()        masks Matrix::expand()
#> ✖ dplyr::failwith()      masks plyr::failwith()
#> ✖ dplyr::filter()        masks stats::filter()
#> ✖ dplyr::id()            masks plyr::id()
#> ✖ dplyr::lag()           masks stats::lag()
#> ✖ purrr::lift()          masks caret::lift()
#> ✖ randomForest::margin() masks ggplot2::margin()
#> ✖ dplyr::mutate()        masks plyr::mutate()
#> ✖ tidyr::pack()          masks Matrix::pack()
#> ✖ dplyr::rename()        masks plyr::rename()
#> ✖ dplyr::summarise()     masks plyr::summarise()
#> ✖ dplyr::summarize()     masks plyr::summarize()
#> ✖ tidyr::unpack()        masks Matrix::unpack()
#> ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
"# R_package_zmy" 
" R_package_zmy" 
" R_package_zmy" 
# R_package_zmy
