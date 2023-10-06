
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

Hi, I'm Bin han. This is an R package I wrote based on my own data analysis needs. This package focus on data statistic and machine learning, especially for medical or laboratory records. I will update more useful functions to improve it.

## Installation

You can install the development version of zmy like so:

``` r
# if(!require(devtools))install.packages("devtools")
# if(!require(package_zmy))devtools::install_github("binhanguo/package_zmy",upgrade = FALSE,dependencies = TRUE)
```

## Functions

1.Data preprocessing
na_process(), outlier_process(),num_plot(),vec_plot()
na_process() and outlier_process() perform handle missing and outlier values for the original data. num_plot() perform box plot for numeric variables, while vec_plot() perform bar plot for categorical variables.

2.Machine learning
the module provides random forest and glmnet as method on model construction by machine learning, for category and numerical dependent variables, respectively.
rf_analysis() perform machine Learning Based on Random Forest for Selecting Variables and evaluate model.
glmnet_analysis() perform machine Learning Based on regressive analysis for evaluate model.

3.Machine learning union
the module provides three methods (gradient Boosting, random forest, and multilayer perception neural network) for machine learning, for category and numerical dependent variables, respectively. Then the metric results of different models will been output.
ML_analysis() perform machine Learning using regression/classification models based on the type of dependent variable.

