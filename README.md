
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MPSurvey

<!-- badges: start -->
<!-- badges: end -->

The goal of MPSurvey is to reduce the burden of analyzing mobile phone
surveys that utilize the [Surveda](https://surveda.instedd.org/) mobile
phone survey tool. The functions in this package cover all aspects of
analysis including applying data labels, identifying skip logic
patterns, calculating item nonresponse, and analyzing weighted survey
data.

## Installation

You can install the development version of MPSurvey from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("csiesel/MPSurvey")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(MPSurvey)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
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
up-to-date. `devtools::build_readme()` is handy for this.

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
