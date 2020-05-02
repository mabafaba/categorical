[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mabafaba/categorical?branch=master&svg=true)](https://ci.appveyor.com/project/mabafaba/categorical)
[![CRAN status](https://www.r-pkg.org/badges/version/categorical)](https://cran.r-project.org/package=categorical)

# categorical <img src="man/figures/logo.png" align="right" height=140/>

The goal of categorical is to provide S3 vector types for categorical survey data.

## Installation

This is under development, but you can try:

```
devtools::install_github('mabafaba/categorical')
```
Once on CRAN, you can install the released version of categorical from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("categorical")
```


Categorical Vectors
-------------------

### features

Categorical Vectors are a basic vector type with the following features:

-   they have a fixed set of allowed values called 'levels' (similar to
    levels in factors)
-   they can store alternative values for each level (for example labels, ranks, ...)
-   each record in the vector can have more than one level 'selected' to
    allow storing multipe response data.
-   they are desigend for easy creation of subclasses ("internal" alternative values and a `class` argument)

