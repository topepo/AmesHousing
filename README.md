# AmesHousing


[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/AmesHousing)](http://cran.r-project.org/web/packages/AmesHousing)
[![Downloads](http://cranlogs.r-pkg.org/badges/AmesHousing)](http://cran.rstudio.com/package=AmesHousing)

This package contains the data described by [De Cock (2011)](http://ww2.amstat.org/publications/jse/v19n3/decock.pdf) where 82 fields were recored for 2,930 properties in Ames IA. Different versions of the data are fallible using the package. 

## Installation

You can install `AmesHousing` from github with:

``` r
# install.packages("devtools")
devtools::install_github("topepo/AmesHousing")
```

## Details

* Using `data(ames_raw)`, a tibble is attached with the data as it is found on the website. 
* Using the command, `make_ames()` a tibble is returned that has a processed version of the data (see notes below). 
* The command `make_ordered_ames()` returns a similar tibble but several of the variables are formatted as ordered factors. 

## The Processed Version

The exact details can be found in the code of `make_ames` but a summary is:

 * All factors are _unordered_.
 * `PID` and `Order` are removed. 
 * Spaces and special characters in column names where changed to snake case. To be consistent, `SalePrice` was changed to `Sale_Price`. 
 * One row was removed with an unexplained missing value. 
 * Many factor levels were changed to be more understandable (e.g. `Split_or_Multilevel` instead of `080`)
 * Many missing values were reset. For example, if the variable `Bsmt_Qual` was missing, this implies that there is no basement on the property. Instead of a missing value, the value of `Bsmt_Qual` was changed to `No_Basement`. Similarly, numeric data pertaining to basements were set to zero where appropriate such as variables `Bsmt_Full_Bath` and `Total_Bsmt_SF`.
* `Garage_Yr_Blt` contained many missing data and was removed. 
* Approximate longitude and latitude are included for 2,911 properties. By default, the processed data returns these instances since their parcel IDs can be found in the Iowa system. This eliminated one neighborhood, Green Hills, from the processed data. Also, note that there are 7 properties with identical geotags. These are units within the same building. 

## The Ordered Factor Version

`make_ordinal_ames` is the same as `make_ames` but many ordinal data were changed to class `ordered`. 
