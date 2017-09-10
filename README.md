# AmesHousing

This package contains the data described by [De Cock (2011)](http://ww2.amstat.org/publications/jse/v19n3/decock.pdf) where 82 fields were recored for 2,930 properties in Ames IA. Different versions of the data are fallible using the package. 

## Installation

You can install AmesHousing from github with:

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
 * Many factor levels were changed to be more understandable (e.g. `split_or_multilevel` instead of `080`)
 * Many missing values were reset. For example, if the variable `Bsmt_Qual` was missing, this implies that there is no basement on the property. Instead of a missing value, the value of `Bsmt_Qual` was changed to `No_Basement`. Similarly, numeric data pertaining to basements were set to zero where appropriate such as variables `Bsmt_Full_Bath` and `Total_Bsmt_SF`.
* `Garage_Yr_Blt` contained many missing data and was removed. 

## The Ordered Factor Version

`make_ordered_ames` is the same as `make_ames` but many ordinal data were changed to class `ordered`. 
