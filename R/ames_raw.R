#' Raw Ames Housing Data
#'
#' Summon the data described by De Cock (2011) where 82 fields were recored for 2,930 properties in Ames IA.
#'
#' @name ames_raw
#' @aliases ames_raw
#' @docType data
#' @return \item{ames_raw}{a tibble}
#' @details
#'
#' From the data documentation reference, the columns include:
#'
#' * `Order`: Observation number
#' * `PID`: Parcel identification number  - can be used with city web site for parcel review.
#' * `MS SubClass`: Identifies the type of dwelling involved in the sale.
#' * `MS Zoning`: Identifies the general zoning classification of the sale.
#' * `Lot Frontage`: Linear feet of street connected to property
#' * `Lot Area`: Lot size in square feet
#' * `Street`: Type of road access to property
#' * `Alley`: Type of alley access to property
#' * `Lot Shape`: General shape of property
#' * `Land Contour`: Flatness of the property
#' * `Utilities`: Type of utilities available
#' * `Lot Config`: Lot configuration
#' * `Land Slope`: Slope of property
#' * `Neighborhood`: Physical locations within Ames city limits (map available)
#' * `Condition 1`: Proximity to various conditions
#' * `Condition 2`: Proximity to various conditions (if more than one is present)
#' * `Bldg Type`: Type of dwelling
#' * `House Style`: Style of dwelling
#' * `Overall Qual`: Rates the overall material and finish of the house
#' * `Overall Cond`: Rates the overall condition of the house
#' * `Year Built`: Original construction date
#' * `Year Remod/Add`: Remodel date (same as construction date if no remodeling or additions)
#' * `Roof Style`: Type of roof
#' * `Roof Matl`: Roof material
#' * `Exterior 1`: Exterior covering on house
#' * `Exterior 2`: Exterior covering on house (if more than one material)
#' * `Mas Vnr Type`: Masonry veneer type
#' * `Mas Vnr Area`: Masonry veneer area in square feet
#' * `Exter Qual`: Evaluates the quality of the material on the exterior
#' * `Exter Cond`: Evaluates the present condition of the material on the exterior
#' * `Foundation`: Type of foundation
#' * `Bsmt Qual`: Evaluates the height of the basement
#' * `Bsmt Cond`: Evaluates the general condition of the basement
#' * `Bsmt Exposure`: Refers to walkout or garden level walls
#' * `BsmtFin Type 1`: Rating of basement finished area
#' * `BsmtFin SF 1`: Type 1 finished square feet
#' * `BsmtFinType 2`: Rating of basement finished area (if multiple types)
#' * `BsmtFin SF 2`: Type 2 finished square feet
#' * `Bsmt Unf SF`: Unfinished square feet of basement area
#' * `Total Bsmt SF`: Total square feet of basement area
#' * `Heating`: Type of heating
#' * `HeatingQC`: Heating quality and condition
#' * `Central Air`: Central air conditioning
#' * `Electrical`: Electrical system
#' * `1st Flr SF`: First Floor square feet
#' * `2nd Flr SF`: Second floor square feet
#' * `Low Qual Fin SF`: Low quality finished square feet (all floors)
#' * `Gr Liv Area`: Above grade (ground) living area square feet
#' * `Bsmt Full Bath`: Basement full bathrooms
#' * `Bsmt Half Bath`: Basement half bathrooms
#' * `Full Bath`: Full bathrooms above grade
#' * `Half Bath`: Half baths above grade
#' * `Bedroom`: Bedrooms above grade (does NOT include basement bedrooms)
#' * `Kitchen`: Kitchens above grade
#' * `KitchenQual`: Kitchen quality
#' * `TotRmsAbvGrd`: Total rooms above grade (does not include bathrooms)
#' * `Functional`: Home functionality (Assume typical unless deductions are warranted)
#' * `Fireplaces`: Number of fireplaces
#' * `FireplaceQu`: Fireplace quality
#' * `Garage Type`: Garage location
#' * `Garage Yr Blt`: Year garage was built
#' * `Garage Finish`: Interior finish of the garage
#' * `Garage Cars`: Size of garage in car capacity
#' * `Garage Area`: Size of garage in square feet
#' * `Garage Qual`: Garage quality
#' * `Garage Cond`: Garage condition
#' * `Paved Drive`: Paved driveway
#' * `Wood Deck SF`: Wood deck area in square feet
#' * `Open Porch SF`: Open porch area in square feet
#' * `Enclosed Porch`: Enclosed porch area in square feet
#' * `3-Ssn Porch`: Three season porch area in square feet
#' * `Screen Porch`: Screen porch area in square feet
#' * `Pool Area`: Pool area in square feet
#' * `Pool QC`: Pool quality
#' * `Fence`: Fence quality
#' * `Misc Feature`: Miscellaneous feature not covered in other categories
#' * `Misc Val`: $Value of miscellaneous feature
#' * `Mo Sold`: Month Sold
#' * `Yr Sold`: Year Sold
#' * `Sale Type`: Type of sale
#' * `Sale Condition`: Condition of sale
#'
#' @source De Cock, D. (2011). "Ames, Iowa: Alternative to the Boston Housing Data as an End of Semester Regression Project," \emph{Journal of Statistics Education},  Volume 19, Number 3.
#'
#' \url{https://ww2.amstat.org/publications/jse/v19n3/decock/DataDocumentation.txt}
#'
#' \url{http://ww2.amstat.org/publications/jse/v19n3/decock.pdf}
#'
#' @keywords datasets
NULL


#' @rdname ames_raw
#' @name ames_geo
#' @aliases ames_geo
NULL

#' @rdname ames_raw
#' @name ames_new
#' @aliases ames_new
NULL


#' @rdname ames_raw
#' @name hood_levels
#' @aliases hood_levels
NULL

#' Ames Public Schools
#'
#' Locations of local schools and outline of elementary school districts.
#' Elementary school district boundaries are returned as `sf` object in CRS 4326
#'
#' @name ames_schools
#' @aliases ames_schools_geo
#' @docType data
#' @return \item{ames_schools_geo}{a tibble}
#' @details
#'
#' The data set includes the school name and the geocodes.
#' @keywords datasets
NULL


#' @rdname ames_schools
#' @name ames_school_districts_sf
#' @aliases ames_school_districts_sf

NULL
