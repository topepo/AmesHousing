# AmesHousing (development version)

# AmesHousing 0.0.4

 * `ames_schools` has been renamed to `ames_schools_geo` for consistency with other datasets carrying long-lat data. The school names are amended to indicate full name of the school.
 
 * A new spatial dataset `ames_school_districts_sf` describes Elementary School District boundaries and includes district name (coinciding with the name of the school) and the polygon/multipolygon outline in `sf` format (using CRS 4326).
 
 * `make_ames_new()` creates a data set of new properties. These were populated using less data sources than the original and lack a number of the condition and quality. Both properties were unsold at the time of this writing.
 
 * As a consequence of the new properties, a new neighborhood (`Hayden_Lake`) is now a factor level where the data from `make_ames()` has no data for.  

 
# AmesHousing 0.0.3

* Changes made to about two dozen data points. Dmytro Perepolkin made a heroic effort to find updated PID fields and was able to resolve the missing geocodes (see [issue #2](https://github.com/topepo/AmesHousing/issues/2)). 
* A data set (`ames_schools`) was added. 
* The missing electrical value was converted from `NA` to `"Unknown"` so that `make_ames()` now returns all of the properties. 


# AmesHousing 0.0.2

First CRAN release. 

# AmesHousing 0.0.1

* Added a `NEWS.md` file to track changes to the package.

