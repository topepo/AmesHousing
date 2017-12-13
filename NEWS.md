# AmesHousing 0.0.1

* Added a `NEWS.md` file to track changes to the package.

# AmesHousing 0.0.2

First CRAN release. 

# AmesHousing 0.0.3

* Changes made to about two dozen data points. Dmytro Perepolkin made a heroic effort to find updated PID fields and was able to resolve the missing geocodes (see [issue #2](https://github.com/topepo/AmesHousing/issues/2)). 
* A data set (`ames_schools`) was added. 
* The missing electrical value was converted from `NA` to `"Unknown"` so that `make_ames()` now returns all of the properties. 


