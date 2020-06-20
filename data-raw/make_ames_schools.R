## code to prepare `ames_school_districts` dataset goes here
# Spell schools as they are spelled https://www.ames.k12.ia.us/schools/
#datapasta::tribble_paste(AmesHousing::ames_schools)

ames_schools_geo <-
 tibble::tribble(~School, ~Longitude, ~Latitude,
                "Edwards Elementary School",   -93.6854,  42.01546,
                "Fellows Elementary School", -93.628046, 42.042803,
                "Meeker Elementary School",  -93.61422, 42.041098,
                "Mitchell Elementary School", -93.601053, 41.990308,
                "Sawyer Elementary School", -93.677066, 42.033903,
                "Northwood Preschool Center", -93.618602, 42.050258,
                "Ames Middle School", -93.671331, 42.013433,
                "Ames High School", -93.635555, 42.040092)

usethis::use_data(ames_schools_geo)

# made from https://www.cityofames.org/home/showdocument?id=1025
ames_school_districts_sf <- sf::st_read("data-raw/ames_schoool_districts.gpkg")

usethis::use_data(ames_school_districts_sf)
