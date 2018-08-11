Geospatial Analysis
================
Jesse Cambon
10 August, 2018

``` r
library(tidycensus) # census data
library(tmap) # thematic mapping

options(tigris_use_cache = TRUE)

ny <- get_acs(geography = "tract", 
              variables = "B19013_001", 
              state = "NY", 
              county = "New York", 
              geometry = TRUE)
```

    ## Please note: `get_acs()` now defaults to a year or endyear of 2016.

``` r
qtm(ny, fill = "estimate")
```

    ## Linking to GEOS 3.5.1, GDAL 2.2.3, proj.4 4.9.3

    ## Some legend labels were too wide. These labels have been resized to 0.54, 0.51, 0.51, 0.51, 0.51. Increase legend.width (argument of tm_layout) to make the legend wider and therefore the labels larger.

![](Geospatial_Analysis_files/figure-markdown_github/unnamed-chunk-1-1.png)
