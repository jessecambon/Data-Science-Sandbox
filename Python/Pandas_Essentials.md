Pandas Essentials
================
Jesse Cambon
11 April, 2020

<https://github.com/rstudio/reticulate/#python-in-r-markdown>

``` r
library(reticulate)
library(knitr)
```

``` python
import pandas as pd
mtcars = r.mtcars
```

Counting

``` python
am_vs = mtcars.groupby(['am','vs']).size().reset_index(name='count').\
  sort_values('count',ascending=False)
  
am_vs
```

    ##     am   vs  count
    ## 0  0.0  0.0     12
    ## 1  0.0  1.0      7
    ## 3  1.0  1.0      7
    ## 2  1.0  0.0      6
