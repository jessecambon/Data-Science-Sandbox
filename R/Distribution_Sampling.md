Bayesian Modeling
================
Jesse Cambon
25 April, 2020

References: \* <http://appliedpredictivemodeling.com/data> \*
<http://faculty.marshall.usc.edu/gareth-james/ISL/data.html>

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.0     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ forcats 0.5.0
    ## ✓ readr   1.3.1

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
unif_sample <- runif(4000,-5,5)

norm_sample <- rnorm(5000,0,1)

binom_sample <- rbinom(10000,10,.5)
```

``` r
ggplot(data=binom_sample %>% as_tibble()) + 
  geom_density(aes(x=value)) + theme_minimal()
```

![](../rmd_images/Distribution_Sampling/unnamed-chunk-3-1.png)<!-- -->
