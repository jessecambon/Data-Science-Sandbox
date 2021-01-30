Multilevel Models
================
Jesse Cambon
January, 2021

References:

  - <https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf>
  - <https://www.rensvandeschoot.com/tutorials/lme4/>

<!-- end list -->

``` r
library(lme4)
library(tidyverse)
```

``` r
fm1 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
```

``` r
#sleepstudy
```

``` r
# Overall Trend
sleepstudy %>%
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() + geom_smooth(method = 'lm')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](../rmd_images/Multilevel-Models/unnamed-chunk-3-1.png)<!-- -->

``` r
#
sleepstudy %>%
  ggplot(aes(x = Days, y = Reaction)) +
  facet_wrap(~Subject) +
  geom_point() + geom_smooth(method = 'lm')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](../rmd_images/Multilevel-Models/unnamed-chunk-3-2.png)<!-- -->

``` r
summary(fm1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Reaction ~ Days + (1 + Days | Subject)
    ##    Data: sleepstudy
    ## 
    ## REML criterion at convergence: 1743.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9536 -0.4634  0.0231  0.4634  5.1793 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev. Corr
    ##  Subject  (Intercept) 612.10   24.741       
    ##           Days         35.07    5.922   0.07
    ##  Residual             654.94   25.592       
    ## Number of obs: 180, groups:  Subject, 18
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  251.405      6.825  36.838
    ## Days          10.467      1.546   6.771
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## Days -0.138
