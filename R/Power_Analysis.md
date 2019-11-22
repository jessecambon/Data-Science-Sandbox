Power Analysis
================
Jesse Cambon
22 November, 2019

Check with <https://www.stat.ubc.ca/~rollin/stats/ssize/b2.html>

``` r
library(pwr)
pwr.2p.test(
       h=ES.h(0.6,0.8),
       n=NULL,
       sig.level=0.05,
       power=0.80,
       alternative="two.sided")
```

    ## 
    ##      Difference of proportion power calculation for binomial distribution (arcsine transformation) 
    ## 
    ##               h = 0.4421432
    ##               n = 80.29912
    ##       sig.level = 0.05
    ##           power = 0.8
    ##     alternative = two.sided
    ## 
    ## NOTE: same sample sizes

``` r
library(pwr)
power.prop.test(n = NULL, 
                p1 = .6, 
                p2 = .8,
                power=0.8,
                sig.level=0.05,
                alternative="two.sided"
                ) 
```

    ## 
    ##      Two-sample comparison of proportions power calculation 
    ## 
    ##               n = 81.22424
    ##              p1 = 0.6
    ##              p2 = 0.8
    ##       sig.level = 0.05
    ##           power = 0.8
    ##     alternative = two.sided
    ## 
    ## NOTE: n is number in *each* group

### Additional References

  - [Biostat Handbook – Power
    Analysis](http://www.biostathandbook.com/power.html)
  - [Biostat Handbook - Hypothesis
    Testing](http://www.biostathandbook.com/hypothesistesting.html)
  - [UCLA Intro to Power
    Analysis](https://stats.idre.ucla.edu/other/mult-pkg/seminars/intro-power/)
  - [An online power calculator for
    proportions](https://www.stat.ubc.ca/~rollin/stats/ssize/b2.html)
  - [The pwr R
    package](https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html)
    – a popular R package used for power analysis
