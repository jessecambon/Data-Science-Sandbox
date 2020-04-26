Distribution Sampling
================
Jesse Cambon
26 April, 2020

References: \* <http://appliedpredictivemodeling.com/data> \*
<http://faculty.marshall.usc.edu/gareth-james/ISL/data.html>

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.0     ✓ dplyr   0.8.5
    ## ✓ tidyr   1.0.2     ✓ forcats 0.5.0
    ## ✓ readr   1.3.1

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
unif_sample <- runif(4000,-5,5)
norm_sample <- rnorm(5000,0,1)
binom_sample <- rbinom(10000,10,.5)
bernouli_sample <- rbernoulli(10,p=0.9)
poison_sample <- rpois(1000,10)
negbinomial_sample <- rnbinom(10000,10,mu=10)
```

``` r
ggplot(data=poison_sample %>% as_tibble()) + 
  geom_density(aes(x=value)) + theme_minimal() +
  geom_density(data=rnorm(2000,10,5) %>% as_tibble(),aes(x=value),color='navy') +
  geom_density(data=negbinomial_sample %>% as_tibble(),aes(x=value),color='red')
```

![](../rmd_images/Distribution_Sampling/unnamed-chunk-3-1.png)<!-- -->

``` r
t.test(trees$Height)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  trees$Height
    ## t = 66.41, df = 30, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  73.6628 78.3372
    ## sample estimates:
    ## mean of x 
    ##        76

``` r
t.test(trees$Girth)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  trees$Girth
    ## t = 23.506, df = 30, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  12.09731 14.39947
    ## sample estimates:
    ## mean of x 
    ##  13.24839

``` r
compare_norms <- rnorm(100,25,10) %>%
  as_tibble() %>% rename(sample1=value) %>%
  mutate(sample2 = rnorm(100,28,10))

library(broom)
results <- t.test(compare_norms$sample1,compare_norms$sample2)
results
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  compare_norms$sample1 and compare_norms$sample2
    ## t = -2.3777, df = 196.73, p-value = 0.01838
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -5.8904601 -0.5492324
    ## sample estimates:
    ## mean of x mean of y 
    ##  25.25533  28.47517

``` r
tidy_results <- results %>% tidy()
```
