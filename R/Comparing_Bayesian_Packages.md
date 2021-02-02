Comparing Bayesian Modeling Packages
================
Jesse Cambon
02 February, 2021

-   [Rstan](#rstan)
-   [Brms](#brms)
-   [rstanarm](#rstanarm)

Compare rstan, brms, and rstanarm

``` r
library(rstan)
library(brms)
library(rstanarm)
library(tidyverse)
library(bayesplot)
options(mc.cores = parallel::detectCores())
```

## Rstan

Walking through this example:
<https://cran.r-project.org/web/packages/rstan/vignettes/rstan.html#sample-from-the-posterior-distribution>

``` r
# Sample Dataset
schools_data <- list(
  J = 8,
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)

stan_code <- "
data {
  int<lower=0> J;          // number of schools 
  real y[J];               // estimated treatment effects
  real<lower=0> sigma[J];  // s.e. of effect estimates 
}
parameters {
  real mu; 
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau * eta;
}
model {
  target += normal_lpdf(eta | 0, 1);
  target += normal_lpdf(y | theta, sigma);
}"
```

``` r
fit1 <- stan(
  model_code = stan_code,  # Stan program
  data = schools_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
  )
```

    ## Warning: There were 3 divergent transitions after warmup. See
    ## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
    ## to find out why this is a problem and how to eliminate them.

    ## Warning: Examine the pairs() plot to diagnose sampling problems

## Brms

Example based on : <https://github.com/paul-buerkner/brms>

-   `(1 | var)` is used to specify a random intercept

Mixed effect model has both random effects and fixed effects

-   <https://www.theanalysisfactor.com/understanding-random-effects-in-mixed-models/>
-   <https://ourcodingclub.github.io/tutorials/mixed-models/#what>
-   <https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html>
-   <https://en.wikipedia.org/wiki/Mixed_model>

``` r
fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient), 
            data = epilepsy, family = poisson())
```

    ## Compiling Stan program...

    ## Start sampling

``` r
fit2 <- brm(count ~ zAge + zBase * Trt + (1|patient) + (1|obs), 
            data = epilepsy, family = poisson())
```

    ## Compiling Stan program...
    ## Start sampling

``` r
fit1
```

    ##  Family: poisson 
    ##   Links: mu = log 
    ## Formula: count ~ zAge + zBase * Trt + (1 | patient) 
    ##    Data: epilepsy (Number of observations: 236) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Group-Level Effects: 
    ## ~patient (Number of levels: 59) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.58      0.07     0.46     0.73 1.00      970     1614
    ## 
    ## Population-Level Effects: 
    ##            Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept      1.77      0.12     1.53     2.00 1.00      768     1487
    ## zAge           0.09      0.09    -0.07     0.26 1.01      638     1142
    ## zBase          0.70      0.12     0.45     0.94 1.00      779     1520
    ## Trt1          -0.27      0.17    -0.60     0.06 1.01      667     1489
    ## zBase:Trt1     0.06      0.16    -0.26     0.38 1.00      843     1389
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
plot(fit1, pars = c("Trt", "zBase")) 
```

![](../rmd_images/Comparing_Bayesian_Packages/unnamed-chunk-6-1.png)<!-- -->

``` r
plot(fit2, pars = c("Trt", "zBase"))
```

![](../rmd_images/Comparing_Bayesian_Packages/unnamed-chunk-6-2.png)<!-- -->

Compare model results with leave-one-out validation

<https://mc-stan.org/loo/>

``` r
loo(fit1, fit2)
```

    ## Warning: Found 9 observations with a pareto_k > 0.7 in model 'fit1'. It is
    ## recommended to set 'moment_match = TRUE' in order to perform moment matching for
    ## problematic observations.

    ## Warning: Found 60 observations with a pareto_k > 0.7 in model 'fit2'. It is
    ## recommended to set 'moment_match = TRUE' in order to perform moment matching for
    ## problematic observations.

    ## Output of model 'fit1':
    ## 
    ## Computed from 4000 by 236 log-likelihood matrix
    ## 
    ##          Estimate   SE
    ## elpd_loo   -671.2 36.5
    ## p_loo        94.2 14.4
    ## looic      1342.5 73.1
    ## ------
    ## Monte Carlo SE of elpd_loo is NA.
    ## 
    ## Pareto k diagnostic values:
    ##                          Count Pct.    Min. n_eff
    ## (-Inf, 0.5]   (good)     212   89.8%   703       
    ##  (0.5, 0.7]   (ok)        15    6.4%   153       
    ##    (0.7, 1]   (bad)        7    3.0%   49        
    ##    (1, Inf)   (very bad)   2    0.8%   13        
    ## See help('pareto-k-diagnostic') for details.
    ## 
    ## Output of model 'fit2':
    ## 
    ## Computed from 4000 by 236 log-likelihood matrix
    ## 
    ##          Estimate   SE
    ## elpd_loo   -597.4 14.4
    ## p_loo       109.8  7.6
    ## looic      1194.7 28.8
    ## ------
    ## Monte Carlo SE of elpd_loo is NA.
    ## 
    ## Pareto k diagnostic values:
    ##                          Count Pct.    Min. n_eff
    ## (-Inf, 0.5]   (good)     78    33.1%   856       
    ##  (0.5, 0.7]   (ok)       98    41.5%   223       
    ##    (0.7, 1]   (bad)      53    22.5%   33        
    ##    (1, Inf)   (very bad)  7     3.0%   7         
    ## See help('pareto-k-diagnostic') for details.
    ## 
    ## Model comparisons:
    ##      elpd_diff se_diff
    ## fit2   0.0       0.0  
    ## fit1 -73.9      26.4

## rstanarm

Rstanarm examle compared with brms

-   <https://mc-stan.org/loo/articles/loo2-example.html>
-   <http://mc-stan.org/rstanarm/articles/count.html>

brms prior setting:
<https://www.jamesrrae.com/post/bayesian-logistic-regression-using-brms-part-1/>

``` r
# Use rstanarm to fit a poisson model
roach_pois <-
  stan_glm(
    formula = y ~ roach1 + treatment + senior,
    offset = log(exposure2),
    data = roaches,
    family = poisson(link = "log"),
    prior = normal(0, 2.5, autoscale = TRUE),
    prior_intercept = normal(0, 5, autoscale = TRUE),
    seed = 12345
  )

# # Use rstanarm to fit a negative binomial model
roach_negbinom2 <- update(roach_pois, family = neg_binomial_2)
```

Fit a Brms model for comparison

``` r
# Priors to be used by brm
my_priors <- c(
  prior(normal(0, 5), class = "Intercept"),
  prior(normal(0, 2.5), class = "b")
)

# Fit with zero inflated negative binomial with brm
roach_zinb <-
  brm(
    formula=y ~ roach1 + treatment + senior,
    data = roaches,
    family = zero_inflated_negbinomial,
    seed = 12345
  )
```

    ## Compiling Stan program...

    ## Start sampling

``` r
plot(roach_pois)
```

![](../rmd_images/Comparing_Bayesian_Packages/unnamed-chunk-10-1.png)<!-- -->

``` r
plot(roach_zinb,pars=c('roach1','treatment','senior'))
```

![](../rmd_images/Comparing_Bayesian_Packages/unnamed-chunk-10-2.png)<!-- -->

``` r
pp_check(roach_pois, plotfun='stat')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../rmd_images/Comparing_Bayesian_Packages/unnamed-chunk-11-1.png)<!-- -->

``` r
pp_check(roach_negbinom2, plotfun='stat')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](../rmd_images/Comparing_Bayesian_Packages/unnamed-chunk-11-2.png)<!-- -->

``` r
pp_check(roach_zinb, plotfun='stat')
```

    ## Using 10 posterior samples for ppc type 'dens_overlay' by default.

    ## Warning: The following arguments were unrecognized and ignored: plotfun

![](../rmd_images/Comparing_Bayesian_Packages/unnamed-chunk-11-3.png)<!-- -->

``` r
prop_zero <- function(y) mean(y == 0)

prop_zero_test1 <- pp_check(roach_pois, plotfun = "stat", stat = "prop_zero", binwidth = .005)
prop_zero_test2 <- pp_check(roach_negbinom2, plotfun = "stat", stat = "prop_zero", 
                            binwidth = 0.01)
prop_zero_test3 <- pp_check(roach_zinb, plotfun = "stat", stat = "prop_zero", 
                            binwidth = 0.01)
```

    ## Using 10 posterior samples for ppc type 'dens_overlay' by default.

    ## Warning: The following arguments were unrecognized and ignored: plotfun, stat,
    ## binwidth

``` r
# Show graphs for Poisson and negative binomial side by side
bayesplot_grid(prop_zero_test1 + ggtitle("Poisson"), 
               prop_zero_test2 + ggtitle("Negative Binomial"), 
               prop_zero_test3 + ggtitle("Zero Inflated Negative Binomial"),
               grid_args = list(ncol = 3))
```

![](../rmd_images/Comparing_Bayesian_Packages/unnamed-chunk-12-1.png)<!-- -->

``` r
#loo(roach_pois, roach_negbinom2)
```
