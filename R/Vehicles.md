Machine Learning on the Vehicles Dataset
================
Jesse Cambon
22 November, 2019

Predict vehicle fuel economy.

## Setup

``` r
library(fueleconomy)
library(tidyverse)
library(broom)
library(caret)
library(skimr)
library(knitr)
library(kableExtra)

# Set seed for reproducibility
set.seed(45)
```

## Build Model

``` r
vehicles_clean <- vehicles %>%
  filter(displ !=0 & !is.na(cyl))

skim(vehicles_clean)
```

    ## Skim summary statistics
    ##  n obs: 33381 
    ##  n variables: 12 
    ## 
    ## ── Variable type:character ───────────────────────────────────────────────────────────
    ##  variable missing complete     n min max empty n_unique
    ##     class       0    33381 33381   4  34     0       34
    ##     drive       0    33381 33381  13  26     0        7
    ##      fuel       0    33381 33381   3  27     0       12
    ##      make       0    33381 33381   3  34     0      124
    ##     model       0    33381 33381   1  39     0     3174
    ##     trans       0    33381 33381   0  32     2       47
    ## 
    ## ── Variable type:integer ─────────────────────────────────────────────────────────────
    ##  variable missing complete     n     mean       sd   p0  p25   p50   p75
    ##       cty       0    33381 33381    17.37     4.58    6   15    17    20
    ##       cyl       0    33381 33381     5.77     1.74    2    4     6     6
    ##       hwy       0    33381 33381    23.46     5.73    9   19    23    27
    ##        id       0    33381 33381 17014.27 10076.89    1 8346 16697 25228
    ##      year       0    33381 33381  1999.09     9.37 1984 1991  1999  2007
    ##   p100     hist
    ##     53 ▁▇▆▁▁▁▁▁
    ##     16 ▁▇▇▅▁▁▁▁
    ##     61 ▁▇▇▃▁▁▁▁
    ##  34932 ▇▇▇▇▇▇▆▇
    ##   2015 ▇▇▇▅▆▇▇▆
    ## 
    ## ── Variable type:numeric ─────────────────────────────────────────────────────────────
    ##  variable missing complete     n mean   sd p0 p25 p50 p75 p100     hist
    ##     displ       0    33381 33381 3.35 1.36  1 2.3   3 4.3  8.4 ▃▇▅▃▃▂▁▁

``` r
lm_fit <- lm(log(cty) ~ drive + year + log(cyl) + log(displ),
             data=vehicles_clean)

lm_stats <- glance(lm_fit)
lm_terms <- tidy(lm_fit,conf.int=T)
```

# Train Models With Caret

``` r
# Regression formula with no variable transformations
alpha_formula <- cty ~ year + fuel +
               cyl + displ

# Regression formula with transformed variables
log_formula <- log(cty) ~ drive + year + fuel +
               log(cyl) + log(displ)


# Use k-fold cross-validation
TC <- trainControl(method="cv", number=5)

caret_lm <- train(alpha_formula , vehicles_clean,method='lm',trControl=TC)
```

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
    ## fit may be misleading

``` r
caret_gamSplines <- train(alpha_formula,vehicles_clean,method='gamSpline',trainControl=TC)
```

    ## Loading required package: gam

    ## Loading required package: splines

    ## Loading required package: foreach

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

    ## Loaded gam 1.16.1

    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored

    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored
    
    ## Warning in model.matrix.default(mt, mf, contrasts): non-list contrasts
    ## argument ignored

``` r
caret_glm <- train(log_formula , vehicles_clean,method='glm',trControl=TC)
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type
    ## == : prediction from a rank-deficient fit may be misleading

``` r
#caret_nb <- train(alpha_formula,vehicles_clean,method='glm.nb',trainControl=TC)
# gam with loess and splines from gam package
#caret_gamLoess <- train(alpha_formula,vehicles_clean,method='gamLoess',trainControl=TC)

caret_glmnet <- train(log_formula , vehicles_clean,method='glmnet',trControl=TC,tuneLength=5)
caret_knn <- train(log_formula , vehicles_clean,method='knn',trControl=TC)
```

## Compare Models

``` r
resamps <- resamples(list(lm=caret_lm,
                          glm=caret_glm,
                          glmnet=caret_glmnet,
                          knn=caret_knn))

# Accuracy comparison
dotplot(resamps,metric='Rsquared')
```

![](/Users/jessecambon/Documents/Data-Science-Codex/rmd_images/Vehicles/compare-models-1.png)<!-- -->

``` r
# Difference in accuracy
bwplot(diff(resamps,metric='Rsquared'))
```

![](/Users/jessecambon/Documents/Data-Science-Codex/rmd_images/Vehicles/compare-models-2.png)<!-- -->
