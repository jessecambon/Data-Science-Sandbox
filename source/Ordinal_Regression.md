Ordinal Regression
================
Jesse Cambon
06 March, 2019

GAM ordinal regression: <https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/ocat.html> Example using polr: <https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/> Ordinal doc: <https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf>

``` r
#library(Hmisc)
library(MASS)
library(ordinal)
library(broom)
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.1.0       v purrr   0.3.0  
    ## v tibble  2.0.1       v dplyr   0.8.0.1
    ## v tidyr   0.8.2       v stringr 1.4.0  
    ## v readr   1.3.1       v forcats 0.4.0

    ## -- Conflicts -------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()
    ## x dplyr::select() masks MASS::select()
    ## x dplyr::slice()  masks ordinal::slice()

``` r
wine <- wine # make wine show up in the R studio environment

# train models
clm_model <- clm(rating ~ temp + judge , data=wine)
polr_model <- polr(rating ~ temp + judge, data=wine)

# Evaluate model
clm_stats <- glance(clm_model)
clm_coef <- tidy(clm_model,exponentiate=T) 

polr_stats <- glance(polr_model)
polr_coef <- tidy(polr_model,exponentiate=T)


clm_predictions <- unlist(predict(clm_model,type='class'))
polr_predictions <- predict(polr_model,type='class')


# Compare predictions of polr() and clm()
compare_models <- wine %>% 
  dplyr::select(rating) %>%
  # clm predictions returned as list for some reason
  # have to unlist it so we can put it in a column
  mutate(clm_pred=unlist(predict(clm_model,type='class')),
         polr_pred=predict(polr_model,type='class')) %>%
  mutate_all(as.numeric) %>% # convert from factor to numeric
  mutate(
  clm_resid=rating-clm_pred,
         polr_resid=rating-polr_pred)

mean(abs(compare_models$clm_resid))
```

    ## [1] 0.5555556

``` r
mean(abs(compare_models$polr_resid))
```

    ## [1] 0.5555556

``` r
# Make frequency tables
freq_preds <- compare_models %>% count(polr_pred,clm_pred)
freq_predcheck <- compare_models %>% count(rating,clm_pred)

# Chi square test
chisq.test(freq_preds)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  freq_preds
    ## X-squared = 21.698, df = 8, p-value = 0.005507

``` r
chisq.test(freq_predcheck)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  freq_predcheck
    ## X-squared = 38.016, df = 28, p-value = 0.09809

``` r
#Spearman correlation
cor(compare_models$rating,compare_models$clm_pred,method='spearman')
```

    ## [1] 0.6493969
