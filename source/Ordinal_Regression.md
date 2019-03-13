Ordinal Regression
================
Jesse Cambon
13 March, 2019

-   [Extract data from smooths and plot](#extract-data-from-smooths-and-plot)
-   [Alternatively, Plot Smooth Terms with MgcViz](#alternatively-plot-smooth-terms-with-mgcviz)

GAM ordinal regression: <https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/ocat.html> Example using polr: <https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/> Ordinal doc: <https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf>

``` r
#library(Hmisc)
library(MASS) # polr()
library(mgcv) # gam model
```

    ## Loading required package: nlme

    ## This is mgcv 1.8-27. For overview type 'help("mgcv-package")'.

``` r
library(mgcViz) # gam visualization
```

    ## Loading required package: qgam

    ## Loading required package: ggplot2

    ## Loading required package: rgl

    ## 
    ## Attaching package: 'mgcViz'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     qqline, qqnorm, qqplot

``` r
library(ordinal) # clm()
```

    ## 
    ## Attaching package: 'ordinal'

    ## The following objects are masked from 'package:nlme':
    ## 
    ##     ranef, VarCorr

``` r
library(broom)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v tibble  2.0.1       v purrr   0.3.0  
    ## v tidyr   0.8.2       v dplyr   0.8.0.1
    ## v readr   1.3.1       v stringr 1.4.0  
    ## v tibble  2.0.1       v forcats 0.4.0

    ## -- Conflicts --------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::collapse() masks nlme::collapse()
    ## x dplyr::filter()   masks stats::filter()
    ## x dplyr::lag()      masks stats::lag()
    ## x dplyr::select()   masks MASS::select()
    ## x dplyr::slice()    masks ordinal::slice()

``` r
Mydiamonds <- diamonds %>% 
  # convert factor to numeric for gam model
  mutate(cutN=as.numeric(cut))

    # make wine show up in the R studio environment

outcomeVar <- 'cut'
predictors <- 'carat  + price + table'

# Construct formula from strings
lmformula <- as.formula(str_c(outcomeVar,' ~ ',predictors))

# train ordinal logistic models
clm_model <- clm(lmformula, data=diamonds)
polr_model <- polr(lmformula, data=diamonds)
# train ordinal GAM model (R is the number of outcome categories)
gam_model <- gam(cutN ~ carat + s(price,k=9) + s(table,k=12),family=ocat(R=5),data=Mydiamonds) 

gam.check(gam_model)
```

![](Ordinal_Regression_files/figure-markdown_github/unnamed-chunk-1-1.png)

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 7 iterations.
    ## Gradient range [-0.0006234016,0.007978845]
    ## (score 65689.64 & scale 1).
    ## Hessian positive definite, eigenvalue range [1.569822,16878.22].
    ## Model rank =  21 / 21 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##             k'   edf k-index p-value    
    ## s(price)  8.00  7.72    0.96  <2e-16 ***
    ## s(table) 11.00  8.97    0.98    0.07 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Evaluate models
clm_stats <- glance(clm_model)
clm_coef <- tidy(clm_model,exponentiate=T) 

polr_stats <- glance(polr_model)
polr_coef <- tidy(polr_model,exponentiate=T)

gam_stats <- glance(gam_model)
gam_Lcoef <-  tidy(gam_model,parametric=T) # get linear coefficients
gam_Scoef <-  tidy(gam_model,parametric=F) # get smooth term coefficients

# Extract probability predictions from GAM
gam_probs <- predict(gam_model,type='response') %>% 
  # remove "V" from column names so we now have the class labels
  as.data.frame() %>% rename_all(list(replace= ~str_replace_all(.,'V',''))) %>% 
  mutate(obs_num=1:nrow(.)) %>%
  gather(class,prob,-obs_num) %>%
  mutate(class=as.numeric(class)) %>% arrange(obs_num,class)

# Extract class predictions
gam_pred <- gam_probs %>% group_by(obs_num) %>%
  filter(prob==max(prob))

# Compare predictions of polr() and clm()
compare_models <- Mydiamonds %>% 
  # clm predictions returned as list for some reason
  # have to unlist it so we can put it in a column
  mutate(clm_pred=unlist(predict(clm_model,type='class')),
         polr_pred=predict(polr_model,type='class'),
         gam_pred=gam_pred %>% pull(class)) %>%
  mutate_all(as.numeric)  # convert from factor to numeric

# Make frequency tables
# freq_preds <- compare_models %>% count(polr_pred,clm_pred)
# freq_predcheck <- compare_models %>% count(cut,clm_pred)

# Chi square test
# chisq.test(freq_preds)
# chisq.test(freq_predcheck)

#Spearman correlations
cor(compare_models$cut,compare_models$clm_pred,method='spearman')
```

    ## [1] 0.4313114

``` r
cor(compare_models$cut,compare_models$polr_pred,method='spearman')
```

    ## [1] 0.431316

``` r
cor(compare_models$cut,compare_models$gam_pred,method='spearman')
```

    ## [1] 0.4808663

``` r
# Confusion matrixes 

check_gam <- compare_models %>% count(cut,gam_pred) %>%
  spread(cut,n,fill=0)

check_clm <- compare_models %>% count(cut,clm_pred) %>%
  spread(cut,n,fill=0)
```

Extract data from smooths and plot
----------------------------------

This method allows us some more direct control over how we plot the smooth terms since we extract the plot data. Alternatively, mgcViz (shown below) can be used.

``` r
# Returns the data to plot all smooth turns in a gam model object
# 100 points per plot
smooth_data <- function(gam_model) {
  # select=0 prevents plots being shown on screen
  gam_viz <- plot(gam_model, rug=FALSE,select=0)
  
  num_smooths <- length(gam_viz) # number of smooth terms
  smooth_df <- tibble() # initialize a dataframe
  
  for (i in 1:num_smooths) {
     print(gam_viz[[i]]$xlab)
    
    # extract and append data we want
    smooth_df <- smooth_df %>%
      bind_rows(tibble( xlab=gam_viz[[i]]$xlab,
                        ylab=gam_viz[[i]]$ylab,
                        x=gam_viz[[i]]$x,
                        fit=gam_viz[[i]]$fit,
                        se=gam_viz[[i]]$se
                        ))
  }
  return(smooth_df)
} 

gam_smoothdata <- smooth_data(gam_model)
```

    ## [1] "price"
    ## [1] "table"

``` r
ggplot(gam_smoothdata, 
      aes(x, fit)) + 
  facet_wrap(~xlab,scales='free') +
  geom_line() +
  theme_minimal() +
 geom_line(aes(y=fit+(2*se)),linetype='dashed') +
 geom_line(aes(y=fit-(2*se)),linetype='dashed') +
  scale_y_continuous() +
  scale_x_continuous(labels=scales::comma)
```

![](Ordinal_Regression_files/figure-markdown_github/unnamed-chunk-3-1.png)

Alternatively, Plot Smooth Terms with MgcViz
--------------------------------------------

``` r
gam_viz <- getViz(gam_model)

plot(sm(gam_viz, 1)) +
  l_fitLine(colour = "red") + 
#  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
    l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
 #   l_points(shape = 19, size = 1, alpha = 0.1) +
  theme_classic()
```

![](Ordinal_Regression_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
plot(sm(gam_viz, 2)) +
  l_fitLine(colour = "red") + 
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) +
  theme_classic()
```

![](Ordinal_Regression_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
print(plot(gam_viz, allTerms = T), pages = 1)
```

![](Ordinal_Regression_files/figure-markdown_github/unnamed-chunk-5-1.png)
