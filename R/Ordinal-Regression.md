Ordinal Regression
================
Jesse Cambon
22 November, 2019

GAM ordinal regression:
<https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/ocat.html>
Example using polr:
<https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/>
Explanation of GAM interpretation:
<https://stats.stackexchange.com/questions/226645/generalized-additive-model-interpretation-with-ordered-categorical-family-in-r>

``` r
#library(Hmisc)
library(MASS) # polr()
library(car)
```

    ## Loading required package: carData

``` r
library(mgcv) # gam model
```

    ## Loading required package: nlme

    ## This is mgcv 1.8-28. For overview type 'help("mgcv-package")'.

``` r
library(mgcViz) # gam visualization
```

    ## Loading required package: qgam

    ## Loading required package: ggplot2

    ## Loading required package: rgl

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## Registered S3 methods overwritten by 'lme4':
    ##   method                          from
    ##   cooks.distance.influence.merMod car 
    ##   influence.merMod                car 
    ##   dfbeta.influence.merMod         car 
    ##   dfbetas.influence.merMod        car

    ## Registered S3 method overwritten by 'mgcViz':
    ##   method from  
    ##   +.gg   GGally

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

    ## ── Attaching packages ───────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ tibble  2.1.3     ✔ purrr   0.3.3
    ## ✔ tidyr   1.0.0     ✔ dplyr   0.8.3
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::collapse() masks nlme::collapse()
    ## ✖ dplyr::filter()   masks stats::filter()
    ## ✖ dplyr::lag()      masks stats::lag()
    ## ✖ dplyr::recode()   masks car::recode()
    ## ✖ dplyr::select()   masks MASS::select()
    ## ✖ dplyr::slice()    masks ordinal::slice()
    ## ✖ purrr::some()     masks car::some()

``` r
# Find frequency counts for all variables in var list

var_freq <- function(data,var) {
  var <- rlang::sym(var)
  print(var)
#  print(quo_name(var))
  if (is.factor(data %>% pull(!!var)) | is.character(data %>% pull(!!var))) {
  return(data %>% count(!!var) %>% mutate(term=quo_name(var)) %>%
          rename(level=!!var) %>%
          mutate(level=as.character(level), # convert to char
                 is_categorical=1))
  } else {
    return(tibble())
  }
}

# Iterate through an entire dataset and return a dataset with all
# frequencies
find_all_freqs <- function(data,var_list) {
  all_freqs <- tibble()
  for (var in var_list) {
    all_freqs <- all_freqs %>%
      bind_rows(var_freq(data,var))
  }
  return(all_freqs)
}

# obtain list of variables in a model. Remove smooth terms (s())
obtain_model_varlist <- function(model_obj) {
    var_list_raw <- unlist(strsplit(as.character(model_obj$formula[3]),split=' \\+ '))
    # Remove smooth terms (s())
    return(var_list_raw[!str_detect(var_list_raw,'^s\\(')])
}

# adds term_name field to a tidy dataframe which includes frequency count
add_termnames <- function(data,term_freqs,var_list) {
  # Regexs to match the varname (when it begins a string)
  varregex <- paste(str_replace(var_list,'^','\\^'), collapse = "|")

  return(
  data %>%
  mutate(term_name = str_extract(term,varregex),
         level = case_when(!is.na(term_name) ~ str_replace(term,varregex,""))) %>%
  # add in frequency counts and labels
  left_join(term_freqs,by=c('term_name'='term','level')) %>%
  mutate(label=case_when(is.na(n) ~ term, # if not categorical than use original label
        is_categorical == 1 ~ str_c(term_name,': ', level,' (',scales::comma(n),')'),
                TRUE ~ str_c(level,' (',scales::comma(n),')')))

  )

}
```

``` r
Mydiamonds <- diamonds %>% 
  # convert factor to numeric for gam model
  mutate(cutN=as.numeric(cut),
          # convert to non-ordered factors
         color=factor(color,ordered=F),
         clarity=factor(clarity,ordered=F)
         )

    # make wine show up in the R studio environment

outcomeVar <- 'cut'
predictors <- 'carat + color + clarity'

# Construct formula from strings
lmformula <- as.formula(str_c(outcomeVar,' ~ ',predictors))

# train ordinal logistic models
clm_model <- clm(lmformula, data=Mydiamonds)
polr_model <- polr(lmformula, data=Mydiamonds)
# train ordinal GAM model (R is the number of outcome categories)
gam_model <- gam(cutN ~ s(carat) + color + clarity,family=ocat(R=5),data=Mydiamonds) 

gam.check(gam_model)
```

![](/Users/jessecambon/Documents/Data-Science-Codex/rmd_images/Ordinal-Regression/unnamed-chunk-2-1.png)<!-- -->

    ## 
    ## Method: REML   Optimizer: outer newton
    ## full convergence after 5 iterations.
    ## Gradient range [0.0009785299,0.008466754]
    ## (score 72378.84 & scale 1).
    ## Hessian positive definite, eigenvalue range [3.780712,17487.96].
    ## Model rank =  23 / 23 
    ## 
    ## Basis dimension (k) checking results. Low p-value (k-index<1) may
    ## indicate that k is too low, especially if edf is close to k'.
    ## 
    ##            k'  edf k-index p-value    
    ## s(carat) 9.00 8.78    0.93  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Check for collinearity
concurvity(gam_model)
```

    ##               para s(carat)
    ## worst    0.9879559 0.238848
    ## observed 0.9879559 0.101962
    ## estimate 0.9879559 0.184054

``` r
vif(polr_model)
```

    ## 
    ## Re-fitting to get Hessian

    ## Warning in vif.default(polr_model): No intercept: vifs may not be sensible.

    ##               GVIF Df GVIF^(1/(2*Df))
    ## carat     2.474925  1        1.573189
    ## color   116.228853  6        1.486310
    ## clarity 206.250047  7        1.463234

``` r
# Find categorical variables and the
# frequency counts of their levels
gam_varlist <- obtain_model_varlist(gam_model)
gam_varfreqs <- find_all_freqs(Mydiamonds,gam_varlist)
```

    ## color
    ## clarity

``` r
# Evaluate models
clm_stats <- glance(clm_model)
clm_coef <- tidy(clm_model,exponentiate=T) 

polr_stats <- glance(polr_model)
polr_coef <- tidy(polr_model,exponentiate=T)

gam_stats <- glance(gam_model)
gam_Lcoef <-  tidy(gam_model,parametric=T) %>% # get parametric coefficients
  add_termnames(gam_varfreqs,gam_varlist)
gam_Scoef <-  tidy(gam_model,parametric=F) # get smooth term coefficients

# gam_allpvalues <- gam_Lcoef %>%
#   dplyr::select(term,p.value) %>%
#   bind_rows(gam_Scoef %>% select(term,p.value)) %>%
#   arrange(p.value)

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

    ## [1] 0.08136128

``` r
cor(compare_models$cut,compare_models$polr_pred,method='spearman')
```

    ## [1] 0.08136128

``` r
cor(compare_models$cut,compare_models$gam_pred,method='spearman')
```

    ## [1] 0.159523

``` r
ggplot(data=gam_Lcoef %>% filter(label != '(Intercept)'),
          aes(x = reorder(label,-estimate), y = exp(estimate))) +
geom_point() +
  scale_y_continuous(breaks=seq(0,10,2),limits=c(0,10)) +
geom_hline(yintercept=1,color='grey') +
coord_flip() +
  theme_classic() +
#geom_pointrange(mapping=aes(ymin=LCLM, ymax=UCLM)) + 
labs(title='Odds Ratios of Parametric Terms',
     caption='Sample sizes shown in ()') +
xlab('Term') + ylab('Odds Ratio')
```

![](/Users/jessecambon/Documents/Data-Science-Codex/rmd_images/Ordinal-Regression/unnamed-chunk-4-1.png)<!-- -->

``` r
# Confusion matrixes 

check_gam <- compare_models %>% count(cut,gam_pred) %>%
  spread(cut,n,fill=0)

check_clm <- compare_models %>% count(cut,clm_pred) %>%
  spread(cut,n,fill=0)
```

## Extract data from smooths and plot

This method allows us some more direct control over how we plot the
smooth terms since we extract the plot data. Alternatively, mgcViz
(shown below) can be used.

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

    ## [1] "carat"

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

![](/Users/jessecambon/Documents/Data-Science-Codex/rmd_images/Ordinal-Regression/unnamed-chunk-6-1.png)<!-- -->

## Alternatively, Plot Smooth Terms with MgcViz

``` r
gam_viz <- getViz(gam_model)

plot(sm(gam_viz, 1)) +
  l_fitLine(colour = "red") + 
#  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
    l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
 #   l_points(shape = 19, size = 1, alpha = 0.1) +
  theme_classic()
```

![](/Users/jessecambon/Documents/Data-Science-Codex/rmd_images/Ordinal-Regression/unnamed-chunk-7-1.png)<!-- -->

``` r
print(plot(gam_viz, allTerms = T), pages = 1)
```

![](/Users/jessecambon/Documents/Data-Science-Codex/rmd_images/Ordinal-Regression/unnamed-chunk-8-1.png)<!-- -->
