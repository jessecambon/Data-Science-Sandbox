Modeling the Titanic
================
Jesse Cambon
14 August, 2018

-   [Exploratory Graphs](#exploratory-graphs)
-   [Logistic Regression Model](#logistic-regression-model)
-   [Linear Regression Model](#linear-regression-model)

A modeling analysis of the titanic dataset using linear and logistic regression.

To add: imputation, log-binomial model (risk ratio)

References: <https://stats.idre.ucla.edu/r/dae/logit-regression/>

``` r
library(tidyverse)
library(PASWR) #titanic3 dataset
library(wesanderson) # color palettes
library(formattable) # percent format
library(caret) # regression utilities
library(Hmisc) # capitalize function
library(broom) # model display capabilities
library(xtable) # pretty table
library(knitr)  
library(kableExtra)
library(MASS) # confint for glm

titanic <- titanic3 %>% as_tibble()

titanic_summ <- titanic %>%
  count(survived,pclass,sex) %>%
  mutate(sex=capitalize(as.character(sex))) %>%
  group_by(pclass,sex) %>%
  mutate(perc_surv_num=n/sum(n),
    perc_surv_char=as.character(percent(n/sum(n),0))) %>%
  ungroup()

# Set default ggplot theme
theme_set(theme_bw()+
  theme(legend.position = "top",
            plot.subtitle= element_text(face="bold",hjust=0.5),
            plot.title = element_text(lineheight=1, face="bold",hjust = 0.5)))
```

Exploratory Graphs
------------------

``` r
ggplot(data=titanic_summ,
       aes(x = fct_rev(pclass), y=perc_surv_num,fill = factor(survived,labels=c('No','Yes')))) +
facet_grid(~factor(sex)) +
geom_bar(stat='identity') +
coord_flip() +
  geom_text(data=titanic_summ,aes(label = ifelse(perc_surv_num > 0.07 ,perc_surv_char,NA)),
    size = 3,position = position_stack(vjust = 0.5)) +
scale_fill_manual(values=wes_palette('FantasticFox1')[c(3,4)]) +
theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
labs(title='') +
xlab('Passenger Class') +
ylab('') +
guides(fill = guide_legend(title='Survived',reverse=T)) # reverse legend order
```

    ## Warning: Removed 1 rows containing missing values (geom_text).

![](Titanic_files/figure-markdown_github/explore-1.png)

Logistic Regression Model
-------------------------

We will use the brier score as one measurement of accuracy for our model: <https://en.wikipedia.org/wiki/Brier_score> The book 'Superforecasting' by Philip Tetlock has a good discussion of brier scores.

``` r
log_fit <- glm(survived ~ sex + pclass + age ,family=binomial(link="logit"),data=titanic)

predictions <- titanic %>%
  dplyr::select(sex,pclass,age,survived) %>%
  mutate(prediction=predict(log_fit,newdata=titanic,type='response')) %>%
  mutate(prediction_binary=case_when(prediction >0.5 ~ 1, TRUE ~ 0),
         brier_score=abs(prediction-survived))

#summary(fit)

log_confint <- confint(log_fit) %>% tidy()
```

    ## Waiting for profiling to be done...

``` r
colnames(log_confint) <- c('Term','LCLM','UCLM')

log_info <- glance(log_fit) %>% 
  mutate(meanBrierScore=mean(predictions$brier_score,na.rm=T)) %>%
  dplyr::select(meanBrierScore,everything())

log_terms <- tidy(log_fit) %>% rename(Coefficient=estimate,Term=term) %>%
   # Order by largest coefficient but put intercept term on bottom
  arrange(Term=='(Intercept)',desc(Coefficient)) %>%
  left_join(log_confint,by='Term') %>%
  dplyr::select(Term,Coefficient,LCLM,UCLM,everything())

# An analysis of our model's classification accuracy
confusionMatrix(factor(predictions$prediction_binary), factor(predictions$survived))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 710 199
    ##          1  99 301
    ##                                           
    ##                Accuracy : 0.7723          
    ##                  95% CI : (0.7487, 0.7948)
    ##     No Information Rate : 0.618           
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.4987          
    ##  Mcnemar's Test P-Value : 9.756e-09       
    ##                                           
    ##             Sensitivity : 0.8776          
    ##             Specificity : 0.6020          
    ##          Pos Pred Value : 0.7811          
    ##          Neg Pred Value : 0.7525          
    ##              Prevalence : 0.6180          
    ##          Detection Rate : 0.5424          
    ##    Detection Prevalence : 0.6944          
    ##       Balanced Accuracy : 0.7398          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
ggplot(data=predictions %>% mutate(sex=capitalize(as.character(sex))),
          aes(x = age, y = prediction, color = pclass)) +
geom_point() +
facet_grid(~factor(sex)) +
scale_y_continuous(labels=scales::percent) +
theme(legend.margin=margin(0,0,0,0)) +
scale_color_manual(values=wes_palette('Moonrise3')) +
labs(title='Probability of Survival - Logistic Regression') +
xlab('Age') +
ylab('Survival Probability') +
guides(color = guide_legend(title='Passenger Class',reverse=F,override.aes = list(size=2.5))) 
```

    ## Warning: Removed 263 rows containing missing values (geom_point).

![](Titanic_files/figure-markdown_github/logistic-regression-1.png)

``` r
ggplot(predictions, aes(prediction))+
  geom_histogram(binwidth=0.02,aes(fill=factor(survived,labels=c('No','Yes'))),
    col='black') + 
  theme(legend.pos='top') +
  scale_fill_manual(values=wes_palette('Moonrise3')) +
  scale_x_continuous(labels=scales::percent) +
  labs(title="Logistic Regression Probability Distribution") +
xlab('Survival Probability') +
ylab('Count') +
guides(fill = guide_legend(title='Survived')) 
```

    ## Warning: Removed 263 rows containing non-finite values (stat_bin).

![](Titanic_files/figure-markdown_github/logistic-regression-2.png)

``` r
ggplot(predictions, aes(brier_score)) +
  geom_histogram(binwidth=0.02,aes(fill=factor(survived,labels=c('No','Yes'))),
                 col='black') +
  labs(title="Brier Score Distribution") +
    scale_fill_manual(values=wes_palette('Moonrise3')) +
xlab('Brier Score') +
ylab('Count') +
guides(fill = guide_legend(title='Survived')) 
```

    ## Warning: Removed 263 rows containing non-finite values (stat_bin).

![](Titanic_files/figure-markdown_github/logistic-regression-3.png)

``` r
kable(log_info %>% 
        dplyr::select(-df.residual,-df.null,-deviance),format='markdown') %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

|  meanBrierScore|  null.deviance|     logLik|       AIC|       BIC|
|---------------:|--------------:|----------:|---------:|---------:|
|       0.3013701|        1414.62|  -491.2266|  992.4531|  1017.217|

``` r
kable(log_terms,format='markdown') %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

| Term        |  Coefficient|        LCLM|        UCLM|  std.error|   statistic|  p.value|
|:------------|------------:|-----------:|-----------:|----------:|-----------:|--------:|
| age         |   -0.0343932|  -0.0469813|  -0.0221378|  0.0063310|   -5.432511|    1e-07|
| pclass2nd   |   -1.2805697|  -1.7280290|  -0.8430583|  0.2255382|   -5.677840|    0e+00|
| pclass3rd   |   -2.2896606|  -2.7406095|  -1.8545158|  0.2258019|  -10.140129|    0e+00|
| sexmale     |   -2.4978447|  -2.8290589|  -2.1776176|  0.1660365|  -15.043949|    0e+00|
| (Intercept) |    3.5220740|   2.8967273|   4.1786006|  0.3267022|   10.780686|    0e+00|

Linear Regression Model
-----------------------

A linear model of passenger fare cost.

``` r
lm_fit <- lm(fare ~ sex + pclass + age + survived,data=titanic)

# Calculate confidence limit
lm_confint <- confint(lm_fit) %>% tidy()
colnames(lm_confint) <- c('Term','LCLM','UCLM')

lm_predictions <- titanic %>%
  dplyr::select(sex,pclass,age,survived,fare) %>%
  mutate(prediction=predict(lm_fit,newdata=titanic)) %>%
  mutate(residual=fare-prediction)

lm_info <- glance(lm_fit)

lm_terms <- tidy(lm_fit) %>%
  rename(Term=term,Coefficient=estimate) %>%
  left_join(lm_confint,by='Term' ) %>%
  dplyr::select(Term,Coefficient,LCLM,UCLM,everything())

#summary(lm_fit)

# Histogram of Residuals
ggplot(lm_predictions, aes(residual)) +
  geom_histogram(bins=30) +
facet_grid(~pclass,scales='free_x') +
scale_x_continuous(labels=scales::dollar) +
  labs(title="Residual Distribution by Passenger Class") +
xlab('Residual') +
ylab('Count') 
```

    ## Warning: Removed 264 rows containing non-finite values (stat_bin).

![](Titanic_files/figure-markdown_github/linear-regression-1.png)

``` r
ggplot(data=lm_predictions %>% mutate(sex=capitalize(as.character(sex))),
          aes(x = age, y = prediction, color = pclass,group=1)) +
geom_point() +
facet_grid(~factor(sex)) +
scale_y_continuous(labels=scales::dollar) +
#theme(legend.margin=margin(0,0,0,0)) +
scale_color_manual(values=wes_palette('Moonrise3')) +
labs(title='Cost of Fare - Linear Regression') +
xlab('Age') +
ylab('Fare Cost') +
guides(color = guide_legend(title='Passenger Class',reverse=F,override.aes = list(size=2.5))) 
```

    ## Warning: Removed 263 rows containing missing values (geom_point).

![](Titanic_files/figure-markdown_github/linear-regression-2.png)

``` r
ggplot(data=lm_predictions %>% mutate(sex=capitalize(as.character(sex))),
          aes(x = prediction, y = residual, color = sex)) +
geom_point() +
facet_grid(~pclass,scales='free_x') +
geom_hline(yintercept=0) + # horizontal line at 0 residual
#geom_smooth(method="lm",show.legend=F,size=0.5) +
scale_x_continuous(labels=scales::dollar) +
scale_y_continuous(labels=scales::dollar) +
#theme(legend.pos='none') +
scale_color_manual(values=wes_palette('Moonrise3')) +
labs(title='Residuals vs Predictions by Passenger Class') +
xlab('Prediction') +
ylab('Residual') + 
guides(color = guide_legend(title='Gender',reverse=F,override.aes = list(size=2.5))) 
```

    ## Warning: Removed 264 rows containing missing values (geom_point).

![](Titanic_files/figure-markdown_github/linear-regression-3.png)

``` r
ggplot(data=lm_predictions %>% mutate(sex=capitalize(as.character(sex))),
          aes(x = age, y = residual, color = sex)) +
geom_point() +
facet_grid(~pclass) +
  geom_hline(slope=0,yintercept=0) + # horizontal line at 0 residual
scale_y_continuous(labels=scales::dollar) +
theme(legend.margin=margin(0,0,0,0)) +
scale_color_manual(values=wes_palette('Moonrise3')) +
labs(title='Residuals By Passenger Class') +
xlab('Age') +
ylab('Residual') +
guides(color = guide_legend(title='Gender',reverse=F,override.aes = list(size=2.5))) 
```

    ## Warning: Ignoring unknown parameters: slope

    ## Warning: Removed 264 rows containing missing values (geom_point).

![](Titanic_files/figure-markdown_github/linear-regression-4.png)

``` r
ggplot(data=lm_terms,
          aes(x = reorder(Term,-Coefficient), y = Coefficient)) +
geom_point() +
coord_flip() +
geom_pointrange(mapping=aes(ymin=LCLM, ymax=UCLM)) + 
scale_color_manual(values=wes_palette('Moonrise3')) +
labs(title='Linear Model Coefficients with Confidence Intervals') +
xlab('Term')
```

![](Titanic_files/figure-markdown_github/linear-regression-5.png)

``` r
kable((lm_info %>% dplyr::select(-df.residual,-logLik,-deviance)),format='markdown') %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

|  r.squared|  adj.r.squared|     sigma|  statistic|  p.value|   df|       AIC|       BIC|
|----------:|--------------:|---------:|----------:|--------:|----:|---------:|---------:|
|  0.3913351|       0.388406|  43.58534|    133.603|        0|    6|  10862.73|  10897.39|

``` r
kable(lm_terms,format='markdown') %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

| Term        |  Coefficient|         LCLM|         UCLM|  std.error|    statistic|    p.value|
|:------------|------------:|------------:|------------:|----------:|------------:|----------:|
| (Intercept) |  108.5873881|   96.4108392|  120.7639370|  6.2054020|   17.4988484|  0.0000000|
| sexmale     |  -11.4555074|  -17.9569857|   -4.9540290|  3.3132776|   -3.4574548|  0.0005674|
| pclass2nd   |  -72.0386084|  -79.7916925|  -64.2855243|  3.9511198|  -18.2324538|  0.0000000|
| pclass3rd   |  -81.1666077|  -88.7119998|  -73.6212157|  3.8452759|  -21.1081360|  0.0000000|
| age         |   -0.2715108|   -0.4789193|   -0.0641024|  0.1056993|   -2.5687094|  0.0103465|
| survived    |    0.5728532|   -6.2008974|    7.3466038|  3.4520327|    0.1659466|  0.8682312|
