Titanic Data Analysis
================
Jesse Cambon
12 August, 2018

-   [Exploratory Graphs](#exploratory-graphs)
-   [Logistic Regression Model](#logistic-regression-model)
-   [Linear Regression Model](#linear-regression-model)

A modeling analysis of the titanic dataset.

To add: imputation, display of coefficients, graphical display of linear regression result

References: <https://stats.idre.ucla.edu/r/dae/logit-regression/>

``` r
library(tidyverse)
library(PASWR) #titanic3 dataset
library(wesanderson) # color palettes
library(formattable) # percent format
library(caret) # regression utilities
library(Hmisc) # capitalize function

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

``` r
fit <- glm(survived ~ sex + pclass + age ,family=binomial(link="logit"),data=titanic)

predictions <- titanic %>%
  dplyr::select(sex,pclass,age,survived) %>%
  mutate(prediction=predict(fit,newdata=titanic,type='response')) %>%
  mutate(prediction_binary=case_when(prediction >0.5 ~ 1, TRUE ~ 0))

summary(fit)
```

    ## 
    ## Call:
    ## glm(formula = survived ~ sex + pclass + age, family = binomial(link = "logit"), 
    ##     data = titanic)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.6399  -0.6979  -0.4336   0.6688   2.3964  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.522074   0.326702  10.781  < 2e-16 ***
    ## sexmale     -2.497845   0.166037 -15.044  < 2e-16 ***
    ## pclass2nd   -1.280570   0.225538  -5.678 1.36e-08 ***
    ## pclass3rd   -2.289661   0.225802 -10.140  < 2e-16 ***
    ## age         -0.034393   0.006331  -5.433 5.56e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1414.62  on 1045  degrees of freedom
    ## Residual deviance:  982.45  on 1041  degrees of freedom
    ##   (263 observations deleted due to missingness)
    ## AIC: 992.45
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# Confusion Matrix analysis with assumed cutoff
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

Linear Regression Model
-----------------------

A linear model of how much a passenger's fare cost

``` r
lm_fit <- lm(fare ~ sex + pclass + age + survived,data=titanic)

lm_predictions <- titanic %>%
  dplyr::select(sex,pclass,age,survived,fare) %>%
  mutate(prediction=predict(lm_fit,newdata=titanic,type='response'))

summary(lm_fit)
```

    ## 
    ## Call:
    ## lm(formula = fare ~ sex + pclass + age + survived, data = titanic)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -86.81 -12.71  -2.53   4.05 424.40 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 108.5874     6.2054  17.499  < 2e-16 ***
    ## sexmale     -11.4555     3.3133  -3.457 0.000567 ***
    ## pclass2nd   -72.0386     3.9511 -18.232  < 2e-16 ***
    ## pclass3rd   -81.1666     3.8453 -21.108  < 2e-16 ***
    ## age          -0.2715     0.1057  -2.569 0.010347 *  
    ## survived      0.5729     3.4520   0.166 0.868231    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 43.59 on 1039 degrees of freedom
    ##   (264 observations deleted due to missingness)
    ## Multiple R-squared:  0.3913, Adjusted R-squared:  0.3884 
    ## F-statistic: 133.6 on 5 and 1039 DF,  p-value: < 2.2e-16

``` r
ggplot(data=lm_predictions %>% mutate(sex=capitalize(as.character(sex))),
          aes(x = age, y = prediction, color = pclass)) +
geom_point() +
facet_grid(~factor(sex)) +
scale_y_continuous(labels=scales::comma) +
#theme(legend.margin=margin(0,0,0,0)) +
scale_color_manual(values=wes_palette('Moonrise3')) +
labs(title='Cost of Fare - Linear Regression') +
xlab('Age') +
ylab('Fare Cost') +
guides(color = guide_legend(title='Passenger Class',reverse=F,override.aes = list(size=2.5))) 
```

    ## Warning: Removed 263 rows containing missing values (geom_point).

![](Titanic_files/figure-markdown_github/linear-regression-1.png)
