Machine Learning with Caret
================
Jesse Cambon
21 September, 2018

-   [References](#references)
-   [Setup](#setup)
-   [Build Model](#build-model)
-   [Glmnet (Elastic Net) Model](#glmnet-elastic-net-model)

Demonstrate a machine learning workflow with caret

References
----------

-   <https://topepo.github.io/caret/model-training-and-tuning.html>
-   <https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html>

Setup
-----

``` r
library(mlbench) # machine learning reference datasets
library(tidyverse)
library(broom)
library(caret)
library(skimr)
library(knitr)
library(kableExtra)

# Set seed for reproducibility
set.seed(45)
```

Build Model
-----------

``` r
data(BreastCancer)

skim(BreastCancer)

BC <- BreastCancer %>% as_tibble() %>%
  dplyr::select(-Id) %>%
  # should really use imputation but we'll just drop nas for now
  drop_na() 

# Use k-fold cross-validation
TC <- trainControl(method="cv", number=5)

# Train some models

# Neural Net
nnet <- train(Class ~ . , BC,method='nnet',trControl=TC)

# Gradient Boosted Machine
gbm <- train(Class ~ . , BC,method='gbm',trControl=TC)

# Radial SVM
svmrad <- train(Class ~ . , BC,method='svmRadial',trControl=TC)

# Elastic-net
glmnet <- train(Class ~ . , BC,method='glmnet',trControl=TC,tuneLength=5)

# Logistic regression - did not converge
glm <- train(Class ~ . , BC,method='glm',trControl=TC)
```

``` r
# Look at results of Glmnet model

# Extract coefficients from optimal model
glm_coeff <- coef(glmnet$finalModel,glmnet$finalModel$lambdaOpt) %>% 
  as.matrix() %>% as.data.frame() %>%
  rownames_to_column('Variable') %>%
  as_tibble() %>%
  rename(Coefficient=2) %>%
  arrange(desc(abs(Coefficient)))


# Combine variable importance data with coefficients
varImportance <- varImp(glmnet)$importance %>% 
  rownames_to_column('Variable') %>%
  rename(Importance=2) %>%
  arrange(desc(Importance)) %>%
  full_join(glm_coeff,by='Variable') %>%
  filter(Coefficient != 0) 
```

``` r
resamps <- resamples(list(nnet=nnet,
                          glmnet=glmnet,
                          svmrad=svmrad,
                          gbm=gbm,
                          glm=glm))

# Accuracy comparison
dotplot(resamps, metric = "Accuracy")
```

![](Caret_files/figure-markdown_github/results-1.png)

``` r
# Difference in accuracy
bwplot(diff(resamps))
```

![](Caret_files/figure-markdown_github/results-2.png)

Glmnet (Elastic Net) Model
--------------------------

``` r
kable(varImportance,format='markdown') %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

| Variable          |   Importance|  Coefficient|
|:------------------|------------:|------------:|
| Cl.thickness.L    |  100.0000000|    3.4100769|
| Cell.shape.L      |   57.9989495|    1.9778088|
| Cell.size.L       |   56.7390611|    1.9348456|
| Marg.adhesion.L   |   56.7390084|    1.9348438|
| Bare.nuclei10     |   50.7586916|    1.7309104|
| Bare.nuclei9      |   45.7678445|    1.5607187|
| Bare.nuclei6      |   41.2990307|    1.4083287|
| Normal.nucleoli10 |   37.7832016|    1.2884362|
| Cell.shape.Q      |   36.2651756|   -1.2366704|
| Bl.cromatin5      |   33.2490293|    1.1338175|
| Marg.adhesion^9   |   31.7087950|    1.0812943|
| Normal.nucleoli4  |   29.5420557|    1.0074068|
| Normal.nucleoli9  |   24.0973080|    0.8217367|
| Epith.c.size^4    |   22.5493010|    0.7689485|
| Normal.nucleoli2  |   22.3438540|   -0.7619426|
| Cell.size.C       |   17.1403971|    0.5845007|
| Bl.cromatin7      |   16.0856470|    0.5485329|
| Cl.thickness^8    |   15.3023494|   -0.5218219|
| Cell.size^8       |   15.2188813|    0.5189756|
| Bare.nuclei7      |   13.2596843|    0.4521654|
| Bare.nuclei3      |   12.0190659|    0.4098594|
| Bare.nuclei4      |   11.1653523|    0.3807471|
| Cell.size^5       |    9.7304910|    0.3318172|
| Cell.shape.C      |    9.2941514|    0.3169377|
| Cell.size.Q       |    8.9781030|   -0.3061602|
| Normal.nucleoli6  |    8.1225173|    0.2769841|
| Epith.c.size^8    |    7.9630578|    0.2715464|
| Epith.c.size.L    |    7.4422032|    0.2537849|
| Bare.nuclei5      |    6.6248268|    0.2259117|
| Cell.shape^8      |    6.5931106|    0.2248301|
| Epith.c.size^7    |    5.8722778|   -0.2002492|
| Cl.thickness^5    |    5.7674433|    0.1966743|
| Bl.cromatin4      |    5.7288930|    0.1953597|
| Cl.thickness.Q    |    4.8120311|    0.1640940|
| Epith.c.size^5    |    1.9890777|   -0.0678291|
| Bare.nuclei8      |    1.6499963|    0.0562661|
| Normal.nucleoli7  |    0.0179295|   -0.0006114|
| (Intercept)       |           NA|    0.6992724|
