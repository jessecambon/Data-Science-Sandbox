Machine Learning with Caret
================
Jesse Cambon
22 November, 2019

Demonstrate a machine learning workflow with
    caret

## References

  - <https://topepo.github.io/caret/model-training-and-tuning.html>
  - <https://cran.r-project.org/web/packages/caretEnsemble/vignettes/caretEnsemble-intro.html>

## Setup

``` r
library(mlbench) # machine learning reference datasets
library(tidyverse)
library(broom)
library(caret)
library(skimr)
library(knitr)
library(kableExtra)

data(BreastCancer) # load

# Set seed for reproducibility
set.seed(45)
```

## Build Model

``` r
#skim(BreastCancer)

BC <- BreastCancer %>% as_tibble() %>%
  dplyr::select(-Id) %>%
  # should really use imputation but we'll just drop nas for now
  drop_na() 

# Use k-fold cross-validation
TC <- trainControl(method="cv", number=5)

# Train some models

# Neural Net
nnet <- train(Class ~ . , BC,method='nnet',trControl=TC,verbose=FALSE)

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

![](/Users/jessecambon/Documents/Data-Science-Codex/rmd_images/Caret/results-1.png)<!-- -->

``` r
# Difference in accuracy
bwplot(diff(resamps))
```

![](/Users/jessecambon/Documents/Data-Science-Codex/rmd_images/Caret/results-2.png)<!-- -->

## Glmnet (Elastic Net) Model

``` r
kable(varImportance,format='markdown') %>%
  kable_styling(bootstrap_options = c("striped",'border'))
```

| Variable          |  Importance | Coefficient |
| :---------------- | ----------: | ----------: |
| Cl.thickness.L    | 100.0000000 |   3.1361533 |
| Bare.nuclei9      |  80.1349380 |   2.5131545 |
| Bare.nuclei6      |  72.1692163 |   2.2633373 |
| Bare.nuclei10     |  62.8228881 |   1.9702221 |
| Cell.shape.L      |  60.0936317 |   1.8846284 |
| Marg.adhesion.L   |  59.9667240 |   1.8806484 |
| Cell.size.L       |  54.3790530 |   1.7054105 |
| Normal.nucleoli10 |  51.3425770 |   1.6101819 |
| Normal.nucleoli9  |  48.7621790 |   1.5292567 |
| Bl.cromatin5      |  42.9191401 |   1.3460100 |
| Marg.adhesion^9   |  38.8206640 |   1.2174755 |
| Normal.nucleoli4  |  38.0200861 |   1.1923682 |
| Cell.shape.Q      |  34.0212706 | \-1.0669592 |
| Cl.thickness^8    |  27.9829503 | \-0.8775882 |
| Normal.nucleoli2  |  27.4707392 | \-0.8615245 |
| Epith.c.size^4    |  27.1064505 |   0.8500998 |
| Bare.nuclei4      |  26.4996140 |   0.8310685 |
| Cell.size^8       |  25.6821554 |   0.8054318 |
| Bare.nuclei3      |  24.5833551 |   0.7709717 |
| Bare.nuclei7      |  21.8755488 |   0.6860507 |
| Cell.size.C       |  20.0900403 |   0.6300545 |
| Bare.nuclei5      |  19.8094615 |   0.6212551 |
| Bl.cromatin7      |  18.8161804 |   0.5901043 |
| Cl.thickness.Q    |  17.8206566 |   0.5588831 |
| Epith.c.size.L    |  17.2140778 |   0.5398599 |
| Cell.shape.C      |  15.8214699 |   0.4961855 |
| Bare.nuclei8      |  15.7832696 |   0.4949875 |
| Cell.shape^8      |  15.5873838 |   0.4888443 |
| Normal.nucleoli6  |  15.5750643 |   0.4884579 |
| Epith.c.size^8    |  15.1072819 |   0.4737875 |
| Cell.size^5       |  14.7341024 |   0.4620840 |
| Mitoses10         |  14.6725910 |   0.4601549 |
| Cell.size.Q       |  13.5285932 | \-0.4242774 |
| Cl.thickness^5    |  12.5086592 |   0.3922907 |
| Normal.nucleoli7  |  11.2372547 | \-0.3524175 |
| Bl.cromatin4      |  11.0371135 |   0.3461408 |
| Epith.c.size^5    |  10.1426736 | \-0.3180898 |
| Bl.cromatin8      |   8.5926819 |   0.2694797 |
| Epith.c.size^9    |   8.1088768 |   0.2543068 |
| Normal.nucleoli3  |   6.1834552 |   0.1939226 |
| Cell.size^6       |   6.0301535 | \-0.1891149 |
| Marg.adhesion.C   |   5.6067864 |   0.1758374 |
| Marg.adhesion^8   |   5.0073879 | \-0.1570394 |
| Epith.c.size^7    |   4.8165935 | \-0.1510558 |
| Bl.cromatin10     |   3.7579941 |   0.1178565 |
| Marg.adhesion^4   |   0.3997860 | \-0.0125379 |
| Cell.shape^5      |   0.2113266 | \-0.0066275 |
| Cl.thickness.C    |   0.1668182 |   0.0052317 |
| (Intercept)       |          NA |   0.5035466 |
