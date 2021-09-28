# The Data Science Codex

A collection of code and resources to serve as a starting point for data science projects. For more explanation and material on R visit [my blog](https://jessecambon.github.io/). 

<span>
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Visualization_Cookbook.md#lollipop"><img src="https://github.com/jessecambon/Data-Science-Cookbook/blob/master/rmd_images/Visualization_Cookbook/lollipop-1.png" height="150px"/></a>
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Visualization_Cookbook.md#bubbleplot"><img src="https://github.com/jessecambon/Data-Science-Cookbook/blob/master/rmd_images/Visualization_Cookbook/bubbleplot-1.png" height="150px"/></a>
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Geospatial_Analysis.md"><img src="https://github.com/jessecambon/Data-Science-Codex/blob/master/rmd_images/Geospatial_Analysis/unnamed-chunk-2-1.png" height="150px"/></a> 
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Visualization_Cookbook.md#ridgeplot"><img src="https://raw.githubusercontent.com/jessecambon/Data-Science-Codex/master/rmd_images/Visualization_Cookbook/ridge-1.png" height="150px"/></a> 
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Titanic.md#logistic-regression-model"><img src="https://github.com/jessecambon/Data-Science-Codex/blob/master/rmd_images/Titanic/logistic-regression-2.png" height="150px"/></a> 
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Titanic.md#logistic-regression-model"><img src="https://github.com/jessecambon/Data-Science-Cookbook/blob/master/rmd_images/Titanic/logistic-regression-1.png" height="150px"/></a> 
</span>

## Notes 
* [Resources](Resources.md) - Websites and references that I find helpful for data science projects
* [Developing With R](R-Development.md) - Notes on R package development
* [How to Git](Git-HowTo.md) - version control with git
* [How to Anaconda](Anaconda-HowTo.md) - managing environments with Anaconda

## Data Visualization
* [Visualization Cookbook](R/Visualization_Cookbook.md) (R) - A wide variety of data visualizations demonstrated.
* [Geospatial Data Analysis](R/Geospatial_Analysis.md) (R) - Making maps with R.

## Statistical Modeling and Machine Learning
* [Modeling Fundamentals](R/Titanic.md) (R) - A primer on logistic and linear regression modeling with the classic Titanic dataset.
* [Survival Analysis](R/Survival.md) (R) - Survival analysis methods such as cox proportion hazard models and Kaplan-Meier curves.
* [Modeling Workflows](R/Modeling_Workflow.md) (R) - Streamlined Tidyverse modeling workflows with the gapminder dataset.
* [Multilevel Models](R/Multilevel-Models.md) (R) - Multi-level aka. mixed effects models
* [Time Series Modeling](R/Time_Series_Modeling.md) (R) - Experimenting with time series modeling (tsibble, forecast libraries, prophet, etc.)
* [Ordinal Regression](R/Ordinal_Regression.md) (R) - Experimenting with ordinal (ranked categorical outcome) regression
* [Presenting Regression Models](R/Regression-Model-Tidying.md) (R) - Code for cleaning the outputs of regression models for presentations.
* [Sklearn Modeling Workflows](Python/Sklearn-Workflow.ipynb) (Python) - Modeling workflows with sklearn (cross-validation, randomized search for optimizing hyperparameters, lift curves).
* [Sklearn - Skopt Workflow](Python/sklearn_skopt_pipeline.ipynb) (Python) - Modeling workflow with sklearn and scikit-optimize (bayesian hyperparameter optimization.
* [Machine Learning with Caret](R/Caret.md) (R) - Using the Caret library for machine learning.
* [Parsnip](R/Parsnip.md) (R) - fitting models with the parsnip package (from tidymodels)

## Bayesian Models
* [Bayesian Basics](R/Bayesian_Basics.md) (R) - exploring a simple Bayesian multilevel model
* [Bayesian Modeling](R/Bayesian_Modeling.md) (R) - Experimenting with Bayesian models using rstanarm
* [Comparing Bayesian Packages](R/Comparing_Bayesian_Packages.md) (R) - Comparing rstanarm, brms, and rstan.

##  Clustering 
* [k-means clustering](R/Clustering.md) (R) - Using the k-means algorithm to cluster data.
* [Clustering](Python/Clustering.ipynb) (Python) - Agglomerative (Hierarchical) clustering, k-means clustering, and Gaussian mixture models

## Stats Analysis
* [Power Analysis](R/Power_Analysis.md) (R) - Statistical power analysis
* [Distribution Sampling and Hypothesis Testing](R/Distribution_Sampling.md) (R)
* [Hypothesis Testing](R/hypothesis_testing.md) (R)

## NLP 
* [Document Embeddings](Python/state_of_union_embeddings.ipynb) (Python) - Using word embeddings to compare the similarity of State of the Union addresses.
* [State of the Union Analysis](Python/state_of_union_v2.ipynb) (Python) - An exploration of state of the union addresses with topic modeling and sentiment analysis. 
* [Sentiment Analysis](R/Sentiment_Analysis.md) (R) - Exploring sentiment analysis in R.
* [LSTM Demo](Python/LSTM-Demo.ipynb) (Python) - An LSTM network for predicting if a company review from glassdoor is positive

## Miscellaneous
* [R-Quickstart](R/R-Quickstart.md) (R) - Minimal data analysis and visualization workflows. See [the blog post "Data Science Essentials"](https://jessecambon.github.io/2020/01/12/data-science-essentials.html) for more details and explanation.
* [Creating Formatted Spreadsheets](R/Create_Formatted_Spreadsheet.md) (R) - How to create a custom formatted spreadsheet report with the openxlsx R package.
* [Using Python and R Together](Python/R-Python-Hybrid.ipynb) - How to use python and R code together in the same Jupyter notebook with the rpy2 python package.
* [R Quotation](R/R_Quotation.md) (R) - If you want to do certain things such as pass variable names as arguments to a function in R, you have to use quotation methods like `quo()` and `enquo()`. This notebook demonstrates how to do this. See [my blog post on Tidy Evaluation](https://jessecambon.github.io/2019/12/08/practical-tidy-evaluation.html) for more details and explanation.
* [SQL Databases](Python/SQL_Databases.ipynb) (Python) - Code for creating and manipulating a SQL database.
