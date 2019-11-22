# The Data Science Codex

A collection of code and resources to serve as a starting point for data science projects. 

<span>
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Visualization_Cookbook.md#lollipop"><img src="https://github.com/jessecambon/Data-Science-Cookbook/blob/master/rmd_images/Visualization_Cookbook/lollipop-1.png" height="200px"/></a>
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Visualization_Cookbook.md#bubbleplot"><img src="https://github.com/jessecambon/Data-Science-Cookbook/blob/master/rmd_images/Visualization_Cookbook/bubbleplot-1.png" height="200px"/></a>
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Geospatial_Analysis.md"><img src="https://github.com/jessecambon/Data-Science-Codex/blob/master/R/Geospatial_Analysis_files/figure-markdown_github/unnamed-chunk-2-1.png" height="200px"/></a> 
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Visualization_Cookbook/Visualization_Cookbook.md#ridgeplot"><img src="https://raw.githubusercontent.com/jessecambon/Data-Science-Codex/master/rmd_images/Visualization_Cookbook/ridge-1.png" height="200px"/></a> 
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Titanic.md#logistic-regression-model"><img src="https://github.com/jessecambon/Data-Science-Codex/blob/master/R/Titanic_files/figure-markdown_github/logistic-regression-2.png" height="200px"/></a> 
<a href = "https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Titanic.md#logistic-regression-model"><img src="https://github.com/jessecambon/Data-Science-Cookbook/blob/master/R/Titanic_files/figure-markdown_github/logistic-regression-1.png" height="200px"/></a> 
</span>


The purpose of this repository is to provide code and resources that can be quickly adapted and used for data science projects. An emphasis is placed on having clear understandable code that can be easily replicated. I make use of inbuilt datasets as much as possible so that there is no need to download external datasets.


## General 
* [Resources](Resources.md) - Websites and references that I find helpful for data science projects.
* [Creating Formatted Spreadsheets](R/Create_Formatted_Spreadsheet.md) (R) - How to create a custom formatted spreadsheet report with the openxlsx R package.
* [Using Python and R Together](R/R-Python-Hybrid.ipynb) - How to use python and R code together in the same Jupyter notebook with the rpy2 python package.
* [R Quotation](R/R_Quotation.md) (R) - If you want to do certain things such as pass variable names as arguments to a function in R, you have to use quotation methods like quo() and enquo(). This notebook demonstrates how to do this.
* [Tidy Pandas](Python/Tidy_Pandas.ipynb) (Python) - Functions to incorporate some of the functionality from dplyr (R/Tidyverse) into Pandas. Includes a function for counting values or combinations of values in a pandas dataframe that doesn't exclude missing values.
* [SQL Databases](source/SQL_Databases.ipynb) (Python) - Code for creating and manipulating a SQL database.

## Data Visualization
* [Visualization Cookbook](R/Visualization_Cookbook.md) (R) - A wide variety of data visualizations demonstrated.
* [Geospatial Data Analysis](R/Geospatial_Analysis.md) (R) - A work-in-progress collection of maps. 

## Modeling and Machine Learning 
* [Modeling Fundamentals](R/Titanic.md) (R) - A primer on logistic and linear regression modeling with the classic Titanic dataset.
* [Modeling Workflows](R/Modeling_Workflow.md) (R) - Streamlined Tidyverse modeling workflows with the gapminder dataset.
* [Sklearn Modeling Workflows](Python/Sklearn-Workflow.ipynb) (Python) - Modeling workflows with sklearn (cross-validation, randomized search for optimizing hyperparameters, lift curves)
* [Time Series Modeling](R/Time_Series_Modeling.md) (R) - Experimenting with time series modeling (tsibble, forecast libraries, etc.)
* [Clustering](R/Clustering.md) (R) - Using the k-means alogrithm to cluster data.
* [Machine Learning with Caret](R/Caret.md) (R) - Using the Caret library to do machine learning.
* [Predicting Fuel Economy](R/Vehicles.md) (R) - Predicting the fuel economy of cars.
* [Ordinal Regression](R/Ordinal_Regression.md) (R) - Experimenting with ordinal (ranked categorical outcome) regression
* [Presenting Regression Models](R/Regression_Model_Tidying.md) (R) - Code for cleaning the outputs of regression models

## NLP 
* [Document Embeddings](Python/state_of_union_embeddings.ipynb) (Python) - Using document embeddings to find similar state of the union addresses.
* [Hello NLP](Python/Hello_NLP.ipynb) (Python) - Testing out some NLP techniques on Wall Street Journal sample data
* [State of the Union Analysis](Python/state_of_union_v2.ipynb) (Python) - An exploration of state of the union addresses with NLP
* [LSTM Demo](Python/LSTM-Demo.ipynb) (Python) - An LSTM network for predicting if a company review from glassdoor is positive
