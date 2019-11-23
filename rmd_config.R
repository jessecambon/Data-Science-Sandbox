# This file contains knitr settings for Rmarkdown files
# run this file via source() in all RMarkdown files
library(knitr)
library(stringr)
library(here)
# get name of file during knitting and strip file extension
rmd_filename <- str_remove(knitr::current_input(),"\\.Rmd")
knitr::opts_chunk$set(fig.path = str_c(here::here("rmd_images",rmd_filename),'/'),echo=TRUE) 