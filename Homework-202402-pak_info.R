## ---------------------------
##
## Script name: Homework-202402
##
## Purpose of script: Write a short code to access information about the tidyverse package,
## named “pak_info.R”. Put the code file into your GitHub.
##
## Author: Weishan Tu 
##
## Date Created: 2024-3-13
##
## Copyright (c) Timothy Farewell, 
## Email: weishan@mail.ustc.edu.cn
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

cat("\014") #clears rhe console
rm(list=ls()) #remove all variales

## set working directory

setwd("~/homework_2024/")     # working directory

## ---------------------------

## load up the packages we will need:  (uncomment as required)
##find the package
install.packages("packagefinder", dependencies = TRUE)
library(packagefinder)
fp("tidyverse")
##install the package
pak::pak("tidyverse")
# or
install.packages("tidyverse")

##load up the packages tidyverse
library(tidyverse)
##get help
help(tidyverse)
#provides a detailed guide to the package and its functions
vignette("tidyverse")
browseVignettes(package="tidyverse")
demo(package="tidyverse")
vignette("dplyr")
vignette("ggplot2")

# Searching for Help
apropos("^tidyverse")
ls("package:tidyverse")
help.search("^tidyverse")

##get help
##get more details for function
help(arrange) ##Order rows using column values
help(filter) ##Keep rows that match a condition


