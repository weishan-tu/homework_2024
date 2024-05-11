 ## ---------------------------
 ##
 ## Script name: Homework-202403 data_manipul
 ##
 ## Purpose of script:Using a data frame as an example, 
 ## write a short code to illustrate some functions or 
 ## packages for data processing.
 ##
 ## Author: Weishan Tu 
 ##
 ## Date Created: 2024-03-22
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
 
 setwd("~/homework_2024/")        # Tim's working directory

 ## ---------------------------
 
 ## load up the packages we will need:  (uncomment as required)
 library(tidyverse)
 # source("functions/packages.R")       # loads up all the packages we need
 ## ---------------------------
 ## use tidyverse---------------------------
 ## import data
 dat <- read.csv("data/starwars_data.csv",header = TRUE) 

 ## save data
 write_csv(dat,"data/starwars_data.csv")
 ## or
 write.csv(dat,"data/starwars_data.csv")
 
 ## 2 inspect data structure
 str(dat)
 # class(dat)
 View(dat)

 ## 3 check whether a column or row has missing data
 # find location of missing values
 which(is.na(dat$height))
 # count total missing values
 sum(is.na(dat$height))
 ## 4 extract values from a column or select/add a column 
 height <- dat$height ## extract value by height 
 dat[ , c("height")]      ## Subset by column name  
 dat %>% 
   mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>% ## add a column
   select(name:mass, bmi) ## select a column
 ## 5 transform a wider table to a long format
 
 ## 6 visualize the data
 
 ## Perform data manipulation using dplyr
 ## subset a data frame, filter species names is Droid
 starwars %>% 
   filter(species == "Droid")
 
 ## select data frame, select colname names include "color"
 colnames(starwars)
 starwars %>% 
   select(name, ends_with("color"))
 
 ## creates new columns 
 starwars %>% 
   mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
   select(name:mass, bmi)
 
 ## orders the rows of a data frame by the values of selected columns
 starwars %>% 
   arrange(desc(mass))
 
 ## data manipulation, 
 df <- starwars %>%
   group_by(species) %>% # group_by by species
   summarise( # summary statistics
     n = n(), # calculate species number
     mass = mean(mass, na.rm = TRUE)) %>% # calculate mean mass
   filter(n > 1,mass > 50) # filter species number >1 and mean mass > 50
 
 ##plot histograms 
 p <- df %>%
   # plot
   ggplot( aes(x=species, fill=mass))+
   geom_histogram(position = 'identity')+
   theme_bw()
 
 p
 
