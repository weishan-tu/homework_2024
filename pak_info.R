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
 library(packagefinder)
 fp("tidyverse")
 ##install the package
 pak::pak("tidyverse")
 # or
 # install.packages("tidyverse")
 
 ##load up the packages tidyverse
 library(tidyverse)
 ##get help
 help(tidyverse)
 help(dplyr)
 help(readr)
 help(forcats)
 help(ggplot2)
 help(tidyr)
 
 ## use tidyverse---------------------------
 ## load up the data frame
 starwars <- starwars 
 
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
