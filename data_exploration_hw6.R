 ## ---------------------------
 ##
 ## Script name: data_exploration.R
 ##
 ## Purpose of script: 
 ## 1. write a short code to remove the sites with missing data of the Doubs dataset,
 ## and detect whether environmental factors are collinearity.
 ## 2. Analysis on the relationships between fishes and environment factors and 
 ## visualize such relationships.
 ##
 ## Author: Weishan Tu
 ##
 ## Date Created: 2024-04-14
 ##
 ## Copyright (c) Timothy Farewell, 2024
 ## Email: weishan@mail.ustc.edu.cn
 ##
 ## ---------------------------
 
 cat("\014") #clears rhe console
 rm(list=ls()) #remove all variales
 
 # install.packages("ade4")
 ##Load package 
 library(ade4)
 library(vegan)
 library(tidyverse)
 
 ##Load data
 data(doubs)
 doubs.spe <- doubs$species #Species list 27 species 
 doubs.env <- doubs$env #11 abiotic variables
 str(doubs.env)
 doubs.xy <- doubs$xy #Spatial coordinates
 doubs.fish <- doubs$fish # Community data
 
 ##
 ##Remove the sites with missing data
 str(doubs.fish)  # structure of objects in dataset
 rowSums(doubs.fish)  # row 8 no species
 colSums(doubs.fish)
 anyNA(doubs.fish) # No NA
 summary(doubs.fish)  # summary statistics for all objects (min, mean, max, etc.)
 anyNA(doubs.env) # No NA
 
 doubs.fish <- doubs.fish[-8, ]  # Site number 8 contains no species, so we remove row 8 (site 8) 
 doubs.env <- doubs.env[-8, ]  # Remove corresponding abiotic data for site 8 (because removed from fish data). 
 doubs.xy <- doubs.xy[-8, ] # Remove xy data for site 8 (because removed from fish data). 
 
 
 ##########################
 ##1 Coinertia Analysis
 ##########################
 # Calculate the correlation matrix
 corr_matrix <- cor(doubs.env)
 
 # Calculate the correlation matrix p-value
 library(Hmisc)#load package
 corr_matrix2 <- rcorr(as.matrix(doubs.env))
 corr_matrix2
 
 # Visualization of a Correlation Matrix
 library(corrplot)#load package
 corrplot(corr_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

 #dts is highly correlated with alt, slo, flo, har,nit...
 # Draw scatter plots
 # pak::pak("PerformanceAnalytics")
 library(PerformanceAnalytics)#load package
 chart.Correlation(doubs.env, histogram=TRUE, pch=19)
 
 ##########################
 ##2 The relationships between fishes and environment factors
 ##########################
 # Check unconstrained and constrained
 vare.dca <- decorana(doubs.env)
 # Axis lengths=1.75 <3 
 # Use constrained ordination 
 # Use RDA analysis
 
 # Step 1: Transform and standardize the data.
 # Hellinger transform the community data
 doubs.fish.hel <- decostand(doubs.fish, method = "hellinger")
 #Scale and center env variables
 doubs.env.z <- decostand(doubs.env, method = "standardize")
 # Variables are now centered around a mean of 0
 round(apply(doubs.env.z, 2, mean), 1)
 # and scaled to have a standard deviation of 1
 apply(doubs.env.z, 2, sd) 

 # use standardized environmental data
 # remove 'dfs', which was correlated with many other variables
 doubs.env.z <- subset(doubs.env.z, select = -dfs)
 
 # Step 2: Select environmental variables.
 # Initial RDA with ALL of the environmental data
 doubs.fish.rda <- rda(doubs.fish.hel ~ ., data = doubs.env.z)
 
 # Forward selection of environmental variables
 fwd.sel <- ordiR2step(rda(doubs.fish.hel ~ 1, data = doubs.env.z),
                       scope = formula(doubs.fish.rda), direction = "forward", R2scope = TRUE,
                       pstep = 1000, trace = FALSE)
 fwd.sel$call
 # rda(formula = doubs.fish.hel ~ alt + oxy + bdo, data = doubs.env.z)
 
 # Step 3: Run the RDA and check its explanatory power.
 
 # Re-run the RDA with the significant variables
 doubs.fish.rda.signif <- rda(formula = doubs.fish.hel ~ alt + oxy + bdo, data = doubs.env.z)
 
 # Find the adjusted R2 of the model with the retained env
 # variables
 RsquareAdj(doubs.fish.rda.signif)$adj.r.squared
 ##0.5401552
 
 # Step 4: Test model significance.
 anova.cca(doubs.fish.rda.signif, step = 1000)

 # selected environmental variables significantly explain 54.0% (p = 0.001)
 
 # Step 5: Plot the RDA results
 # Scaling 1
 ordiplot(doubs.fish.rda.signif, scaling = 1, main = "doubs RDA - Scaling 1")
 # Scaling 2
 ordiplot(doubs.fish.rda.signif, scaling = 2, main = "doubs RDA - Scaling 2")
 