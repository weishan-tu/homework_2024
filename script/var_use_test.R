 ## ---------------------------
 ##
 ## Script name: 
 ##
 ## Purpose of script:
 ##
 ## Author: Weishan Tu 
 ##
 ## Date Created: 
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
 
var.name <- 123
print(var.name)
cat(var.name)

cat("var.name is", var.name)

a <- c(1,3,5,7)
b <- c(2,4,6,8)
a%%b

pak::pak("ade4")
