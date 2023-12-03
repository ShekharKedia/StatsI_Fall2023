######    Applied Statistical Analysis I    ######     
######    Problem Set 4                     ######
######    Shekhar Kedia - 23351315          ######

## Preparing the environment ##

# Getting working directory
getwd()

# Setting working directory 
setwd("D:/TCD- ASDS/Applied Statistics 1/GitHub/StatsI_Fall2023/problemSets/PS04/my_answers")
getwd()

# Clearing global environment and removing objects
rm(list=ls())

# Detaching all library packages and loading relevant package(s)
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("ggplot2","stargazer"),  pkgTest)

# Reading in data
install.packages("car")
library(car)
data(Prestige)
help(Prestige)

# 1.a
# Recoding the 'type' variable to create 'professional' variable
Prestige$professional <- ifelse(Prestige$type %in% c("prof"), 1, 0)
# Removing the missing values (same as 'type' variable)
Prestige$professional[is.na(Prestige$type)] <- NA

# 1.b
model_1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)
stargazer(model_1) #Reporting Regression results

# 1.c
prestige = 21.142 + 0.003 * income + 37.781 * professional - 0.002 income * professional

# 2.a
t1 <- 0.042/0.016
pvalue_1 <- 2 * pt(t1, df = (131-3), lower.tail = F) #For two-tail
print(pvalue_1) #Printing the output

# 2.b
t2 <- 0.042/0.013
pvalue_2 <- 2 * pt(t2, df = (131-3), lower.tail = F) #For two-tail
print(pvalue_2) #Printing the output