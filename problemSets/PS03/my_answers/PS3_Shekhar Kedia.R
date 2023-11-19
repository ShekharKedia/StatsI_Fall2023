######    Applied Statistical Analysis I    ######     
######    Problem Set 3                     ######
######    Shekhar Kedia - 23351315          ######

## Preparing the environment ##

# Getting working directory
getwd()

# Setting working directory 
setwd("D:/TCD- ASDS/Applied Statistics 1/GitHub/StatsI_Fall2023/problemSets/PS03/my_answers")
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

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

## Problem 1.1 ##
model_vote_diff <- lm(voteshare ~ difflog, data = inc.sub)
summary(model_vote_diff) #Exploring the output of the model

#Reporting Regression results
stargazer(model_vote_diff,
          title = "Model 1 Regression Results")

## Problem 1.2 ##
pdf("plot1.pdf", width=8)
plot(inc.sub$difflog, 
     inc.sub$voteshare,
     xlab = "Log diff. in campaign spending b/w incumbent & challenger", 
     ylab = "Incumbent vote share", 
     main = "Scatterplot and regression line") #Scatter plot
abline(model_vote_diff) #Adding regression line
dev.off()

## Problem 1.3 ##
residuals_vote_diff <- model_vote_diff$residuals
print(residuals_vote_diff)

## Problem 1.4 ##
ŷ = 0.579 + 0.041*difflog #Predicted equation

## Problem 2.1 ##
model_pres_diff <- lm(presvote ~ difflog, data = inc.sub)
summary(model_pres_diff) #Exploring the output of the model

#Reporting Regression results
stargazer(model_pres_diff,
          title = "Model 2 Regression Results")

## Problem 2.2 ##
pdf("plot2.pdf", width=8)
plot(inc.sub$difflog, 
     inc.sub$presvote,
     xlab = "Log diff. in campaign spending b/w incumbent & challenger", 
     ylab = "Presidential candidate vote share", 
     main = "Scatterplot and regression line") #Scatter plot
abline(model_pres_diff) #Adding regression line
dev.off()

## Problem 2.3 ##
residuals_pres_diff <- model_pres_diff$residuals
print(residuals_pres_diff)

## Problem 2.4 ##
ŷ = 0.507 + 0.023*difflog #Predicted equation

## Problem 3.1 ##
model_vote_pres <- lm(voteshare ~ presvote, data = inc.sub)
summary(model_vote_pres) #Exploring the output of the model

#Reporting Regression results
stargazer(model_vote_pres,
          title = "Model 3 Regression Results")

## Problem 3.2 ##
pdf("plot3.pdf", width=8)
plot(inc.sub$presvote, 
     inc.sub$voteshare,
     xlab = "Presidential candidate vote share", 
     ylab = "Incumbent vote share", 
     main = "Scatterplot and regression line") #Scatter plot
abline(model_vote_pres) #Adding regression line
dev.off()

## Problem 3.3 ##
ŷ = 0.441 + 0.388*presvote #Predicted equation

## Problem 4.1 ##
model_res <- lm(residuals_vote_diff ~ residuals_pres_diff)
summary(model_res)

#Reporting Regression results
stargazer(model_res,
          title = "Model 4 Regression Results")

## Problem 4.2 ##
pdf("plot4.pdf", width=8)
plot(residuals_pres_diff,
     residuals_vote_diff,
     xlab = "Unexplained variance in Model 2", 
     ylab = "Unexplained variance in Model 1", 
     main = "Scatterplot and regression line") #Scatter plot
abline(model_res) #Adding regression line
dev.off()

## Problem 4.3 ##
ŷ = -5.934e-18 + 2.569e-01*residuals_pres_diff #Predicted equation

## Problem 5.1 ##
model_vote_diff_pres <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model_vote_diff_pres)

#Reporting Regression results
stargazer(model_vote_diff_pres,
          title = "Model 5 Regression Results")

## Problem 5.2 ##
ŷ = 0.448 + 0.0355*difflog + 0.256*presvote #Predicted equation
