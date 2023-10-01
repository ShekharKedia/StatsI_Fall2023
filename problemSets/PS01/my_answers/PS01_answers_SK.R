                      ######    Applied Statistical Analysis I    ######     
                      ######    Problem Set 1                     ######
                      ######    Shekhar Kedia - 23351315          ######

## Preparing the environment ##
                      
# Getting working directory
getwd()

# Setting working directory 
setwd("D:/TCD- ASDS/Applied Statistics 1/GitHub/StatsI_Fall2023/problemSets/PS01/my_answers")
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

library(ggplot2)

lapply(c(),  pkgTest)

                      #####################
                      ## Problem 1
                      #####################

# Loading the sample observations
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 
       80, 97, 95, 111, 114, 89, 95, 126, 98)

## Problem 1.1 ##

len_y <- length(y) #Calculating the number of observations in object y
mean_y <- mean(y) #Calculating the mean value of the obs.
stand_dev_y <- sd(y) #Calculating the standard deviation of the obs.
stand_error_y <- stand_dev_y/sqrt(len_y) #Calculating the standard error

t90 <- qt((1-.90)/2, df = (len_y - 1), lower.tail = FALSE) #Calculating the t-value for given confidence coefficient (i.e. 90%)

lower_ci <- mean_y - (t90 * stand_error_y) #Calculating lower bounds
upper_ci <- mean_y + (t90 * stand_error_y) #Calculating upper bounds
confint90 <- round(c(lower_ci, upper_ci),2) #Combining both bounds of confidence interval rounded upto 2 decimal points

print(round(c(lower_ci, mean_y, upper_ci), 2)) #Printing the 90% confidence intervals 

# Using the built-in function to cross verify the findings
t.test(y, conf.level = .90)

## Problem 1.2 ##

t_stat <- (mean_y - 100)/stand_error_y #Calculating the t-statistics

t95 <- qt((1-.95)/2, df = (len_y - 1), lower.tail = FALSE) #Calculating the t-value for given confidence coefficient (i.e. 95%)

compare <- t_stat > t95 #Comparing the observed t-statistics with the critical t-value and storing the results
print(compare)  #Printing the result of the comparison to draw conclusion

# Performing the one-sample t-test using the built-in function to cross verify the findings
t.test(y, alternative = c("greater"), mu = 100)

                      #####################
                      ## Problem 2
                      #####################

# Reading the expenditure data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

View(expenditure) #Viewing the expenditure data to visually understand the structure and spread

## Problem 2.1 ##

# Creating scatterplot & calculating correlation value to show relationship between Y and X1
cor_Y_X1 <- round(cor(expenditure$Y, expenditure$X1),2) #Shows the default pearson correlation value rounded upto 2 decimals

pdf("plot_Y_X1.pdf") #Creates a pdf file where we can then input/write the scatter plot

plot(expenditure$X1, expenditure$Y,
     xlab = "per capita personal income in state",
     ylab = "per capita expenditure on shelters/housing assistance",
     main = paste("per capita personal income vs expenditure on housing assistance.
     Correlation = ", cor_Y_X1)) #The scatter plot also has the correlation value mentioned in the title
dev.off()

# Creating scatterplot & calculating correlation value to show relationship between Y and X2
cor_Y_X2 <- round(cor(expenditure$Y, expenditure$X2),2)

pdf("plot_Y_X2.pdf")

plot(expenditure$X2, expenditure$Y,
     xlab = "No. of residents financially insecured residents per 100k in state",
     ylab = "per capita expenditure on shelters/housing assistance",
     main = paste("No. of fin. insecured residents per 100k vs per capita exp. on housing assistance.
     Correlation = ", cor_Y_X2))
dev.off()

# Creating scatterplot & calculating correlation value to show relationship between Y and X3
cor_Y_X3 <- round(cor(expenditure$Y, expenditure$X3),2)

pdf("plot_Y_X3.pdf")

plot(expenditure$X3, expenditure$Y,
     xlab = "No. of people per thousand residing in urban areas in state",
     ylab = "per capita expenditure on shelters/housing assistance",
     main = paste("No. people per thousand residing in urban areas vs percapita exp. housing assistance.
     Correlation = ", cor_Y_X3))
dev.off()

# Creating scatterplot & calculating correlation value to show relationship between X1 and X2
cor_X1_X2 <- round(cor(expenditure$X1, expenditure$X2),2)

pdf("plot_X1_X2.pdf")

plot(expenditure$X1, expenditure$X2,
     xlab = "per capita personal income in state",
     ylab = "No. of financially insecured residents per 100k in state",
     main = paste("per capita personal inc. vs No. of fin. insecured residents per 100k.
     Correlation = ", cor_X1_X2))
dev.off()

# Creating scatterplot & calculating correlation value to show relationship between X1 and X3
cor_X1_X3 <- round(cor(expenditure$X1, expenditure$X3),2)

pdf("plot_X1_X3.pdf")

plot(expenditure$X1, expenditure$X3,
     xlab = "per capita personal income in state",
     ylab = "No. of people per thousand residing in urban areas in state",
     main = paste("percapita personal inc. vs No. people per thousand residing in urban areas.
     Correlation = ", cor_X1_X3))
dev.off()

# Creating scatterplot & calculating correlation value to show relationship between X2 and X3
cor_X2_X3 <- round(cor(expenditure$X2, expenditure$X3),2)

pdf("plot_X2_X3.pdf")

plot(expenditure$X2, expenditure$X3,
     xlab = "No. of financially insecured residents per 100k in state",
     ylab = "No. of people per thousand residing in urban areas in state",
     main = paste("No. of fin. insecured residents per 100k vs per thousand residing in urban areas.
     Correlation = ", cor_X2_X3))
dev.off()

## Problem 2.2 ##

# Plotting relationship between Y and Region
pdf("plot_Y_Region.pdf")

boxplot(Y~Region,
     xlab = "Regions in state",
     ylab = "per capita expenditure on shelters/housing assistance",
     data = expenditure,
     main = "Regions vs per capita expenditure on housing assistance in state",
     names = c("Northeast", "North Central", "South", "West")
     )
dev.off()

# The following code is used to check the mean per capita expenditure on housing assistance for each region
  # N.B.: The below code is inspired from the module Computer Programming for Social Scientists (TCD-ASDS Term 1)
i <- 1 #Setting the iteration value
while (i <= length(unique(expenditure$Region))) { #Setting iteration limit to length of Region i.e. 4
mean_temp <- mean(expenditure[expenditure$Region == i, ]$Y) #Calculating the mean value of Y for all observations where Region == i
  cat("The mean per capita expenditure on housing assistance for Region ", i)
  cat(" is =", mean_temp)
  cat("\n")
  i <- i + 1 #Incrementing the iteration value by one each time
}

## Problem 2.3 ##

# Plotting relationship between Y and X1
  # Already covered in 2.1 and so, repeating the same here
pdf("plot_Y_X1.pdf")

plot(expenditure$X1, expenditure$Y,
     xlab = "per capita personal income in state",
     ylab = "per capita expenditure on shelters/housing assistance",
     main = paste("per capita personal income vs expenditure on housing assistance.
     Correlation = ", cor_Y_X1))
dev.off()

# Adding Region variable to the plot and adding legends to depict different colours and shapes
pdf("plot_Y_X1_new.pdf")

plot(expenditure$X1, expenditure$Y, col = expenditure$Region, pch = as.numeric(factor(expenditure$Region)),
     xlab = "per capita personal income in state",
     ylab = "per capita expenditure on shelters/housing assistance",
     main = "Region-wise per capita personal inc. vs expenditure on housing assistance")
legend("bottomright",
     legend = c("Northeast", "North Central", "South", "West"),
     col = c("black","red","green","blue"),
     pch = 1:4)

dev.off()
