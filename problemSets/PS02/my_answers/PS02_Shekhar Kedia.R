                      ######    Applied Statistical Analysis I    ######     
                      ######    Problem Set 2                     ######
                      ######    Shekhar Kedia - 23351315          ######

## Preparing the environment ##
                      
# Getting working directory
getwd()

# Setting working directory 
setwd("D:/TCD- ASDS/Applied Statistics 1/GitHub/StatsI_Fall2023/problemSets/PS02/my_answers")
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

lapply(c("ggplot2"),  pkgTest)

                      #####################
                      ## Problem 1
                      #####################

## Problem 1.1 ##

# Creating the table for use
tab <- matrix(c(14, 6, 7, 7, 7, 1), ncol=3, byrow=TRUE)
colnames(tab) <- c('Not stopped','Bribe requested','Stopped/given warning')
rownames(tab) <- c('Upper class','Lower class')
tab <- as.table(tab)
tab #Displaying the crosstab with observed frequencies

# Creating a function to calculate the expected frequencies if the variables were independent
f_exp <- function (input_table, row, col) {
  total_sum <- sum(input_table)
  col_sum <- colSums(input_table)
  row_sum <- rowSums(input_table)
  return(col_sum[col:col]*row_sum[row:row]/total_sum)
}

# Creating the expected frequency table for use
f_expected <- c() #Creating an empty vector
for (row in 1:nrow(tab)) {
  for (col in 1:ncol(tab)) {
    temp = f_exp(tab,row,col) #Temporary variable to capture the expected frequency
    f_expected = c(f_expected, temp) #Appending values after end of each iteration
  }
}
tab_exp <- matrix(f_expected, ncol = 3, byrow = TRUE)
tab_exp <- as.table(tab_exp)
tab_exp #Displaying the crosstab with expected frequencies

# Calculating the chisquare statistics by hand using the formula taught in stats class
chisqr <- 0
for (row in 1:nrow(tab)) {
  for (col in 1:ncol(tab)) {
    chisqr = chisqr + ((tab[row,col]-tab_exp[row,col])^2/tab_exp[row,col])
  }
}
chisqr #Displaying the chisquare value calculated by hand

# Using the built-in function to cross verify the findings
chisq.test(tab) #We get the same chisquare value = 3.7912

## Problem 1.2 ##

# Using the built-in function to calculate the p-value
p_val <- pchisq(chisqr, df= ((nrow(tab)-1)*(ncol(tab)-1)), lower.tail = FALSE)
p_val #We get the p-value = 0.1502

## Problem 1.3 ##

# Creating a function to calculate the standardized residuals
# Note that only part of the value is calculated using the function
res_cal <- function (input_table, row, col) {
  total_sum <- sum(input_table)
  col_sum <- colSums(input_table)
  row_sum <- rowSums(input_table)
  return((1-(row_sum[row:row]/total_sum))*(1-(col_sum[col:col]/total_sum)))
}

# Calculating the standardized residuals
residual_exp <- c()
for (row in 1:nrow(tab)) {
  for (col in 1:ncol(tab)) {
    temp_fe = f_exp(tab,row,col)
    temp_r = (tab[row,col]-temp_fe)/(sqrt(temp_fe*res_cal(tab, row, col)))
    residual_exp = c(residual_exp, temp_r)
  }
}
tab_res <- matrix(residual_exp, ncol = 3, byrow = TRUE)
tab_res <- as.table(tab_res)
tab_res #Displaying the standardized residuals

# Using the built-in function to cross verify the findings
(chisq.test(tab))$stdres


                      #####################
                      ## Problem 2
                      #####################

# Reading the csv data and importing to R environment
ind_eco <- read.table("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header = TRUE, sep = ",")

View(ind_eco) #Viewing the data to visually understand the structure and spread

summary(ind_eco) #Inspecting the data through summary

## Problem 2.2 ##

# Creating scatterplot & calculating correlation value to show relationship between the two variables
cor_Y_X <- round(cor(ind_eco$water, ind_eco$reserved),2) #Shows the default pearson correlation value rounded upto 2 decimals

pdf("plot_Y_X.pdf") #Creates a pdf file where we can then input/write the scatter plot
plot(ind_eco$reserved, ind_eco$water,
     xlab = "reservation policy",
     ylab = "number of new/improved water facilities",
     main = paste("reservation policy vs no. of new/improved water facilities
     Correlation = ", cor_Y_X)) #The scatter plot also has the correlation value mentioned in the title
dev.off()

# Bivariate regression function
reg <- lm(water~reserved, data = ind_eco)

summary(reg) #Displaying the summarized results of the linear regression model