######    Applied Statistical Analysis I    ######     
######    Problem Set 1                     ######
######    Shekhar Kedia - 23351315          ######

# Get working directory
getwd()

# Set working directory 
setwd("D:/TCD- ASDS/Applied Statistics 1/GitHub/StatsI_Fall2023/problemSets/PS01")
getwd()

# Clearing global environment and removing objects
rm(list=ls())

# Detaching all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Loading libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)


#####################
# Problem 1
#####################

# Loading the sample observations
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

len_y <- length(y) #Ensuring we have 25 observations by checking the length of object y

mean <- mean(y) #Calculating the mean value

stand_dev <- sd(y) #Calculating the standard deviation

stand_error <- stand_dev/sqrt(len_y) #Calculating the standard error

upper_ci <- mean + qt(.95, df = (len_y - 1)) * stand_error
lower_ci <- mean - qt(.95, df = (len_y - 1)) * stand_error

# Hypothesis test
# Null hypo: mean <= 100
# Alternate hypo: mean > 100

t_stat <- (mean-100)/stand_error

t_value <- 2.064 #Using t-distribution to calculate the critical t-value for df = 24 and alpha = 0.05

compare <- t_stat > t_value
compare

t.test


# As the output is False, which indicates, t_stat is less than t_value, we accept the null

t.test(y, alternative = c("greater"), mu = 100)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
