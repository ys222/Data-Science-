library(tidyverse) # helpful in Data Cleaning and Manipulation
library(arules) # Mining Association Rules and Frequent Itemsets
library(arulesViz) # Visualizing Association Rules and Frequent Itemsets 
library(gridExtra) #  low-level functions to create graphical objects 
library(ggthemes) # For cool themes like fivethirtyEight
library(dplyr) # Data Manipulation
library(readxl)# Read Excel Files in R
library(plyr)# Tools for Splitting, Applying and Combining Data
library(ggplot2) # Create graphics and charts
library(knitr) # Dynamic Report generation in R
library(lubridate) # Easier to work with dates and times.
library(kableExtra) # construct complex tables and customize styles
library(RColorBrewer)

data <- read.csv("Q1_a.csv")

sumofspeed <- sum(cars$speed)
sumofspeed / 50

sum(cars$speed) / length(cars$speed)

sort(cars$speed)

median(cars$speed)

table(cars$speed)

modeforcars <- table(as.vector(cars$speed))
names(modeforcars)[modeforcars == max(modeforcars)]

myvector_outlier<- c(-100, 2, 4, 6, 8, 10, 12, 14, 16)
mean(myvector_outlier[2:9])

myvector_trim<- c(-15, 2, 3, 4, 5, 6, 7, 8, 9, 12)
mean(myvector_trim)

mean(myvector_trim, trim=.1)

min(cars$speed)

max(cars$speed)

range(cars$speed)

IQR(cars$speed)

quantile(cars$speed)

quantile(cars$speed, probs=c(.25, .75))

var(cars$speed)

sqrt(var(cars$speed))

sd(cars$speed)
# this is the calculation of Skewness
library(psych)
skew(cars$speed)

#this is the clculation of Kurtosis
kurtosi(cars$speed)

plot(density(cars$speed))
plot(density(cars$dist))

summary(cars$speed)

summary(cars)

describe(cars)

describeBy(cars, group=cars$speed)

cor(cars$speed, cars$dist)

cor(cars)

cor(cars$speed,cars$dist, method="spearman")

barplot(skew(cars$speed))
barplot((cars$speed))
barplot(kurtosi(cars$speed))
barplot(cars$dist)

boxplot((cars$speed))
boxplot(cars$dist)

plot(cars$speed, cars$dist)
plot(cars$dist, cars$speed)

plot.ecdf(cars$speed, cars$dist)
plot.ecdf(cars$dist, cars$speed)

#To draw a barplot of speed
#Horizontal
barplot(cars$speed,xlab = "Speed", col = "cyan", horiz = TRUE)
#Vertical
barplot(cars$speed, ylab = "Speed", col = "cyan", horiz = FALSE)

#To draw a barplot of Dist
#Horizontal
barplot(cars$dist,xlab = "Dist", col = "cyan", horiz = TRUE)
#Vertical
barplot(cars$dist, ylab = "Dist", col = "cyan", horiz = FALSE)

#To find histogram for Speed
hist(cars$speed,xlab = "Speed", main = "Histogram for SPEED", col = "yellow")

#To find histogram for Dist
hist(cars$dist,xlab = "dist", main = "Histogram for DIST", col = "yellow")

scatterplot3d::scatterplot3d(cars$speed ,cars$dist ,cars$Index)

scatter.smooth(cars$speed, cars$Index)
scatter.smooth(cars$dist, cars$Index)

