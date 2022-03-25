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

data <- read.csv("Q2_b.csv")

summary(data)

sumofspeed <- sum(data$SP)
sumofspeed / 50

sumofspeed <- sum(data$WT)
sumofspeed / 50

sort(data$SP)
sort(data$WT)

median(data$SP)
median(data$WT)

table(data$SP)
table(data$WT)

modeforcars <- table(as.vector(data$SP))
names(modeforcars)[modeforcars == max(modeforcars)]

modeforcars <- table(as.vector(data$WT))
names(modeforcars)[modeforcars == max(modeforcars)]

myvector_outlier<- c(-100, 2, 4, 6, 8, 10, 12, 14, 16)
mean(myvector_outlier[2:9])

myvector_trim<- c(-15, 2, 3, 4, 5, 6, 7, 8, 9, 12)
mean(myvector_trim)

mean(myvector_trim, trim=.1)

min(data$SP)
min(data$WT)

max(data$SP)
max(data$WT)

range(data$SP)
range(data$WT)

IQR(data$SP)
IQR(data$WT)

quantile(data$SP)
quantile(data$WT)

var(data$SP)
var(data$WT)

sqrt(var(data$SP))
sqrt(var(data$WT))

library(psych)
skew(data$SP)

#this is the clculation of Skewness
library(psych)
skew(data$WT)

#this is the clculation of Kurtosis
kurtosi(data$SP)
kurtosi(data$WT)

plot(density(data$SP))
plot(density(data$WT))

summary(data$SP)
summary(data$WT)

summary(data)

describe(data)

describeBy(data, group=data$SP)
describeBy(data, group=data$WT)

cor(data$SP, data$WT)

cor(data)

cor(data$SP, data$WT,  method="spearman")


barplot(skew(data$SP))
barplot(data$SP, data$WT)
barplot((data$WT))
barplot(kurtosi(data$SP))
barplot(data$SP)

boxplot((data$SP))
boxplot(data$WT)

plot(data$X, data$SP)
plot(data$WT, data$X)

plot.ecdf(data$X, data$SP)
plot.ecdf(data$WT, data$X)

#To draw a barplot of sp
#Horizontal
barplot(data$SP,xlab = "Sp", col = "cyan", horiz = TRUE)
#Vertical
barplot(data$SP, ylab = "Sp", col = "cyan", horiz = FALSE)

#To draw a barplot of wt
#Horizontal
barplot(data$WT,xlab = "wt", col = "cyan", horiz = TRUE)
#Vertical
barplot(data$WT, ylab = "wt", col = "cyan", horiz = FALSE)

#To find histogram for Sp
hist(data$SP,xlab = "SP", main = "Histogram for SP", col = "yellow")

#To find histogram for wt
hist(data$WT,xlab = "WT", main = "Histogram for WT", col = "yellow")

scatterplot3d::scatterplot3d(data$WT, data$SP, data$X)

scatter.smooth(data$X, data$SP)
scatter.smooth(data$WT, data$X)
