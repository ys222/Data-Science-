library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr)

# Read the data
Startups <- read.csv(file.choose())
View(Startups)
class(Startups)

Startups$State <- as.numeric(revalue(Startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(Startups)

Startups <- as.data.frame(Startups)
attach(Startups)

# Exploratory data Analysis :

plot(R.D.Spend, Profit)

plot(Administration, Profit)

plot(Marketing.Spend, Profit)

plot(State, Profit)

windows()
# Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(Startups)

summary(Startups)

sum(is.na(Startups))
dim(Startups)

names(Startups)

startup1 <- Startups[,-4] #We exclude the 4th column as it holds the Name of the States

cor(startup1) #we can see that R.D.Spend and Profit are Highly Correlated.

#Standard Deviation
sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(Profit)

#Variance
var(R.D.Spend)
var(Administration)
var(Marketing.Spend)
var(Profit)

#Plots
plot(R.D.Spend,Administration, type = "p")

plot(Marketing.Spend,Profit, main = "Marketing Expense vs Profit")
#Lets Normalize the data
normalise <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

startup_norm <- as.data.frame(lapply(startup1, normalise))

#Lets Split the Dataset into Train and Test Set Randomly
split1 <- sort(sample(nrow(startup_norm),nrow(startup_norm)*0.7)) #Here we are Splitting it in 70:30 ratio
train1 <- startup_norm[split1,]
test1 <- startup_norm[-split1,]

plot(startup_norm, rep = "best")

par(mar = numeric(4), family = 'serif')
plot(startup_norm, alpha = 0.6)

