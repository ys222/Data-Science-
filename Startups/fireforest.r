library(neuralnet)
library(caret)
library(grDevices)
library(leaps)
library(relaimpo)
library(corrplot)
library(car)
library(DAAG)
# Alters the way numbers are outputted
options(scipen = 999,digits = 3)
library(nnet)
library(NeuralNetTools)
library(plyr)

# Read the data
fireforest <- read.csv(file.choose())
View(fireforest)
class(fireforest)

# Number of data cases and variables
dim(fireforest)

# Check names of variables - match database infomation.
colnames(fireforest)

# Check to see if any data missing - we're OK here so can proceed.
sum(is.na(fireforest))

# Check to see how many cases have an area of 0
length(which(fireforest$area==0))

# Convert month and day string variables into numeric values
fireforest$month <- as.numeric(as.factor(fireforest$month))
fireforest$day <- as.numeric(as.factor(fireforest$day))

# Density plot - Predictors
# Shows us rain ais right skewed and FFMC is left skewed. 
# We can see normal looking distributions for temp, wind, RH, X, DMC and day.
#pdf(file="predictordesnisty.pdf")
par(mfrow=c(2,6),mar=c(3.90, 4.25, 2.5, 0.5))
for (variables in 1:(dim(fireforest)[2]-1)){
  thisvar = fireforest[,variables]
  d <- density(thisvar)
  plot(d, main = names(fireforest[variables]),xlab="")
  polygon(d, col="cyan", border="blue")
  title("Density plots for all 11 Model Variables", line = -19, outer = TRUE)}

# Rain variable has a heavy 0 distribution with only 1.56% of the data
# This will therefore be removed from the model as there is not enough variance
print(paste("Percentage non-zero rain: ",round(length(which(fireforest$rain>0)) /dim(fireforest)[1]*100,2)))

# Since the FFMC is left-skew, we'll cube it to normalize it
par(mfrow=c(1,2),mar=c(5, 4.25, 5.5, 2))
d <- density(fireforest$FFMC)
plot(d,main="FFMC Density (original)",xlab="FFMC index", col='tomato', lwd=3)
fireforest$FFMC<- (fireforest$FFMC^3)
d <- density(fireforest$FFMC)
plot(d,main="FFMC Density (x^3)",xlab="FFMC index", col='tomato', lwd=3)
fireforest <- fireforest[,-which(colnames(fireforest)== "rain")]

# Density plot - Outcome
# Shows us a extensive right skew in the data
par(mfrow=c(1,2),mar=c(5, 4.25, 5.5, 2))
d <- density(fireforest$area)
plot(d,main="Area Burned Density (original)",xlab="Area Burned (Hec)", col='tomato', lwd=3)
d <- density(log(fireforest$area+1))
plot(d,main="Area Burned Density (log(x+1))",xlab="Area Burned (Hec)", col='tomato', lwd=3)

# Heavy skew indicates log transformation
# Since there are also many 0 counts for area, we'll first add 1 before transforming
fireforest$area <- log(fireforest$area+1)

# Examine correlations between all 12 predictors and the area outcome
par(mfrow=c(1,1))
M <- cor(fireforest)
corrplot(M, method="color", outline = TRUE,type="lower",order = "hclust",
         tl.col="black", tl.srt=45, diag=FALSE,tl.cex = 1,mar=c(0,0,3,0),
         title="Correlation Matrix between Predictor and Outcome variables")

assumptionsmodel <- lm(area ~ ., data=fireforest)
lmtest::bptest(assumptionsmodel)
par(mfrow=c(2,2))
plot(assumptionsmodel)

assumptionsmodel_all <- lm(area ~ ., data=fireforest)
assumptionsmodel_0 <- lm(area ~ .,data=fireforest[which(fireforest$area>0),])
# Remove all cases with an area burned of 0
fireforest <- fireforest[which(fireforest$area>0),]
# Plots both with and without 0 residuals
par(mfrow=c(1,2))
hist(assumptionsmodel_all$residuals, main = "Data with 0 area burned", xlab = 'Residuals')
abline(v=mean(assumptionsmodel_all$residuals), col='red', lwd=2)
hist(assumptionsmodel_0$residuals,main = "Data without 0 area burned", xlab = 'Residuals')
abline(v=mean(assumptionsmodel_0$residuals), col='red', lwd=2)

model1 <- lm(area ~ ., data=fireforest)
outcome1 <- summary(model1)
(round(outcome1$coefficients[ , c(2,4)],3))
# Find which variables are significant
sig.pred <- row.names(outcome1$coefficients)[which(outcome1$coefficients[ ,4] <= 0.05)]
rsquare <- round(summary(model1)$r.squared,3)
print(paste("The R2 value is ",rsquare))


# Create interactive terms for Fire index
fireforest$FFMC.DMC <- fireforest$FFMC*fireforest$DMC
fireforest$FFMC.DC <-fireforest$FFMC*fireforest$DC
fireforest$FFMC.ISI <-fireforest$FFMC*fireforest$ISI
fireforest$DMC.DC<-fireforest$DMC*fireforest$DC
fireforest$DMC.ISI<-fireforest$DMC*fireforest$ISI
fireforest$DC.ISI<-fireforest$DC*fireforest$ISI

# Create interactive terms for Weather
fireforest$wind.temp<-(fireforest$wind)*(fireforest$temp)
fireforest$temp.RH<-(fireforest$temp)*(fireforest$RH)
fireforest$wind.RH<-(fireforest$wind)*(fireforest$RH)



# Model with original and interactive terms
model2 <- lm(area~.,data=fireforest)
outcome2 <- summary(model2)
# Find which variables are significant
sig.pred <- row.names(outcome2$coefficients)[which(outcome2$coefficients[ ,4] <= 0.05)]
rsquare <- round(summary(model2)$r.squared,3)
print(paste("The R2 value is ",rsquare))
# Visualise the outliers and influence points in the model
influencePlot(model2,id.n=5)

# Remove the 2 outliers from the data
fireforest2 <- fireforest[-which(row.names(fireforest) %in% c(200,470)),]

# Run full model again with outliers removed
model3 <- lm(area~.,data=fireforest2)
outcome3 <- summary(model3)
# Find which variables are significant
sig.pred <- row.names(outcome3$coefficients)[which(outcome3$coefficients[ ,4] <= 0.05)]
rsquare <- round(summary(model3)$r.squared,3)
print(paste("The R2 value is ",rsquare))

