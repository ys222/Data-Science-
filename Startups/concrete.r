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
library(ggvis) #Data visulization
library(psych) #Scatterplot matrix
library(knitr)

# Read the data
concrete <- read.csv(file.choose())
View(concrete)
class(concrete)

knitr::kable(head(concrete), caption = "Partial Table Preview")

str(concrete)

normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x) ))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))

#training set
concrete_train <- concrete_norm[1:773, ]

#test set
concrete_test <- concrete_norm[774:1030, ]

#Build a neural network with one hidden layer 
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age , data = concrete_train, hidden = 1)

plot(concrete_model)

#building the predictor, exclude the target variable column
model_results <- compute(concrete_model, concrete_test[1:8])

#store the net.results column 
predicted_strength <- model_results$net.result

cor(predicted_strength, concrete_test$strength)

#building the new model
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = 5 )

#nuilding the new predictor
model_results2 <- compute(concrete_model2, concrete_test[1:8])

#storing the results
predicted_strength2 <- model_results2$net.result

cor(predicted_strength2, concrete_test$strength)

