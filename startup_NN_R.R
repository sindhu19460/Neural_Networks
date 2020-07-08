library(neuralnet)
library(nnet)
library(NeuralNetTools)
library(plyr)

#READ THE DATA

Startups <- read.csv(file.choose())
View(Startups)
class(Startups)

Startups$State <- as.numeric(revalue(Startups$State,
                                     c("New York"="0","California"="1",
                                      "Florida"="2" )))
str(Startups)


Startups <- as.data.frame(Startups)
attach(Startups)
#Startups

#EXPLORATORY DATA ANALYSIS:
plot(Startups$R.D.Spend, Startups$Profit)


plot(Administration, Profit)
plot(Marketing.Spend,Profit)
plot(State, Profit)

# Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, S
#tate) - SCATTER DIAGRAM
dev.off()
pairs(Startups)


#Correlation coefficient
cor(Startups)

#Summary
summary(Startups)


#Apply Normilization technique to the whole dataset

normalize <- function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm <- as.data.frame(lapply(Startups, FUN = normalize))
summary(Startups_norm$Profit)


#Data partitiion
set.seed(123)
ind <- sample(2,nrow(Startups_norm),replace = TRUE, prob=c(0.8,0.2))                  
startup_train <- Startups_norm[ind==1,]
startup_test <- Startups_norm[ind==2,]




#creating a neural network model on training data

startup_model <- neuralnet(Profit~., data = startup_train)
str(startup_model)


#VISULIZATION
dev.off()
plot(startup_model, rep = "best")

summary(startup_model)

par(mar = numeric(4), family = "serif")
plotnet(startup_model, alpha = 0.6)



#Evaluating model performance

set.seed(12323)
model_results <- compute(startup_model,startup_test[1:4])
predicted_profit <- model_results$net.result
# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startup_test$Profit)



str_max <- max(startup_test$Profit)
str_min <- min(startup_test$Profit)

unnormalize <- function(x,min,max){
  return((max-min)*x+min)
}

actual <-  unnormalize(predicted_profit,str_min,str_max)
head(actual)


#Improve the model

set.seed(12345)
startup_model1 <- neuralnet(Profit~., data = startup_train, hidden = 2)
plot(startup_model1, rep = "best")


model_results2 <- compute(startup_model1,startup_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startup_test$Profit)


plot(predicted_Profit2, startup_test$Profit)
par(mar = numeric(4), family = 'serif')
plotnet(startup_model1, alpha = 0.6)
