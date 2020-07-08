concrete <- read.csv(file.choose())
str(concrete)

hist(concrete$strength, probability = T, breaks = 30)
lines(density(concrete$strength))

hist(concrete$age, probability = T, breaks = 30)
lines(density(concrete$age))

hist(concrete$fineagg, probability = T, breaks = 30)
lines(density(concrete$fineagg))

hist(concrete$coarseagg, probability = T, breaks = 30)
lines(density(concrete$coarseagg))

hist(concrete$superplastic, probability = T, breaks = 30)
lines(density(concrete$superplastic))

hist(concrete$water, probability = T, breaks = 30)
lines(density(concrete$water))

hist(concrete$ash, probability = T, breaks = 30)
lines(density(concrete$ash))

hist(concrete$slag, probability = T, breaks = 30)
lines(density(concrete$slag))

hist(concrete$cement, probability = T, breaks = 30)
lines(density(concrete$cement))

#normalize

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete,normalize))
summary(concrete_norm$strength)# normalized in the 0 to 1 range.

summary(concrete$strength)

#test and train

Train <- concrete_norm[1:700,]
Test <- concrete_norm[700:1030,]

library(neuralnet)
head(concrete)

set.seed(123)

concrete_model <- neuralnet(formula = strength~., data = Train)
plot(concrete_model)

library(NeuralNetTools)

par(mar=numeric(4),family='serif')
plotnet(concrete_model, alpha = 0.6)


#Evaluating model performance

model_result <- neuralnet::compute(concrete_model,Test[1:8])

predicted_strength <- model_result$net.result
View(predicted_strength)

cor(predicted_strength, Test$strength) #80.5
head(predicted_strength)


s_min <- min(concrete$strength)
s_min
s_max <- max(concrete$strength)
s_max

unnormalize <- function(x,min,max){
  return((max-min)*x+min)
}

srength_pred <- unnormalize(predicted_strength, s_min, s_max)
head(srength_pred, n=10)


set.seed(12345)

con_model <- neuralnet(strength~., data= Train, hidden = 5,
                       act.fct = "logistic")
plot(con_model)

par(mar = numeric(4), family ='serif')
plotnet(con_model, alpha=0.6)

