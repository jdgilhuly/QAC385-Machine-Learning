#Analyze salary data
install.packages("carData")
library(carData)
data(Salaries)

#create train and test datasets
library(caret)
set.seed(1234)
index <- createDataPartition(Salaries$salary)
train <- Salaries[index, ]
test <- Salaries[-index, ]

#build model
model.lm <- train(salary ~ .,
                  data = train,
                  method ="lm",
                  metric = "RMSE",
                  trControl = trainControl(method ="none"))

summary(model.lm)

library(visreg)
visreg(model.lm)

#predict with new data
test$predSalary <- predict(model.lm, newdata=test)
postResample(pred=test$predSalary, obs=test$salary)

library(relaimpo)
imp <- calc.relimp(model.lm$finalModel, rela = TRUE)
plot(imp, names.abbrev=6)

