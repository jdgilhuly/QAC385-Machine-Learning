library(qacr)
library(OneR)
Employee.Attrition <- read.csv("~/Downloads/Employee-Attrition.csv", header=TRUE)
#employee count all 1
#over 18 useless
#Standard Hours all 80
#employeeNumber

df_summary(Employee.Attrition)

Employee.Attrition$EmployeeCount <- NULL
Employee.Attrition$StandardHours <- NULL
Employee.Attrition$Over18 <- NULL

# create training and test data sets
library(caret)
set.seed(1234)
index <- createDataPartition(Employee.Attrition$Attrition, 
                             p=0.8, list=FALSE)
train <- Employee.Attrition[index,]
test <- Employee.Attrition[-index,]

#logreg model
model.lr <- train(Attrition ~ ., 
                  data = train,
                  trControl = trainControl(method="none"),
                  method = "glm",
                  family = "binomial",
                  metric = "Accuracy")

summary(model.lr)
plot(varImp(model.lr))
round(exp(coef(model.lr$finalModel)), 3)
#OverTimeYes: 8.045 times more likely to leave
#Envionment Satisfaction: 0.645 more likely to leave per level
#NumCompaniesWorkedAt: 1.235 more likely per company worked at
#business Travel Frequency
#job satisfaction

library(ROCR)
train$pred <- as.factor(predict(model.lr, train))
train$predProb <- predict(model.lr, train, type="prob")[2]
train$pred <- "No"
train$pred[train$prob>=0.47]

source("enhanced ROC curve.R")
myROC(train$predProb, train$Attrition)
train$pred.47 <- 'No'
train$pred.47[train$prob >= .47 ]<-'Yes'
train$pred.47<- as.factor(train$pred.47)
train$Attrition<- as.factor(train$Attrition)
confusionMatrix(train$pred.47, train$Attrition, positive = 'Yes')




