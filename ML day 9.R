#k-fold cross validation with caret

#load packages
library(caret)
library(MASS)
library(regclass)

# create a train and test datasets
set.seed(1234)
index <- createDataPartition(Boston$medv,
                             p = .8,
                             list = FALSE)
train <- Boston[index,]
test <- Boston[-index,]

#multiple regression

#define train control
train_control <- trainControl(method ="cv", number = 10)

# train model
set.seed(1234)
model.lm <- train(medv ~ ., data=train,
                  trControl = train_control,
                  method = "lm",
                  metric = "RMSE")

model.lm
model.lm$resample

#define training control
train_control <- trainControl(method="cv", number = 10)

# train model
set.seed(1234)
model.knn <- train(medv ~., data= train,
                   trControl= train_control,
                   method = "knn",
                   metric = "RMSE")

model.knn

# choose number of hyperparameter values

set.seed(1234)
model.knn <- train(medv ~., data= train,
                   trControl= train_control,
                   method = "knn",
                   metric = "RMSE",
                   tuneGrid = data.frame(k=c(2,20,2)))

model.knn

#evaluate best on test data
pred <- pred(model.lm, newdata=test)
postResample(pred,test$medv)

# logistic regression
data(PIMA)
table(PIMA$Diabetes)
prop.table(table(PIMA$Diabetes))

#train the model
train_control <- trainControl(method="cv", number =10,
                              classProb= TRUE,
                              summaryFunction=twoClassSummary)
set.seed(1234)
model.lr <- train(Diabetes ~ ., data= PIMA,
                  trControl = train_control,
                  method = "glm",
                  family = "binomial",
                  metric = "ROC")

model.lr$resample
model.lr

pred <- predict(model.lr, PIMA)
confusionMatrix(pred, PIMA$Diabetes, positive ="Yes")