data(iris)

library(caret)
set.seed(1234)
index <- createDataPartition(iris$Species,
                             p = 0.7,
                             list = FALSE)

train <- iris[index,]
test <- iris [-index,]

# try k = 5
set.seed(1234)
model.knn <- train(Species ~.,
                   data = train,
                   preProcess = c("center", "scale"),
                   method = "knn",
                   trControl = trainControl(method = "none"),
                   metric = "Accuracy",
                   tuneGrid = data.frame(k=5))

#variable importance
varImp(model.knn)
plot(varImp(model.knn))

#evaluate the model on test data\
knnpred <- predict(model.knn, test)
confusionMatrix(knnpred, test$Species)
