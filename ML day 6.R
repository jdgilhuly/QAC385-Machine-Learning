#Wisconsin Breast Cancer
#Logistic Regression

#get data
#install.packages("OneR")
library(OneR)
data(breastcancer)
help(breastcancer)

#review data
library(qacr)
df_summary(breastcancer)
histograms(breastcancer)
barcharts(breastcancer)
tab(breastcancer, Class)

# housekeeping
df <- na.omit(breastcancer)
names(df) <- make.names(names(df))
names(df) <- tolower(names(df))

# what is predicted
contrasts(df$class)

# create train and test datasets
library(caret)
set.seed(1234)
index <- createDataPartition(df$class,
                              p = 0.8,
                              list = FALSE)

train <- df[index, ]
test <- df[-index, ]
tab(train,class)
tab(test,class)

#fit logistic regression model
library(caret)
model.lr <- train(class ~.,
                  data = train,
                  method = "glm",
                  family = "binomial",
                  metric = "Accuracy",
                  trControl = trainControl(method ="none"))

summary(model.lr)

#look at the coefficients in the model
round(coef(model.lr$finalModel),3)
round(exp(coef(model.lr$finalModel)), 3)

#examine preformance in training data
train$pred <- factor(predict(model.lr, train))
train$prob <- predict(model.lr, train, type = "prob")
confusionMatrix(train$pred, train$class, positive="malignant")

#ROC
source("enhanced ROC curve.R")

myROC(train$prob, train$class)

train$pred.24 <- ifelse(train$prob < .24, "benign", "malignant")
train$pred.24 <- as.factor(train$pred.24)
confusionMatrix(train$pred.24, train$class, positive="malignant")

#relative importance of variables
varImp(model.lr)
plot(varImp(model.lr))

#examine marginal impact of bare nuclei
#holding other variables constant
#install.packages("ggeffects")
library(ggplot)
plotdata <- ggpredict(model.lr$finalModel, terms = "bare.nuclei")
ggplot(plotdata, aes(x,predicted)) + geom_point() +
  geom_line() + labs(x="Bare Nuclei", y= "Prob(Malignant)")+
  scale_x_continuous(breaks=c(1:10))

#evaluate model on test data
test$prob <- predict(model.lr, test, type="prob")[2]
test$pred.24 <- ifelse(test$prob < .24, "benign", "malignant")
test$pred.24 <- ifelse(test$prob <.24, "benign", "malignant")
