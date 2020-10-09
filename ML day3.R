#multiple regression of advertising data

#import and explore data
library(readr)
advertising <- read_csv("Advertising.csv")

library(qacr)
df_summary(advertising)
histograms(advertising)
round(cor(advertising),3)
corplot(advertising)

#create train and test datasets
library(caret)
set.seed(1234)
index <- createDataPartition(advertising$Sales, p = 0.8, list =FALSE)
train <- advertising[index, ]
test <- advertising[-index,]

# build model on training data
fit <- lm(Sales ~ TV + Radio + Newspaper, data=train)
summary(fit)
model.lm <- train(Sales ~ TV + Radio + Newspaper,
                  data = train,
                  method = "lm",
                  metric = "RMSE",
                  trControl = trainControl(method="none"))

#visual results
#install.package("visreg")
library(visreg)
visreg(model.lm, gg = TRUE)

#visualize relative importance 
#install.packages("relaimpo")
library(relaimpo)
imp <- calc.relimp(model.lm$finalModel, rela=TRUE)
plot(imp)

#emplore interactions
model2.lm <- train(Sales ~ TV + Radio + TV*Radio,
                   data = train,
                   method ="lm",
                   metric = "RMSE",
                   trControl = trainControl(method ="none"))

summary(model2.lm)

library(visreg)
visreg(model2.lm , "Radio", by="TV", gg = TRUE)

# evaluate model on test data
test$predSales <- predict(model2.lm, newdata=test)
postResample(pred=test$predSales, obs=test$Sales)
