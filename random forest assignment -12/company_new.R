Company_Data <- read.csv("G:\\ExcelR\\Assignments\\Decision Tree\\company Data\\Company_Data.csv")

data1<- Company_Data

str(Company_Data)

mean(Company_Data$Sales)

median(Company_Data$Sales)

hist(Company_Data$Sales)

box<-boxplot(Company_Data$Sales)

str(box)

summary(Company_Data$Sales)

Company_Data$Sales <- cut(Company_Data$Sales,breaks = c(0,6,11,17),labels = c("low","medium","high"))

library(caret)

library(C50)

dataset <- createDataPartition(Company_Data$Sales, p=0.7,list = FALSE)

train_data <- Company_Data[dataset,]

test_data <- Company_Data[-dataset,]

model_tree <-C5.0(train_data$Sales~.,data = train_data,method = "class")

model.pred <- predict(model_tree,test_data[,-1])

model.table <- table(test_data$Sales,model.pred,dnn = c("Actual","Predicted"))

confusionMatrix(model.table)

# model accuracy = 88.14%

summary(model_tree) 

plot(model_tree)

#random forrest

fit <- randomForest(train_data$Sales~.,data = train_data,importance=T,na.action = na.roughfix)

fit

importance(x = fit,type = 2)

fit.pred <- predict(fit,test_data[,-1])

fit.table <- table(test_data$Sales,fit.pred,dnn = c("Actual","Predicted"))

confusionMatrix(fit.table)
# Accuracy = 73.73%


