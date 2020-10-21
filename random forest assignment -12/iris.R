library(party)

iris

str(iris)

set.seed(1234)

sam <- sample( 2, nrow(iris), replace = TRUE, prob = c(0.7,0.3) )

train_data <- iris[sam==1,]

test_data <- iris[sam==2,]

tree_iris <- ctree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = train_data)

tree_iris

windows()

summary(tree_iris)

plot(tree_iris)


tab <- table(predict(tree_iris), train_data$Species)

tab

pred <- predict(tree_iris, newdata= test_data)

tab2 <- table(pred, test_data$Species)

tab2
library(caret)
confusionMatrix(tab2)
