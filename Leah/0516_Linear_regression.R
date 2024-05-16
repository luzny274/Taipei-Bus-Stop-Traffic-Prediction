library(Metrics)
X_train <- read.table("C:\\Users\\my613\\Downloads\\Final Project\\Data\\X_train_reshaped.csv", sep = ',', header = TRUE)
Y_train <- read.table("C:\\Users\\my613\\Downloads\\Final Project\\Data\\Y_train_reshaped.csv", sep = ',', header = TRUE)
Y <- as.matrix(Y_train)

LR <- lm(Y~., data = X_train)
summary(LR)

X_test <- read.table("C:\\Users\\my613\\Downloads\\Final Project\\Data\\X_test_reshaped.csv", sep = ',', header = TRUE)
Y_test <- read.table("C:\\Users\\my613\\Downloads\\Final Project\\Data\\Y_test_reshaped.csv", sep = ',', header = TRUE)

LR_predict <- predict(LR, newdata = X_test)
LR_predict <- data.frame(LR_predict)
lapply(1:7, function(x) rmse(Y_test[,x], LR_predict[,x]))
