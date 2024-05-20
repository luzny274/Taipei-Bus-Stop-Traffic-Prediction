### Preprocessing --------------------------------------------------------------

# load the data ----------------------------------------------------------------

FinalData_reshaped <- read.csv("ModelData/FinalData_reshaped2.csv")[-1,]; head(FinalData_reshaped); dim(FinalData_reshaped)
FinalData_reshaped_Y <- read.csv("ModelData/FinalData_reshaped_Y2.csv"); head(FinalData_reshaped_Y); dim(FinalData_reshaped_Y)
colnames(FinalData_reshaped_Y) <- c("中山_mrt_flow","北投_mrt_flow","北門_mrt_flow","古亭_mrt_flow","士林_mrt_flow","大橋頭_mrt_flow","松山_mrt_flow")

# remove rows with incomplete Y
incomplete1 <- which(is.na(FinalData_reshaped_Y))
incomplete2 <- which(is.na(FinalData_reshaped_Y))+1
incomplete <- unique(append(incomplete1,incomplete2))
FinalData_reshaped <- FinalData_reshaped[-incomplete,]
FinalData_reshaped_Y <- FinalData_reshaped_Y[-incomplete,]

# train-val-test splitting -----------------------------------------------------
sample_cnt <- nrow(FinalData_reshaped) # v1: 9981 # v2: 11293
train_cnt <- as.integer(sample_cnt*0.7) # v1: 6986 # v2: 7905
val_cnt <- as.integer(sample_cnt*0.1) # v1: 998 # v2: 1129
test_cnt <- sample_cnt-train_cnt-val_cnt # v1: 1996 # v2: 2259

Train_X <- FinalData_reshaped[1:(train_cnt),] # v1: 1-48902 # v2: 1-55335
Val_X <- FinalData_reshaped[(train_cnt+1):(train_cnt+val_cnt),] # v1: 48903-55888 # v2: 55336-63238
Test_X <- FinalData_reshaped[-(1:(train_cnt+val_cnt)),] # v1: 55889-69867 # v2: 63239-79051

Train_Y <- FinalData_reshaped_Y[1:(train_cnt),] # v1: 1-48902 # v2: 1-55335
Val_Y <- FinalData_reshaped_Y[(train_cnt+1):(train_cnt+val_cnt),] # v1: 48903-55888 # v2: 55336-63238
Test_Y <- FinalData_reshaped_Y[-(1:(train_cnt+val_cnt)),] # v1: 55889-69867 # v2: 63239-79051

# robust scaling (r encodes categorical var automatically for some models) -----

# categorical var
for(i in c(2:4,31:37)){
  Train_X[,i] <- as.character(Train_X[,i])
  Val_X[,i] <- as.character(Val_X[,i])
  Test_X[,i] <- as.character(Test_X[,i])
}

# numerical var
for(i in c(5:30,38:142)){
  Train_X[,i] <- as.numeric(Train_X[,i])
  median <- median(Train_X[,i], na.rm = TRUE); iqr <- IQR(Train_X[,i], na.rm = TRUE); if(is.na(iqr)){iqr = 1}; if(iqr == 0){iqr = 1}
  Train_X[,i] <- (Train_X[,i]-median)/iqr
  Train_X[is.na(Train_X[,i]), i] <- 0
  
  Val_X[,i] <- as.numeric(Val_X[,i])
  median <- median(Val_X[,i], na.rm = TRUE); iqr <- IQR(Val_X[,i], na.rm = TRUE); if(is.na(iqr)){iqr = 1}; if(iqr == 0){iqr = 1}
  Val_X[,i] <- (Val_X[,i]-median)/iqr
  Val_X[is.na(Val_X[,i]), i] <- 0
  
  Test_X[,i] <- as.numeric(Test_X[,i])
  median <- median(Test_X[,i], na.rm = TRUE); iqr <- IQR(Test_X[,i], na.rm = TRUE); if(is.na(iqr)){iqr = 1}; if(iqr == 0){iqr = 1}
  Test_X[,i] <- (Test_X[,i]-median)/iqr
  Test_X[is.na(Test_X[,i]), i] <- 0
}

Train_Y_original <- Train_Y; Val_Y_original <- Val_Y; Test_Y_original <- Test_Y
for(i in c(1:7)){
  Train_Y[,i] <- as.numeric(Train_Y[,i])
  median <- median(Train_Y[,i], na.rm = TRUE); iqr <- IQR(Train_Y[,i], na.rm = TRUE); if(is.na(iqr)){iqr = 1}; if(iqr == 0){iqr = 1}
  Train_Y[,i] <- (Train_Y[,i]-median)/iqr
  Train_Y[is.na(Train_Y[,i]), i] <- 0
  
  Val_Y[,i] <- as.numeric(Val_Y[,i])
  median <- median(Val_Y[,i], na.rm = TRUE); iqr <- IQR(Val_Y[,i], na.rm = TRUE); if(is.na(iqr)){iqr = 1}; if(iqr == 0){iqr = 1}
  Val_Y[,i] <- (Val_Y[,i]-median)/iqr
  Val_Y[is.na(Val_Y[,i]), i] <- 0
  
  Test_Y[,i] <- as.numeric(Test_Y[,i])
  median <- median(Test_Y[,i], na.rm = TRUE); iqr <- IQR(Test_Y[,i], na.rm = TRUE); if(is.na(iqr)){iqr = 1}; if(iqr == 0){iqr = 1}
  Test_Y[,i] <- (Test_Y[,i]-median)/iqr
  Test_Y[is.na(Test_Y[,i]), i] <- 0
}

# reshape again (some model package in R cannot fit with multiple y) -----------
cat_id_start <- 10; cat_id_end <- 142
v_names <- c("previous_mrt_flow","bike_flow","aqi","status","so2","co","o3","o3_8hr","pm10","pm2.5","no2","nox","no","windspeed","winddirec","co_8hr","pm2.5_avg","pm10_avg","so2_avg")

Train_X2 <- reshape(Train_X[,c(1:(cat_id_start+6))], direction='long', varying=c(cat_id_start:(cat_id_start+6)), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=v_names[1])
Train_X2 <- Train_X2[,-ncol(Train_X2)]
i <- cat_id_start + 7
while(1){
  tmp_reshaped2 <- reshape(Train_X[,c(i:(i+6))], direction='long', varying=c(1:7), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=v_names[(i-cat_id_start)/7+1])
  Train_X2 <- cbind(Train_X2,tmp_reshaped2[,1:2])
  Train_X2 <- Train_X2[,-(ncol(Train_X2)-1)]
  i <- i + 7; if (i > cat_id_end){ break }
}
Train_Y2 <- reshape(Train_Y, direction='long', varying=c(1:7), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=c("mrt_flow"))
Train_Y2_original <- reshape(Train_Y_original, direction='long', varying=c(1:7), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=c("mrt_flow"))

Val_X2 <- reshape(Val_X[,c(1:(cat_id_start+6))], direction='long', varying=c(cat_id_start:(cat_id_start+6)), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=v_names[1])
Val_X2 <- Val_X2[,-ncol(Val_X2)]
i <- cat_id_start + 7
while(1){
  tmp_reshaped2 <- reshape(Val_X[,c(i:(i+6))], direction='long', varying=c(1:7), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=v_names[(i-cat_id_start)/7+1])
  Val_X2 <- cbind(Val_X2,tmp_reshaped2[,1:2])
  Val_X2 <- Val_X2[,-(ncol(Val_X2)-1)]
  i <- i + 7; if (i > cat_id_end){ break }
}
Val_Y2 <- reshape(Val_Y, direction='long', varying=c(1:7), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=c("mrt_flow"))
Val_Y2_original <- reshape(Val_Y_original, direction='long', varying=c(1:7), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=c("mrt_flow"))

Test_X2 <- reshape(Test_X[,c(1:(cat_id_start+6))], direction='long', varying=c(cat_id_start:(cat_id_start+6)), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=v_names[1])
Test_X2 <- Test_X2[,-ncol(Test_X2)]
i <- cat_id_start + 7
while(1){
  tmp_reshaped2 <- reshape(Test_X[,c(i:(i+6))], direction='long', varying=c(1:7), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=v_names[(i-cat_id_start)/7+1])
  Test_X2 <- cbind(Test_X2,tmp_reshaped2[,1:2])
  Test_X2 <- Test_X2[,-(ncol(Test_X2)-1)]
  i <- i + 7; if (i > cat_id_end){ break }
}
Test_Y2 <- reshape(Test_Y, direction='long', varying=c(1:7), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=c("mrt_flow"))
Test_Y2_original <- reshape(Test_Y_original, direction='long', varying=c(1:7), timevar="mrt_station",times=c("中山", "北投", "北門", "古亭", "士林", "大橋頭", "松山"),v.names=c("mrt_flow"))

original_mrt_flow <- Train_Y2_original[,c(2)]; Train <- cbind(Train_Y2[,c(1:2)],original_mrt_flow,Train_X2[,c(2:9,11:29)])
original_mrt_flow <- Val_Y2_original[,c(2)]; Val <- cbind(Val_Y2[,c(1:2)],original_mrt_flow,Val_X2[,c(2:9,11:29)])
original_mrt_flow <- Test_Y2_original[,c(2)]; Test <- cbind(Test_Y2[,c(1:2)],original_mrt_flow,Test_X2[,c(2:9,11:29)])

# check data -------------------------------------------------------------------
head(Train); summary(Train); dim(Train) # 55335/7 = 7905
head(Val); summary(Val); dim(Val) # 7903/7 = 1129
head(Test); summary(Test); dim(Test) # 15813/7 = 2259

#write.csv(Train,"ModelData/Train2_long.csv",row.names = F)
#write.csv(Val,"ModelData/Val2_long.csv",row.names = F)
#write.csv(Test,"ModelData/Test2_long.csv",row.names = F)


### Modeling -------------------------------------------------------------------

## 1: multiple y format (LR) ---------------------------------------------------

# RMSE (testing): 0.3081202

# read data --------------------------------------------------------------------

X_train <- read.csv("ModelData/X_train2.csv")[,-1]; head(X_train); dim(X_train) # 7905, 205
Y_train <- read.csv("ModelData/Y_train2.csv")[,-1]; head(Y_train); dim(Y_train) # 7905, 7

X_val <- read.csv("ModelData/X_val2.csv")[,-1]; head(X_val); dim(X_val) # 1129, 205
Y_val <- read.csv("ModelData/Y_val2.csv")[,-1]; head(Y_val); dim(Y_val) # 1129, 7

X_test <- read.csv("ModelData/X_test2.csv")[,-1]; head(X_test); dim(X_test) # 2259, 205
Y_test <- read.csv("ModelData/Y_test2.csv")[,-1]; head(Y_test); dim(Y_test) # 2259, 7

colnames(Y_train) <- c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")
Data_train <- cbind(Y_train,X_train)

colnames(Y_val) <- c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")
Data_val <- cbind(Y_val,X_val)

colnames(Y_test) <- c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")
Data_test <- cbind(Y_test,X_test)



# LR ---------------------------------------------------------------------------

m1 <- lm(cbind(Data_train$Y1,Data_train$Y2,Data_train$Y3,Data_train$Y4,Data_train$Y5,Data_train$Y6,Data_train$Y7) ~ . , Data_train[,-c(1:7)])
#summary(m1)
mean(m1$residuals^2) # 0.04109282
sqrt(mean(m1$residuals^2)) # 0.2027136

y_pred <- predict(m1, Data_val)
y_real <- Y_val
y_pred <- rbind(y_pred[,1],y_pred[,2],y_pred[,3],y_pred[,4],y_pred[,5],y_pred[,6],y_pred[,7])
y_real <- rbind(y_real[,1],y_real[,2],y_real[,3],y_real[,4],y_real[,5],y_real[,6],y_real[,7])
mean((y_real-y_pred)^2) # 0.1117158
sqrt(mean((y_real-y_pred)^2)) # 0.3342391

y_pred <- predict(m1, Data_test)
y_real <- Y_test
y_pred <- rbind(y_pred[,1],y_pred[,2],y_pred[,3],y_pred[,4],y_pred[,5],y_pred[,6],y_pred[,7])
y_real <- rbind(y_real[,1],y_real[,2],y_real[,3],y_real[,4],y_real[,5],y_real[,6],y_real[,7])
mean((y_real-y_pred)^2) # 0.09493806
sqrt(mean((y_real-y_pred)^2)) # 0.3081202



## 2: one y format, all var & station interaction (LR, LASSO, Ridge, DT) -------

# RMSE (testing): 0.2609548, 0.2647687, 0.2586306, 0.3229786 (best: Ridge)

# read data --------------------------------------------------------------------

Train <- read.csv("ModelData/Train2_long.csv"); head(Train); dim(Train)
Val <- read.csv("ModelData/Val2_long.csv"); head(Val); dim(Val)
Test <- read.csv("ModelData/Test2_long.csv"); head(Test); dim(Test)

# temp
#bike_flow <- append(c(0),Train$bike_flow)
#Train$bike_flow <- bike_flow[1:(length(bike_flow)-1)]
#bike_flow <- append(c(0),Val$bike_flow)
#Val$bike_flow <- bike_flow[1:(length(bike_flow)-1)]
#bike_flow <- append(c(0),Test$bike_flow)
#Test$bike_flow <- bike_flow[1:(length(bike_flow)-1)]

# categorical var
for(i in c(4:6)){
  Train[,i] <- as.character(Train[,i])
  Val[,i] <- as.character(Val[,i])
  Test[,i] <- as.character(Test[,i])
}
median_test <- median(Test[,3]); iqr_test <- IQR(Test[,3])
Train <- Train[,-3]; Val <- Val[,-3]; Test <- Test[,-3]
summary(Train); summary(Val); summary(Test)

# linear regression with interactions ------------------------------------------
lr_model <- lm(mrt_flow~ . * mrt_station, Train)
#lr_model <- lm(mrt_flow ~ . * mrt_station, rbind(Train,Val))
summary(lr_model)
mean(lr_model$residuals^2) # 0.06650603
sqrt(mean(lr_model$residuals^2)) # 0.2578876

y_pred <- predict(lr_model, Val)
y_real <- Val$mrt_flow
mean((y_real-y_pred)^2) # 0.1042135
sqrt(mean((y_real-y_pred)^2)) # 0.3228212

y_pred <- predict(lr_model, Test)
y_real <- Test$mrt_flow
mean((y_real-y_pred)^2) # 0.0680974
sqrt(mean((y_real-y_pred)^2)) # 0.2609548
cor(y_real[1:length(y_real)],y_pred[1:length(y_pred)])

# lasso with interactions ------------------------------------------------------
f <- as.formula(mrt_flow ~ .* mrt_station) # using .*. for all interactions
y <- Train$mrt_flow
x <- model.matrix(f, Train)[,-1] # using model.matrix to take advantage of f
#y <- rbind(Train,Val)$mrt_flow
#x <- model.matrix(f, rbind(Train,Val))[,-1] # using model.matrix to take advantage of f

library(glmnet)
lasso_kfold <- cv.glmnet(x, y, alpha=0, nfolds=10)
lasso_best_lambda <- lasso_kfold$lambda.min
lasso_model <- glmnet(x, y, alpha=0, lambda=lasso_best_lambda)

x <- model.matrix(mrt_flow ~.*mrt_station, rbind(Train,Val))[,-1][-(1:nrow(Train)),]
y_pred <- predict(lasso_model, x)
y_real <- Val$mrt_flow
mean((y_real-y_pred)^2) # 0.1019762
sqrt(mean((y_real-y_pred)^2)) # 0.3193371

x <- model.matrix(mrt_flow ~.*mrt_station, rbind(Train,Test))[,-1][-(1:nrow(Train)),]
#x <- model.matrix(mrt_flow ~.*mrt_station, rbind(Train,Val,Test))[,-1][-(1:nrow(rbind(Train,Val))),]
y_pred <- predict(lasso_model, x)
y_real <- Test$mrt_flow
mean((y_real-y_pred)^2) # 0.07016843
sqrt(mean((y_real-y_pred)^2)) # 0.2647687
cor(y_real[1:length(y_real)],y_pred[1:length(y_pred)])

# ridge with interactions ------------------------------------------------------
f <- as.formula(mrt_flow ~ .* mrt_station) # using .*. for all interactions
y <- Train$mrt_flow
x <- model.matrix(f, Train)[,-1] # using model.matrix to take advantage of f
#y <- rbind(Train,Val)$mrt_flow
#x <- model.matrix(f, rbind(Train,Val))[,-1] # using model.matrix to take advantage of f

library(glmnet)
ridge_kfold <- cv.glmnet(x, y, alpha=1, nfolds=10)
ridge_best_lambda <- ridge_kfold$lambda.min
ridge_model <- glmnet(x, y, alpha=0, lambda=ridge_best_lambda)

x <- model.matrix(mrt_flow ~.*mrt_station, rbind(Train,Val))[,-1][-(1:nrow(Train)),]
y_pred <- predict(ridge_model, x)
y_real <- Val$mrt_flow
mean((y_real-y_pred)^2) # 0.1014082
sqrt(mean((y_real-y_pred)^2)) # 0.3184465

x <- model.matrix(mrt_flow ~.*mrt_station, rbind(Train,Test))[,-1][-(1:nrow(Train)),]
#x <- model.matrix(mrt_flow ~.*mrt_station, rbind(Train,Val,Test))[,-1][-(1:nrow(rbind(Train,Val))),]
y_pred <- predict(ridge_model, x)
y_real <- Test$mrt_flow
mean((y_real-y_pred)^2) # 0.06688979
sqrt(mean((y_real-y_pred)^2)) # 0.2586306
cor(y_real[1:length(y_real)],y_pred[1:length(y_pred)])

# regression tree (decision tree) with interactions ----------------------------
library(rpart)
dt_model <- rpart(mrt_flow~., Train, cp=0.000005)
#dt_model <- rpart(mrt_flow~.,rbind(Train,Val))
#summary(dt_model)
#printcp(dt_model)
#plotcp(dt_model)
#dt_model_pruned <- prune(dt_model, cp = 0.0000001)

y_pred <- predict(dt_model, Val)
y_real <- Val$mrt_flow
mean((y_real-y_pred)^2) # 0.05843638
sqrt(mean((y_real-y_pred)^2)) # 0.2417362

y_pred <- predict(dt_model, Test)
#y_pred <- predict(dt_model_pruned, Test)
y_real <- Test$mrt_flow
mean((y_real-y_pred)^2) # 0.02926891
sqrt(mean((y_real-y_pred)^2)) # 0.1710816
cor(y_real[1:length(y_real)],y_pred[1:length(y_pred)])

# GLMNET with interactions -----------------------------------------------------

for (i in seq(0,1,0.05)){

f <- as.formula(mrt_flow ~ .* mrt_station) # using .*. for all interactions
y <- Train$mrt_flow
x <- model.matrix(f, Train)[,-1] # using model.matrix to take advantage of f
#y <- rbind(Train,Val)$mrt_flow
#x <- model.matrix(f, rbind(Train,Val))[,-1] # using model.matrix to take advantage of f

library(glmnet)
glmnet_kfold <- cv.glmnet(x, y, alpha=i, nfolds=10)
glmnet_best_lambda <- glmnet_kfold$lambda.min
glmnet_model <- glmnet(x, y, alpha=0, lambda=glmnet_best_lambda)

x <- model.matrix(mrt_flow ~.*mrt_station, rbind(Train,Val))[,-1][-(1:nrow(Train)),]
y_pred <- predict(glmnet_model, x)
y_real <- Val$mrt_flow
mean((y_real-y_pred)^2) # 0.1014082
cat(i, sqrt(mean((y_real-y_pred)^2)), " ") # 0.3184465

x <- model.matrix(mrt_flow ~.*mrt_station, rbind(Train,Test))[,-1][-(1:nrow(Train)),]
#x <- model.matrix(mrt_flow ~.*mrt_station, rbind(Train,Val,Test))[,-1][-(1:nrow(rbind(Train,Val))),]
y_pred <- predict(glmnet_model, x)
y_real <- Test$mrt_flow
mean((y_real-y_pred)^2) # 0.06688979
cat(i, sqrt(mean((y_real-y_pred)^2)), " ") # 0.2586306
cor(y_real[1:length(y_real)],y_pred[1:length(y_pred)])
}
