library(lubridate)
library(dplyr)
library(DescTools)

original_data <- read.table("C:\\Users\\my613\\Downloads\\Final Project\\Data\\FinalData2.csv", sep = ',', header = TRUE)
data_clean <- original_data

#delete useless columns
col_delete_id <- c(5, 6, 7, 8, 10)  #day/next_dist/bus_cnt/bike_cnt/school_cnt
data_clean <- data_clean[-col_delete_id]


#add "day_in_a_week" & "month"
data_clean <- data_clean %>% mutate("date" = sub(" .*", "", original_data[,1]))
data_clean <- data_clean %>% mutate(
  "day_in_a_week" = as.character(wday(as.Date(data_clean[,28]), week_start = 1)), 
  "month" = substring(data_clean[,28], 6, 7))


#replace "-" and "" of "windspeed" & "winddirec"
data_clean[data_clean == "-"] <- NA
data_clean[which(data_clean[, 22] == ""), 22] <- NA
data_clean[which(data_clean[, 23] == ""), 23] <- NA


#check data type
data_clean[, 4] <- as.character(data_clean[, 4])
data_clean[, 22] <- as.numeric(data_clean[, 22])
data_clean[, 23] <- as.numeric(data_clean[, 23])
lapply(data_clean, class)


#split X and Y
cat_id <- c(1, 2, 4, 12, 29, 30) #("datetime", "mrt_station", "hour", "status", "day_in_a_week", "month")
num_id <- which(!seq(1, 27)%in%c(cat_id, 3))

Y_raw <- data_clean[,c(1,2,3)]
X_raw <- data_clean[c(cat_id, num_id)]

Y_reshaped <- reshape(Y_raw, idvar = "datetime", timevar = "mrt_station", direction = "wide")
Y_reshaped <- Y_reshaped[-1]
rownames(Y_reshaped) <- seq(1, 11402)


#add pre_mrt_flow
pre_flow <- Y_reshaped
pre_flow[2:nrow(Y_reshaped),] <- pre_flow[1:nrow(Y_reshaped)-1,]
pre_flow[1,] <- NA
colnames(pre_flow) <- c("pre_mrt_flow.中山", "pre_mrt_flow.北投", "pre_mrt_flow.北門", "pre_mrt_flow.古亭", "pre_mrt_flow.士林", "pre_mrt_flow.大橋頭", "pre_mrt_flow.松山")

X_common_id <- c(c(3, 5, 6), seq(8,12)) 
X_reshaped <- cbind(unique(X_raw[X_common_id]), pre_flow, reshape(X_raw, idvar = "datetime", timevar = "mrt_station", direction = "wide", drop = X_common_id))
X_reshaped <- X_reshaped[-16]


#delete incomplete rows
row_delete <- which(is.na(Y_reshaped) == TRUE)
row_delete <- unique(row_delete - (11402 * floor(row_delete/11402)))
row_delete <- c(1, unique(c(row_delete, row_delete + 1)))

Y_reshaped <- Y_reshaped[-row_delete,]
X_reshaped <- X_reshaped[-row_delete,]


#split training and testing data
train_index <- seq(1, round(nrow(X_reshaped) * 0.8, 0))
testing_index <- seq(round(nrow(X_reshaped) * 0.8) + 1, nrow(X_reshaped))

X_train <- X_reshaped[train_index, ]
Y_train <- Y_reshaped[train_index, ]

X_test <- X_reshaped[testing_index, ]
Y_test <- Y_reshaped[testing_index, ]


#Robust scaled for numeric variables
scaled_id <- which(!seq(1,141)%in%c(seq(1,3), seq(16,141,18), c(7,8,48), seq(33,141,18)))
X_train[,scaled_id] <-RobScale(X_train[,scaled_id])
X_train[is.na(X_train) == TRUE] <- 0

scaled_id_test <- which(!seq(1,141)%in%c(seq(1,3), seq(16,141,18), c(7,8, 48, 84), seq(33,141,18)))
X_test[,scaled_id_test] <-RobScale(X_test[,scaled_id_test])
X_test[is.na(X_test) == TRUE] <- 0
#which(is.na(X_test))


#export file
#write.csv(X_train, "C:\\Users\\my613\\Downloads\\Final Project\\Data\\X_train_reshaped.csv", row.names=FALSE)
#write.csv(Y_train, "C:\\Users\\my613\\Downloads\\Final Project\\Data\\Y_train_reshaped.csv", row.names=FALSE)
#write.csv(X_test, "C:\\Users\\my613\\Downloads\\Final Project\\Data\\X_test_reshaped.csv", row.names=FALSE)
#write.csv(Y_test, "C:\\Users\\my613\\Downloads\\Final Project\\Data\\Y_test_reshaped.csv", row.names=FALSE)

#test <- read.table("C:\\Users\\my613\\Downloads\\Final Project\\X_train_reshaped.csv", sep = ',', header = TRUE)
