
## Check 
FinalData <- read.csv("FinalData.csv"); head(FinalData); dim(FinalData)
FinalData2 <- read.csv("FinalData2.csv"); head(FinalData2); dim(FinalData2)

length(unique(FinalData$datetime))
nrow(unique(cbind(FinalData$datetime,FinalData$mrt_station)))

length(unique(FinalData2$datetime))
nrow(unique(cbind(FinalData2$datetime,FinalData2$mrt_station)))

library(dplyr)
check <- group_by(FinalData, datetime)%>%
  summarise(station_cnt=length(mrt_station))
check[check$station_cnt > 7,]

check2 <- group_by(FinalData2, datetime)%>%
  summarise(station_cnt=length(mrt_station))
check2[check2$station_cnt > 7,]


### Data Precprocessing

## read and integrate data
MRTFlow <- read.csv("NewData/MRTFlow2.csv"); head(MRTFlow)
MRTDist <- read.csv("NewData/MRTDist.csv"); head(MRTDist)
BusCnt <- read.csv("NewData/BusCnt.csv"); head(BusCnt)
YoubikeCnt <- read.csv("NewData/YoubikeCnt.csv"); head(YoubikeCnt)
YoubikeFlow <- read.csv("NewData/YoubikeFlow.csv"); head(YoubikeFlow)
SchoolCnt <- read.csv("NewData/SchoolCnt.csv"); head(SchoolCnt)
Weather <- read.csv("NewData/Weather2.csv"); head(Weather)
AirQuality <- read.csv("NewData/AirQuality.csv"); head(AirQuality)

# station features
station <- merge(merge(merge(merge(MRTDist,BusCnt),YoubikeCnt),YoubikeFlow),SchoolCnt)
head(station)
tail(station)
dim(station) # 91375, 7

# environment data
env <- merge(Weather,AirQuality)
head(env)
tail(env)
dim(env) # 91308, 24

# merge
FinalData <- merge(merge(MRTFlow,station,by=c("datetime", "mrt_station")),env,by=c("datetime", "mrt_station"))

# time
library(lubridate)
date <- substr(as.character(FinalData$datetime),1,10)
time <- substr(as.character(FinalData$datetime),12,19)
month <- month(parse_date_time(date, orders = "Ymd"))
week <- strftime(parse_date_time(date, orders = "Ymd"), format = "%A")
day_week <- strftime(parse_date_time(date, orders = "Ymd"), format = "%w")
hour <- hour(parse_date_time(time, orders = "HMS"))

FinalData$month <- month
FinalData$day_week <- day_week
FinalData$hour <- hour
FinalData$day <- "Weekday"
FinalData$day[week == "Saturday" | week == "Sunday"] <- "Weekend"

# transfer
FinalData$transfer <- "N"
FinalData[FinalData$mrt_station == "中山" | FinalData$mrt_station == "古亭",]$transfer <- "Y"

# final data
FinalData <- FinalData[,c(1:3,33:34,4:30)]
head(FinalData)
tail(FinalData)
dim(FinalData) # 4137, 32 (only 2023/11 first) # 74802, 35 # 79754

# check
library(lubridate)
date <- substr(as.character(FinalData$datetime),1,10)
table(date) # 10-21 hrs a day

# write the data (Final Data)
write.csv(FinalData,"FinalData2.csv", row.names = F)

## 1.MRTFlow -----------------------------------------------------------------------------------------------

# read the data (Raw Data1)
library(data.table)
data_info <- read.csv("RawData1/臺北捷運每日分時各站OD流量統計資料.csv")
head(data_info)

flow_ym <- data_info[which(data_info[,2] == "202206"):which(data_info[,2] == "202311"),][,2]
#flow_url <- data_info[which(data_info[,2] == "202206"):which(data_info[,2] == "20231"),]$URL

#url <- flow_url[1]

flow_data <- NULL
for(ym in flow_ym){
  #for (url in flow_url){

  flow_csv <- fread(paste0("RawData1/臺北捷運每日分時各站OD流量統計資料_",ym,".csv"))
  # flow_csv <- fread(url)
  head(flow_csv)
  dim(flow_csv)
  
  # change column names
  colnames(flow_csv) <- c("date", "hour", "mrt_from", "mrt_to", "people_cnt")
  colnames(flow_csv)
  
  # count total numbers of flows
  library(dplyr)
  flow_from <- group_by(flow_csv, date, hour, mrt_from)%>%
    summarise(mrt_flow=sum(people_cnt))
  colnames(flow_from)[3] <- "mrt_station"
  head(flow_from)
  dim(flow_from)
  
  flow_to <- group_by(flow_csv, date, hour, mrt_to)%>%
    summarise(mrt_flow=sum(people_cnt))
  colnames(flow_to)[3] <- "mrt_station"
  head(flow_to)
  dim(flow_to)
  
  flow_sum <- group_by(rbind(flow_from,flow_to), date, hour, mrt_station)%>%
    summarise(mrt_flow=sum(mrt_flow))
  head(flow_sum)
  dim(flow_sum)
  
  flow_sum$mrt_station <- gsub("站", "", flow_sum$mrt_station)
  
  # extract 中山、松山、大同（大橋頭）、古亭、萬華（北門）、士林、陽明（北投）
  flow_month <- flow_sum[flow_sum$mrt_station == "中山" |
                                   flow_sum$mrt_station == "松山" |
                                   flow_sum$mrt_station == "大橋頭" |
                                   flow_sum$mrt_station == "古亭" |
                                   flow_sum$mrt_station == "北門" |
                                   flow_sum$mrt_station == "士林" |
                                   flow_sum$mrt_station == "北投", , ]
  
  # change time format
  #flow_month$hour <- as.character(flow_month$hour)
  #flow_month$hour[flow_month$hour < 10] <- paste0("0",flow_month$hour)
  flow_month$datetime <- paste0(paste(flow_month$date, flow_month$hour, sep=" "),":00:00")
  flow_month$datetime <- as.POSIXct(flow_month$datetime, format="%Y-%m-%d %H:%M:%S")
  flow_month <- flow_month[,c(5,3,4)]
  
  # flow data one month
  head(flow_month)
  tail(flow_month)
  dim(flow_month)
  
  flow_data <- rbind(flow_data, flow_month)
  
}

# final flow data
head(flow_data)
tail(flow_data)
dim(flow_data) # 4557, 3 (1 var: y var) (only 2024/03, 7 stations) # 80591, 3 (1.5 2022/6-2023/11)

# check
library(lubridate)
date <- substr(as.character(flow_data$datetime),1,10)
table(date) # 10-21 hrs a day

# write the data (New Data1)
write.csv(flow_data,"NewData/MRTFlow.csv", row.names = F)

## 2.MRTDist ----------------------------------------------------------------------------------------------

# read the data (Raw Data2)
station_csv <- read.csv("RawData2/northern-taiwan.csv")
head(station_csv)
colnames(station_csv)

# remove columns
station_info <- station_csv[,c(3,8,9)]
names(station_info) <- c("mrt_station","latitude","longitude")
head(station_info)
colnames(station_info)

# extract 中山、松山、大同（大橋頭）、古亭、萬華（中山國小）、士林、陽明
station_data <- station_info[station_info$mrt_station == "中山" |
                               station_info$mrt_station == "松山" |
                               station_info$mrt_station == "大橋頭" |
                               station_info$mrt_station == "古亭" |
                               station_info$mrt_station == "北門" |
                               station_info$mrt_station == "士林" |
                               station_info$mrt_station == "北投" , ]

head(station_data)
dim(station_data) # 7, 3 (2 vars) (only 7 stations)

# calculate distances

library(geosphere)

mrt_list <- read.table("RawData8/MRT_station.txt", header = TRUE, sep = ",")
mrt_target <- c("北投", "大橋頭", "松山", "古亭", "北門", "中山", "士林")

mrt_station <- mrt_list[mrt_list$station_name_tw %in% mrt_target,c(1,3,8,9)] #& mrt_list$station_code %in% mrt_target_code
mrt_list <- mrt_list[,c(3,8,9)]

# the closet distance of the MRT nearby
mrt_station <- mrt_station[-c(6,8),]
next_distance <- array(dim = c(7,1))
for(i in 1:nrow(mrt_station)){
  x <- mrt_station[i,4]
  y <- mrt_station[i,3]
  min_d = 1000000000
  for(j in 1:nrow(mrt_list)){
    x1 <- unlist(mrt_list[j,3])
    y1 <- unlist(mrt_list[j,2])
    d = distm(c(x1, y1), c(x, y), fun = distGeo)
    if(d < min_d & d != 0){
      next_distance[i] <- d[1]
      min_d = d[1]
    }
  }
}

station_data <- data.frame(mrt_station$station_name_tw, next_distance)
colnames(station_data) <- c("mrt_station","next_dist")

# final mrt distance data
station_data

# write the data (New Data2)
write.csv(station_data,"NewData/MRTDist.csv", row.names = F)

## 3.BusCnt (option1) ----------------------------------------------------------------------------------------

# read the data (Raw Data3)
bus_csv <- read.csv("RawData3/車站出口公車資訊.csv", fileEncoding = "big5")
head(bus_csv)
dim(bus_csv)

# change column names
colnames(bus_csv) <- c("id", "mrt_station", "exit", "bus_cnt")

# remove duplicates
bus_csv <- bus_csv[,c(2,4)]
bus_csv <- bus_csv[!duplicated(bus_csv),]
dim(bus_csv)

# change station names
bus_csv$mrt_station <- sub(".*_", "", bus_csv$mrt_station)

# count numbers of buses
library(dplyr)
bus_info <- group_by(bus_csv, mrt_station)%>%
  summarise(bus_cnt=length(bus_cnt))
head(bus_info)
dim(bus_info)

# extract 中山、松山、大同（大橋頭）、古亭、萬華（北門）、士林、陽明（北投）
bus_data <- bus_info[bus_info$mrt_station == "中山" |
                     bus_info$mrt_station == "松山" |
                     bus_info$mrt_station == "大橋頭" |
                     bus_info$mrt_station == "古亭" |
                     bus_info$mrt_station == "北門" |
                     bus_info$mrt_station == "士林" |
                     bus_info$mrt_station == "北投" , ]

# final bus data
head(bus_data)
dim(bus_data) # 7, 2 (1 var) (only 7 stations)

# write the data (New Data3)
write.csv(bus_data,"NewData/BusCnt.csv", row.names = F)

## 3.BusCnt (option2) ----------------------------------------------------------------------------------------

library(dplyr)
library(rjson)
library(geosphere)

bus_list <- read.table("RawData3/車站出口公車資訊.csv", header = TRUE, sep = ",", fileEncoding="big5")
mrt_list <- read.table("RawData8/MRT_station.txt", header = TRUE, sep = ",")
mrt_target <- c("北投", "大橋頭", "松山", "古亭", "北門", "中山", "士林")
k <- substr(bus_list[1,2], ",*", "_")

mrt_station <- mrt_list[mrt_list$station_name_tw %in% mrt_target,c(1,3,8,9)] #& mrt_list$station_code %in% mrt_target_code
mrt_list <- mrt_list[,c(3,8,9)]

bus_list <- bus_list %>% mutate("station_code"=sub("_.*","", bus_list[,2]))
bus_list <- bus_list %>% mutate("station_name"=sub(".*_","", bus_list[,2]))
bus_list <- bus_list[-2]
bus_list <- bus_list[bus_list$station_name %in% mrt_target,]

# the number of bus route nearby the MRT station 
bus_route_list <- aggregate(data=bus_list, 公車名稱~station_name, function(x) length(unique(x)))
names(bus_route_list)[2] <- "bus_cnt"
names(bus_route_list)[1] <- "mrt_station"

# final bus data
bus_route_list

# write the data (NewData3)
write.csv(bus_route_list,"NewData/BusCnt.csv", row.names = F)

## Revised Data6 ---------------------------------------------------------------------------------------------

# read the data (Raw Data6)
#bike_csv <- read.csv("YouBike.csv")
#head(bike_csv)

library(data.table)
bike_ym <- c("202206","202207","202208","202209","202210","202211",
             "202212","202301","202302","202303","202304","202305",
             "202306","202307","202308","202309","202310","202311")

ym <- bike_ym[1:2]

bike_flow_data <- NULL
for(ym in bike_ym){
  #for (url in bike_url){
  
  bike_csv <- fread(paste0("RawData6/",ym,"_YouBike2.0票證刷卡資料.csv"))
  # bike_csv <- fread(url)
  head(bike_csv)
  dim(bike_csv)
  
  bike_csv <- bike_csv[,c(1,2,4)]
  
  # change column names
  colnames(bike_csv) <- c("datetime", "bike_from", "bike_to")
  colnames(bike_csv)
  
  # change time format
  bike_csv$datetime <- as.POSIXct(bike_csv$datetime, format="%Y-%m-%d %H:%M:%S")
  
  # count total numbers of flows
  library(dplyr)
  bike_from <- group_by(bike_csv, datetime, bike_from)%>%
    summarise(bike_flow=length(bike_to))
  colnames(bike_from)[2] <- "bike_station"
  head(bike_from)
  dim(bike_from)
  
  bike_to <- group_by(bike_csv, datetime, bike_to)%>%
    summarise(bike_flow=length(bike_to))
  colnames(bike_to)[2] <- "bike_station"
  head(bike_to)
  dim(bike_to)
  
  bike_month <- group_by(rbind(bike_from,bike_to), datetime, bike_station)%>%
    summarise(bike_flow=sum(bike_flow))
  head(bike_month)
  dim(bike_month)
  
  # bike flow data one month
  head(bike_month)
  tail(bike_month)
  dim(bike_month)
  
  bike_flow_data <- rbind(bike_flow_data, bike_month)
  
}

# final bike flow data
head(bike_flow_data)
tail(bike_flow_data)
dim(bike_flow_data) #  # 749485, 3 (only 2023/11, all stations) # 12769645, 3

# check
library(lubridate)
date <- substr(as.character(bike_flow_data$datetime),1,10)
table(date)
table(bike_flow_data$datetime)

# write the data (Revised Data6)
write.csv(bike_flow_data,"RevisedData6/202311_youbike.csv", row.names = F)

## 4.YoubikeCnt ----------------------------------------------------------------------------------------------
## 5.YoubikeFlow ---------------------------------------------------------------------------------------------

library(rjson)
library(geosphere)

mrt_list <- read.table("RawData8/MRT_station.txt", header = TRUE, sep = ",")
mrt_target <- c("北投", "大橋頭", "松山", "古亭", "北門", "中山", "士林")

mrt_station <- mrt_list[mrt_list$station_name_tw %in% mrt_target,c(1,3,8,9)] #& mrt_list$station_code %in% mrt_target_code
mrt_list <- mrt_list[,c(3,8,9)]

bike_data <- fromJSON(file="RawData7/Youbike.json")
bike_station <- do.call(rbind, bike_data)

bike_station <- lapply(bike_data, function(x) c(as.vector(x[[4]]),as.vector(x[[5]])))
bike_station <- do.call(rbind, bike_station)
bike_station <- as.data.frame(bike_station)
bike_station$Zh_tw <- sub(".*_","", bike_station$Zh_tw)
bike_station <- bike_station[-5]
bike_station <- bike_station[-2]

# You-bike stations in 700m from the MRT (bike_station_list)
mrt_station <- mrt_station[-c(6,8),]
bike_station_list <- array(dim = c(7, 35))
for(i in 1:nrow(mrt_station)){
  x <- mrt_station[i,4]
  y <- mrt_station[i,3]
  c = 1
  for(j in  1:nrow(bike_station)){
    x1 <- unlist(bike_station[j,2])
    y1 <- unlist(bike_station[j,3])
    d = distm(c(x1, y1), c(x, y), fun = distGeo)
    if(d < 700){
      bike_station_list[i,c] <- bike_station[j,1]
      c = c + 1
      
    }
  }
}
bike_station_list

# merge with bike flow data
bike_csv <- fread("RevisedData6/202311_youbike.csv")
head(bike_csv)

merge_data <- NULL
for(i in 1:7){
  station <- bike_csv[bike_csv$bike_station %in% bike_station_list[i,],]
  station$mrt_station <- mrt_station$station_name_tw[i]
  merge_data <- rbind(merge_data, station)
}
head(merge_data)
tail(merge_data)
dim(merge_data)

# count bike and flow
library(dplyr)
bike_cnt_data <- group_by(merge_data[,-1][!duplicated(merge_data[,-c(1,3)]),], mrt_station)%>%
  summarise(bike_cnt=length(bike_station))

bike_flow_data <- group_by(merge_data, datetime, mrt_station)%>%
  summarise(bike_flow=sum(bike_flow))

# final bike cnt data
head(bike_cnt_data)
tail(bike_cnt_data)
dim(bike_cnt_data)

# write the data (NewData4)
write.csv(bike_cnt_data,"NewData/YoubikeCnt.csv", row.names = F)

# final bike cnt data
head(bike_flow_data)
tail(bike_flow_data)
dim(bike_flow_data) # 5029, 3 # 91375, 3

# check
library(lubridate)
date <- substr(as.character(bike_flow_data$datetime),1,10)
table(date)
table(bike_flow_data$datetime)

# write the data (NewData5)
write.csv(bike_flow_data,"NewData/YoubikeFlow.csv", row.names = F)

## 6.SchoolCnt -----------------------------------------------------------------------------------------------

# create the data: 中山: 4; 松山: 2; 大同（大橋頭）: 8; 古亭: 4; 萬華（北門）:2; 士林: 3; 陽明（北投）: 4
mrt_station <- c("中山","北投","北門","古亭","士林","大橋頭","松山")
school_cnt <- c(4,4,2,4,3,8,2)
school_data <- data.frame(mrt_station, school_cnt)

# final school data
head(school_data)
tail(school_data)
dim(school_data) #  # 7, 2 (only 7 stations)

# write the data (New Data6)
write.csv(school_data,"NewData/SchoolCnt.csv", row.names = F)

## 7.Weather -------------------------------------------------------------------------------------------------

# read the data (Raw Data4)
library(rjson)

# 2022/6 -2023/5
weather_json <- fromJSON(file = "RawData4//weather.json")
weather_location <- weather_json[["cwbdata"]][["resources"]][["resource"]][["data"]][["surfaceObs"]][["location"]]

# check station's info
for (i in 1:length(weather_location)){
  print(i); print(weather_location[[i]][["station"]])
} 
# -> New Taipei: 2, TAMSUI: 3, TAIPEI: 5

# extract TAIPEI's data
weather_taipei <- weather_location[[5]][["stationObsTimes"]][["stationObsTime"]]
#weather_taipei[[1]]["datetime"]; weather_taipei[[1]][["weatherElements"]] # check

# 2023/4 - 2024/3
weather_json <- fromJSON(file = "RawData4/C-B0024-002.json")
weather_location <- weather_json[["cwaopendata"]][["resources"]][["resource"]][["data"]][["surfaceObs"]][["location"]]

# check station's info
for (i in 1:length(weather_location)){
  print(i); print(weather_location[[i]][["station"]])
} 
# -> New Taipei: 1, TAMSUI: 2, TAIPEI: 4

# extract TAIPEI's data
weather_taipei <- weather_location[[4]][["stationObsTimes"]][["stationObsTime"]]
#weather_taipei[[1]]["datetime"]; weather_taipei[[1]][["weatherElements"]] # check

##

# fill null value with 0
for (i in 1:length(weather_taipei)){
  if (is.null(weather_taipei[[i]]$weatherElements$SunshineDuration)){
    weather_taipei[[i]]$weatherElements$SunshineDuration = 0}
}

# convert into dataframe format
col_num <- length(unlist(weather_taipei[[1]]))
weather_data <- as.data.frame(matrix(unlist(weather_taipei), byrow = T, ncol = col_num))
names(weather_data)

# change column names
names(weather_data) <- names(unlist(weather_taipei)[1:col_num])
names(weather_data) <- c("datetime","air_pressure","air_temperature","relative_humidity",
                          "wind_speed","wind_direction","precipitation","sunshine_duration")
names(weather_data)

# remove columns
weather_data <- weather_data[,c(1:4,7:8)]

# change time format
weather_data$datetime <- substr(gsub("T", " ", weather_data$datetime), 1, 19)
weather_data$datetime <- as.POSIXct(weather_data$datetime, format="%Y-%m-%d %H:%M:%S")

# other data cleaning
weather_data$precipitation[weather_data$precipitation == "T"] <- 0.1
weather_data$sunshine_duration <- as.double(weather_data$sunshine_duration)

# final weather data
head(weather_data)
tail(weather_data)
dim(weather_data) # 8808, 6 (5 vars, 2023/04/19-2024/04/19)

##

weather_data1 <- weather_data
weather_data2 <- weather_data
weather_data <- rbind(weather_data1,weather_data2)
weather_data <- weather_data[!duplicated(weather_data),]

# final weather data
head(weather_data)
tail(weather_data)
dim(weather_data) # 8808, 6 (5 vars, 2023/04/19-2024/04/19) # 17088, 6 (2022/5-2024/4)

# write the data (New Data7)
write.csv(weather_data,"NewData/Weather2.csv", row.names = F)

## 8.Air Quality ---------------------------------------------------------------------------------------------

# read the data (Raw Data5)
air_csv <- read.csv("RawData5/空氣品質指標(AQI)(歷史資料) (2022-06).csv")
for(m in 7:12){
  if(m < 10){
    air_csv <- rbind(air_csv, read.csv(paste0("RawData5/空氣品質指標(AQI)(歷史資料) (2022-0",m,").csv")))
  }
  else{
    air_csv <- rbind(air_csv, read.csv(paste0("RawData5/空氣品質指標(AQI)(歷史資料) (2022-",m,").csv")))
  }
}
for(m in 1:11){
  if(m < 10){
    d <- read.csv(paste0("RawData5/空氣品質指標(AQI)(歷史資料) (2023-0",m,").csv"))
  }
  else{
    d <- read.csv(paste0("RawData5/空氣品質指標(AQI)(歷史資料) (2023-",m,").csv"))
  }
  colnames(d) <- gsub("X.", "", colnames(d))
  colnames(d) <- sub("\\..*", "", colnames(d))
  colnames(d)[11] <- "pm2.5"
  colnames(d)[20] <- "pm2.5_avg"
  air_csv <- rbind(air_csv, d)
}
head(air_csv)
tail(air_csv)
dim(air_csv)

# extract TAIPEI's data
air_taipei <- air_csv[air_csv$county == "臺北市",]
head(air_taipei)
dim(air_taipei)

# remove columns / change column orders
air_data <- cbind(air_taipei[,17], air_taipei[,-c(2,4,17,18,23:25)])
head(air_data)
dim(air_data)

# change column names
colnames(air_data)[1] <- "datetime"
colnames(air_data)[2] <- "mrt_station"
colnames(air_data)

# change station name: 中山、松山、大同（大橋頭）、古亭、萬華（北門）、士林、陽明（北投）
air_data[air_data$mrt_station == "大同",]$mrt_station <- "大橋頭"
air_data[air_data$mrt_station == "萬華",]$mrt_station <- "北門"
air_data[air_data$mrt_station == "陽明",]$mrt_station <- "北投"

# change time format
air_data$datetime <- as.character(air_data$datetime)
air_data$datetime <- paste0(air_data$datetime,":00")
air_data$datetime <- as.POSIXct(air_data$datetime, format="%Y-%m-%d %H:%M:%S")

# final air quality data
head(air_data)
tail(air_data)
dim(air_data) # 60782, 19 (17 vars, 2023/04-2024/03) # 91315, 19 (2022/6 - 2023/11)

# check
library(lubridate)
date <- substr(as.character(air_data$datetime),1,10)
table(date)

# write the data (New Data8)
write.csv(air_data,"NewData/AirQuality.csv", row.names = F)

## ------------------------------------------------------------------------------------------------------------------------












## -----------------------------------------------------------------------------------------------------------

# read json data
library(rjson)
station_json <- fromJSON(file = "MRT/TpeMrtStations_TWD97_FIDCODE.json")

# convert into dataframe format
col_num_ <- length(unlist(station_json[["features"]][1]))
station_info <- as.data.frame(matrix(unlist(station_json[["features"]]), byrow = T, ncol = col_num_))
names(station_info)

# change column names
names(station_info) <- names(unlist(station_json[["features"]][1][1:col_num_]))
names(station_info) <- gsub("geometry.", "", names(station_info))
names(station_info) <- gsub("properties.", "", names(station_info))
names(station_info)[7] <- "mrt_station"
names(station_info)

# change column orders
station_info <- station_info[,c(7,4,5)]

# remove duplicates
station_info <- station_info[!duplicated(station_info),]
dim(station_info)

# change station names
station_info$mrt_station <- gsub("站", "", station_info$mrt_station)

# plot stations
plot(station_info$coordinates1, station_info$coordinates2)

# plot mrt and air station 
station_info

station_info$type <- 1
station = c("a","b","c","d","e","f","g")   
coordinates1 <- c(303118.5350, 308380.8646, 301796.4547, 303449.3928, 301258.6358, 302112.4425, 303378.9799)    
coordinates2 <- c(2772788.1064, 2771440.5093, 2772888.2529, 2768164.8687, 2771024.2287, 2777323.4818, 2786121.2051)
type <- c(2, 2, 2, 2, 2 ,2, 2)
station_info2 <- rbind(station_info,cbind(station, coordinates1, coordinates2, type))

station_info2

# plot
plot(station_info2$coordinates1, station_info2$coordinates2, col=station_info2$type)

# 中山 松山 大同（大橋頭） 古亭 萬華（中山國小） 士林 陽明
# 303118.5350, 308380.8646, 301796.4547, 303449.3928, 301258.6358, 302112.4425, 303378.9799
# 2772788.1064, 2771440.5093, 2772888.2529, 2768164.8687, 2771024.2287, 2777323.4818, 2786121.2051