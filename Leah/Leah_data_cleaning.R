<<<<<<< HEAD
library(dplyr)
library(rjson)
library(geosphere)
bus_list <- read.table("C:\\Users\\my613\\Downloads\\Final Project\\Data\\Bus.csv", header = TRUE, sep = ",")
mrt_list <- read.table("C:\\Users\\my613\\Downloads\\Final Project\\Data\\MRT_station.txt", header = TRUE, sep = ",")
mrt_target <- c("北投", "大橋頭", "松山", "古亭", "北門", "中山", "士林")
k <- substr(bus_list[1,2], ",*", "_")

mrt_station <- mrt_list[mrt_list$station_name_tw %in% mrt_target & mrt_list$station_code %in% mrt_target_code,c(1,3,8,9)]
mrt_list <- mrt_list[,c(3,8,9)]

bus_list <- bus_list %>% mutate("station_code"=sub("_.*","", bus_list[,2]))
bus_list <- bus_list %>% mutate("station_name"=sub(".*_","", bus_list[,2]))
bus_list <- bus_list[-2]
bus_list <- bus_list[bus_list$station_name %in% mrt_target,]

bike_data <- fromJSON(file="C:\\Users\\my613\\Downloads\\Final Project\\Youbike.json")
bike_station <- do.call(rbind, bike_data)

bike_station <- lapply(bike_data, function(x) c(as.vector(x[[4]]),as.vector(x[[5]])))
bike_station <- do.call(rbind, bike_station)
bike_station <- as.data.frame(bike_station)
bike_station$Zh_tw <- sub(".*_","", bike_station$Zh_tw)
bike_station <- bike_station[-5]
bike_station <- bike_station[-2]


#You-bike stations in 700m from the MRT (bike_station_list)
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

#the closet distance of the MRT nearby
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

#the number of bus route nearby the MRT station 
bus_route_list <- aggregate(data=bus_list, 公車名稱~station_name, function(x) length(unique(x)))
names(bus_route_list)[2] <- "bus_quantity"
=======
library(dplyr)
library(rjson)
library(geosphere)
bus_list <- read.table("C:\\Users\\my613\\Downloads\\Final Project\\Data\\Bus.csv", header = TRUE, sep = ",")
mrt_list <- read.table("C:\\Users\\my613\\Downloads\\Final Project\\Data\\MRT_station.txt", header = TRUE, sep = ",")
mrt_target_code <- c("R22", "O12", "G19", "G09", "G13", "G14", "R16")
mrt_target <- c("北投", "大橋頭", "松山", "古亭", "北門", "中山", "士林")
k <- substr(bus_list[1,2], ",*", "_")

bus_list <- bus_list %>% mutate("station_code"=sub("_.*","", bus_list[,2]))
bus_list <- bus_list %>% mutate("station_name"=sub(".*_","", bus_list[,2]))
bus_list <- bus_list[-2]
bus <- subset(bus_list, bus_list$station_name %in% mrt_target)

bus_data <- fromJSON(file="C:\\Users\\my613\\Downloads\\Final Project\\GetRoute.json")
bus_route <- lapply(bus_data[[2]], function(x) as.vector(x))
bus_route <- do.call(rbind, bus_route)

bike_data <- fromJSON(file="C:\\Users\\my613\\Downloads\\Final Project\\Youbike.json")
bike_station <- do.call(rbind, bike_data)

bike_station <- lapply(bike_data, function(x) c(as.vector(x[[4]]),as.vector(x[[5]])))
bike_station <- do.call(rbind, bike_station)
bike_station <- as.data.frame(bike_station)
bike_station$Zh_tw <- sub(".*_","", bike_station$Zh_tw)
bike_station <- bike_station[-5]
bike_station <- bike_station[-2]

mrt_station <- mrt_list[mrt_list$station_name_tw %in% mrt_target & mrt_list$station_code %in% mrt_target_code,c(1,3,8,9)]
mrt_list <- mrt_list[,c(3,8,9)]

bus_station_list <- array(dim = c(7, 35))
for(i in 1:nrow(mrt_station)){
  x <- mrt_station[i,4]
  y <- mrt_station[i,3]
  c = 1
  for(j in  1:nrow(bike_station)){
    x1 <- unlist(bike_station[j,2])
    y1 <- unlist(bike_station[j,3])
    d = distm(c(x1, y1), c(x, y), fun = distGeo)
    if(d < 700){
      bus_station_list[i,c] <- bike_station[j,1]
      c = c + 1
      
    }
  }
}

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
>>>>>>> d42d4f801ce749943774144aec1ebad0c880985d
