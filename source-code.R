library(caret)
library(maptools)
library(reshape2)
library(rgdal)
library(rgeos)
library(sp)
library(tidyverse)


# Pre-processing: Community areas
areas_map <-
  readOGR(paste0("Data Sources/Boundaries - Community Areas (current)/",
                 str_subset(dir("Boundaries - Community Areas (current)"),
                            "shp$")))
areas_map@data <-
  areas_map@data %>%
  rename(area_name = community,
         area_id = area_numbe)
areas_map_ggplot <- fortify(areas_map, region = "area_id")

# Pre-processing: Traffic region map
traffic_map <-
  read.csv(paste0("Data Sources/",
                  "Chicago_Traffic_Tracker_-_",
                  "Congestion_Estimates_by_Regions.csv"),
           header = TRUE)
for(i in 1:29){
  traffic_map$polygonMatrix[i] <-
    list(Polygons(list(Polygon(matrix(c(traffic_map$WEST[i],
                                        traffic_map$SOUTH[i],
                                        traffic_map$WEST[i],
                                        traffic_map$NORTH[i],
                                        traffic_map$EAST[i],
                                        traffic_map$NORTH[i],
                                        traffic_map$EAST[i],
                                        traffic_map$SOUTH[i],
                                        traffic_map$WEST[i],
                                        traffic_map$SOUTH[i]),
                                      ncol = 2, byrow = TRUE))),
                  ID = traffic_map$REGION[i]))
}
traffic_map <-
  SpatialPolygonsDataFrame(SpatialPolygons(traffic_map$polygonMatrix),
                           data.frame(region_name = traffic_map$REGION,
                                      region_id = traffic_map$REGION_ID,
                                      row.names = traffic_map$REGION))
proj4string(traffic_map) <- proj4string(areas_map)
traffic_map_ggplot <- fortify(traffic_map, region = "region_id")

# Pre-processing: Weather
weather <-
  read.csv("Data Sources/weather.csv", header = TRUE) %>%
  filter(STATION == "USW00014819") %>%
  select(DATE, PRCP, TMIN, TMAX) %>%
  rename(date = DATE, prcp = PRCP, tmin = TMIN, tmax = TMAX)
weather$date <- as.Date(weather$date, "%Y-%m-%d")

# Pre-processing: Census
census <-
  read.csv("Data Sources/ReferenceCCA20112015.csv", header = TRUE) %>%
  mutate(area_name = str_to_upper(GEOG)) %>%
  mutate(area_name = ifelse(area_name == "THE LOOP", "LOOP", area_name)) %>%
  mutate(area_name = ifelse(area_name == "O'HARE", "OHARE", area_name)) %>%
  mutate(area_name = as.factor(area_name)) %>%
  mutate(prop_drive_alone = DROVE_AL / TOT_COMM) %>%
  mutate(prop_carpool = CARPOOL / TOT_COMM) %>%
  mutate(prop_transit = TRANSIT / TOT_COMM) %>%
  mutate(prop_walk_bike = WALK_BIKE / TOT_COMM) %>%
  left_join(areas_map@data, by = "area_name") %>%
  mutate(area_id = as.factor(area_id)) %>%
  rename(pop2010 = X2010_POP,
         med_age = MED_AGE,
         med_inc = MEDINC,
         avg_vmt = AVG_VMT) %>%
  select(area_name, area_id, pop2010, med_age, med_inc, prop_drive_alone,
         prop_carpool, prop_transit, prop_walk_bike, avg_vmt) %>%
  left_join(data.frame(id = areas_map@data$area_id,
                       as.data.frame(coordinates(areas_map)) %>%
                         rename(longitude = V1, latitude = V2)),
            by = c("area_id" = "id"))
census_plot <- melt(census, id.vars = c("area_name", "area_id", "pop2010",
                                        "med_age", "med_inc", "avg_vmt",
                                        "longitude", "latitude")) %>%
  mutate(variable = ifelse(variable == "prop_drive_alone",
                           "Drive alone",
                           ifelse(variable == "prop_carpool",
                                  "Carpool",
                                  ifelse(variable == "prop_transit",
                                         "Public transit",
                                         ifelse(variable == "prop_walk_bike",
                                                "Walk/bike",
                                                variable)))))

# Pre-processing: 'L' station map
l_map <-
  read.csv(paste0("Data Sources/",
                  "CTA_-_System_Information_-_List_of__L__Stops_-_Map.csv"),
           header = TRUE) %>%
  mutate(x = as.numeric(str_remove(sapply(str_split(Location, ", "),
                                          "[", 1), "\\("))) %>%
  mutate(y = as.numeric(str_remove(sapply(str_split(Location, ", "),
                                          "[", 2), "\\)"))) %>%
  mutate(lat = x) %>%
  mutate(long = y)
coordinates(l_map) <- ~ y + x
proj4string(l_map) <- proj4string(areas_map)
l_map$area_name <- over(l_map, areas_map)$area_name
l_map$area_id <- over(l_map, areas_map)$area_id
l_stops <- l_map@data %>%
  group_by(MAP_ID, area_name, area_id) %>%
  summarize()

# Pre-processing: 'L' ridership
l_rides <-
  read.csv(paste0("Data Sources/",
                  "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.csv"),
           header = TRUE) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  rename(day_type = daytype) %>%
  filter(rides > 0) %>%
  left_join(l_stops, by = c("station_id" = "MAP_ID")) %>%
  group_by(area_name, area_id, date, day_type) %>%
  summarize(l_total_stops = n(),
            l_total_rides = sum(rides)) %>%
  filter(!is.na(area_name)) %>%
  left_join(census, by = c("area_name", "area_id")) %>%
  mutate(l_norm_rides = (l_total_rides / l_total_stops) / pop2010) %>%
  select(area_name, area_id, date, day_type, l_total_rides, l_norm_rides) %>%
  left_join(data.frame(id = areas_map@data$area_id,
                       as.data.frame(coordinates(areas_map)) %>%
                         rename(longitude = V1, latitude = V2)),
            by = c("area_id" = "id")) 

# Pre-processing: Day type
day_type_ref <-
  l_rides[, c("date", "day_type")] %>%
  group_by(date, day_type) %>%
  summarize()

# Pre-processing: Bus station map
bus_map <-
  readOGR(paste0("Data Sources/CTA Bus Stops/",
                 str_subset(dir("CTA Bus Stops"), "shp$")))
bus_map@data$area_name <- over(bus_map, areas_map)$area_name
bus_map@data$area_id <- over(bus_map, areas_map)$area_id
bus_stops <-
  over(bus_map, areas_map) %>%
  mutate(systemstop = bus_map@data$systemstop) %>%
  mutate(objectid = bus_map@data$objectid) %>%
  mutate(public_nam = bus_map@data$public_nam) %>% 
  mutate(routesstpg = bus_map@data$routesstpg) %>%
  mutate(dir = bus_map@data$dir) %>%
  mutate(point_x = bus_map@data$point_x) %>%
  mutate(point_y = bus_map@data$point_y) %>%
  mutate(route = strsplit(as.character(routesstpg), ",")) %>%
  unnest(route) %>%
  mutate(route = as.factor(route)) %>%
  group_by(area_name, area_id, route) %>%
  summarize(total_stops = n()) %>%
  mutate(prop_stops = total_stops / sum(total_stops))

# Pre-processing: Bus ridership
bus_rides <-
  read.csv(paste0("Data Sources/",
                  "CTA_-_Ridership_-_Bus_Routes_-_Daily_Totals_by_Route.csv"),
           header = TRUE) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  rename(day_type = daytype) %>%
  # Filter out routes that do not exist at present or that started after 2010
  filter(!route %in% c("1002", "111A", "115", "122", "123", "127", "129", "137",
                       "14", "144", "145", "154", "168", "17", "170", "173",
                       "174", "200", "202", "203", "204", "25", "27", "290",
                       "290S", "31", "33", "37", "38", "40", "49A", "53AL",
                       "56A", "64", "69", "69BR", "90N", "95", "992", "999",
                       "J14", "N201", "R22", "R39", "R55", "R63", "R69", "R79",
                       "R87", "R95", "X20", "X21", "X28", "X3", "X4", "X49",
                       "X54", "X55", "X80", "X9", "X99")) %>%
  left_join(bus_stops, by = "route") %>%
  mutate(bus_norm_rides = rides * prop_stops) %>%
  filter(!is.na(area_name)) %>%
  group_by(date, day_type, area_name, area_id) %>%
  summarize(bus_total_rides = sum(rides),
            bus_norm_rides = sum(bus_norm_rides)) %>%
  left_join(census, by = c("area_name", "area_id")) %>%
  mutate(bus_norm_rides = bus_norm_rides / pop2010) %>%
  select(area_name, area_id, date, day_type, bus_total_rides,
         bus_norm_rides) %>%
  left_join(data.frame(id = areas_map@data$area_id,
                       as.data.frame(coordinates(areas_map)) %>%
                         rename(longitude = V1, latitude = V2)),
            by = c("area_id" = "id")) 

# Pre-processing: Taxi dropoffs
taxi_dropoffs <-
  read.csv("Data Sources/taxi-dropoffs.csv", header = TRUE) %>%
  rename(area_id = Dropoff.Community.Area,
         date = Ride.Date,
         taxi_avg_ride_len = Average.Ride.Length,
         taxi_total_dropoffs = Total.Number.of.Rides) %>%
  mutate(area_id = as.factor(area_id)) %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  left_join(census, by = "area_id") %>%
  mutate(taxi_norm_dropoffs = taxi_total_dropoffs / pop2010) %>%
  select(area_name, area_id, date, taxi_total_dropoffs, taxi_norm_dropoffs,
         taxi_avg_ride_len)

# Pre-processing: Taxi pickups
taxi_pickups <-
  read.csv("Data Sources/taxi-pickups.csv", header = TRUE) %>%
  rename(area_id = Pickup.Community.Area,
         date = Ride.Date,
         taxi_total_pickups = Total.Number.of.Rides) %>%
  mutate(area_id = as.factor(area_id)) %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  left_join(census, by = "area_id") %>%
  mutate(taxi_norm_pickups = taxi_total_pickups / pop2010) %>%
  select(area_name, area_id, date, taxi_total_pickups, taxi_norm_pickups)

# Pre-processing: Traffic congestion
traffic_areas_join <- read.csv("Data Sources/key-traffic-join.csv",
                               header = TRUE)
if(!("Chicago_Traffic_Tracker_-_Historical_Congestion_Estimates_by_Region.csv"
     %in% dir("Data Sources"))) {
  unzip(paste0("Data Sources/Chicago_Traffic_Tracker_-_",
               "Historical_Congestion_Estimates_by_Region.csv.zip"),
        exdir = "Data Sources")
}
traffic <-
  read.csv(paste0("Data Sources/", "Chicago_Traffic_Tracker_-_",
                  "Historical_Congestion_Estimates_by_Region.csv"),
           header = TRUE) %>%
  mutate(time = as.POSIXct(strptime(TIME, "%m/%d/%Y %I:%M:%S %p"))) %>%
  mutate(date = as.Date(time)) %>%
  mutate(month = format(date, "%m")) %>%
  mutate(hour = format(time, "%H")) %>%
  rename(speed = SPEED,
         region_id = REGION_ID) %>%
  left_join(day_type_ref, by = "date") %>%
  left_join(traffic_map@data, by = "region_id") %>%
  filter(BUS.COUNT >= 5, NUMBER.OF.READS >= 30, !is.na(day_type))

# Pre-processing: Aggregate traffic data to peak congestion per day
traffic_agg <-
  traffic %>%
  group_by(region_name, region_id, date, day_type, month) %>%
  summarize(speed_min = min(speed)) %>%
  left_join(weather, by = "date")
traffic_areas <- traffic_agg %>%
  left_join(traffic_areas_join, by = c("region_name" = "RAW")) %>%
  mutate(area_name = strsplit(str_to_upper(CORRECTED), "-")) %>%
  unnest(area_name) %>%
  left_join(areas_map@data, by = "area_name") %>%
  select(region_name, region_id, area_name, area_id, date, day_type,
         month, speed_min)
traffic_areas$area_name <- as.factor(traffic_areas$area_name)

# Pre-processing: Filter traffic data to valid observations for modeling
traffic_filt <-
  traffic %>%
  group_by(region_id, time) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  mutate(valid = TRUE) %>%
  select(-count)
traffic_model <-
  traffic %>%
  left_join(traffic_filt, by = c("region_id", "time")) %>%
  filter(valid == TRUE) %>%
  mutate(region_id = as.factor(region_id)) %>%
  mutate(hour = as.numeric(hour)) %>%
  filter(time < "2015-02-01 00:00:00") %>%
  select(region_name, region_id, time, hour, date, day_type, month, speed) 

# Pre-processing: Merge all pre-processed datasets
transit <-
  traffic_areas %>%
  left_join(weather, by = "date") %>%
  left_join(l_rides, by = c("area_name", "area_id", "date", "day_type")) %>%
  left_join(bus_rides, by = c("area_name", "area_id", "date", "day_type")) %>%
  left_join(taxi_dropoffs, by = c("area_name", "area_id", "date")) %>%
  left_join(taxi_pickups, by = c("area_name", "area_id", "date")) %>%
  select(-longitude.x, -latitude.x, -longitude.y, -latitude.y)
transit$taxi_total_dropoffs[is.na(transit$taxi_total_dropoffs)] <- 0
transit$taxi_norm_dropoffs[is.na(transit$taxi_norm_dropoffs)] <- 0
transit$taxi_avg_ride_len[is.na(transit$taxi_avg_ride_len)] <- 0
transit$taxi_total_pickups[is.na(transit$taxi_total_pickups)] <- 0
transit$taxi_norm_pickups[is.na(transit$taxi_norm_pickups)] <- 0

# Modeling
set.seed(101) 
inTrain = createDataPartition(traffic_model$speed, 1, 0.8, FALSE)
traffic_train = traffic_model[inTrain, ]
traffic_test = traffic_model[-inTrain, ]
traffic_valid = read.csv("Data Sources/validation.csv", header = TRUE) %>%
  mutate(time = as.POSIXct(strptime(last_updated, "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(hour = as.numeric(format(time, "%H"))) %>%
  mutate(region_id = as.factor(region_id)) %>%
  mutate(day_type = ifelse(format(time, "%u") < 6, "W",
                          ifelse(format(time, "%u") == 6, "A", "U"))) %>%
  rename(speed = current_speed) %>%
  select(-last_updated)
model_slr <- lm(speed ~ region_id + hour + day_type, traffic_train)
model_slr_int <- lm(speed ~ region_id * hour * day_type, traffic_train) 
trCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model_knn <- train(speed ~ region_id + hour + day_type, traffic_train,
                   method = "knn",
                   trControl = trCtrl,
                   preProcess = c("center", "scale"),
                   tuneLength = 10
)
traffic_train$modeled_slr <- predict(model_slr)
traffic_train$modeled_slr_int <- predict(model_slr_int)
traffic_train$modeled_knn <- predict(model_knn)
traffic_test$pred_slr <- predict(model_slr, newdata = traffic_test)
traffic_test$pred_slr_int <- predict(model_slr_int, newdata = traffic_test)
traffic_test$pred_knn <- predict(model_knn, newdata = traffic_test)
traffic_valid$pred_slr <- predict(model_slr, newdata = traffic_valid)
traffic_valid$pred_slr_int <- predict(model_slr_int, newdata = traffic_valid)
traffic_valid$pred_knn <- predict(model_knn, newdata = traffic_valid)

# Identify top 10 areas for each response
l_total_top10 <-
  unique(rbind(head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(l_total_rides)) %>%
                      filter(day_type == "A") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit,area_name, area_id, day_type),
                              mean = mean(l_total_rides)) %>%
                      filter(day_type == "W") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(l_total_rides)) %>%
                      filter(day_type == "U") %>%
                      arrange(desc(mean)), 10))$area_name)
l_norm_top10 <-
  unique(rbind(head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(l_norm_rides)) %>%
                      filter(day_type == "A") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit,area_name, area_id, day_type),
                              mean = mean(l_norm_rides)) %>%
                      filter(day_type == "W") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(l_norm_rides)) %>%
                      filter(day_type == "U") %>%
                      arrange(desc(mean)), 10))$area_name)

bus_total_top10 <-
  unique(rbind(head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(bus_total_rides)) %>%
                      filter(day_type == "A") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit,area_name, area_id, day_type),
                              mean = mean(bus_total_rides)) %>%
                      filter(day_type == "W") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(bus_total_rides)) %>%
                      filter(day_type == "U") %>%
                      arrange(desc(mean)), 10))$area_name)
bus_norm_top10 <-
  unique(rbind(head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(bus_norm_rides)) %>%
                      filter(day_type == "A") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit,area_name, area_id, day_type),
                              mean = mean(bus_norm_rides)) %>%
                      filter(day_type == "W") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(bus_norm_rides)) %>%
                      filter(day_type == "U") %>%
                      arrange(desc(mean)), 10))$area_name)
taxi_norm_dropoffs_top10 <-
  unique(rbind(head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(taxi_norm_dropoffs)) %>%
                      filter(day_type == "A") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit,area_name, area_id, day_type),
                              mean = mean(taxi_norm_dropoffs)) %>%
                      filter(day_type == "W") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(taxi_norm_dropoffs)) %>%
                      filter(day_type == "U") %>%
                      arrange(desc(mean)), 10))$area_name)
taxi_norm_pickups_top10 <-
  unique(rbind(head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(taxi_norm_pickups)) %>%
                      filter(day_type == "A") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit,area_name, area_id, day_type),
                              mean = mean(taxi_norm_pickups)) %>%
                      filter(day_type == "W") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(taxi_norm_pickups)) %>%
                      filter(day_type == "U") %>%
                      arrange(desc(mean)), 10))$area_name)

taxi_avg_ride_len_top10 <-
  unique(rbind(head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(taxi_avg_ride_len)) %>%
                      filter(day_type == "A") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit,area_name, area_id, day_type),
                              mean = mean(taxi_avg_ride_len)) %>%
                      filter(day_type == "W") %>%
                      arrange(desc(mean)), 10),
               head(summarize(group_by(transit, area_name, area_id, day_type),
                              mean = mean(taxi_avg_ride_len)) %>%
                      filter(day_type == "U") %>%
                      arrange(desc(mean)), 10))$area_name)
traffic_speed_top10 <-
  unique(rbind(head(summarize(group_by(traffic_agg, region_name, region_id,
                                       day_type),
                              mean = mean(speed_min)) %>%
                      filter(day_type == "A") %>%
                      arrange(mean), 10),
               head(summarize(group_by(traffic_agg, region_name, region_id,
                                       day_type),
                              mean = mean(speed_min)) %>%
                      filter(day_type == "W") %>%
                      arrange(mean), 10),
               head(summarize(group_by(traffic_agg, region_name, region_id,
                                       day_type),
                              mean = mean(speed_min)) %>%
                      filter(day_type == "U") %>%
                      arrange(mean), 10))$region_name)
l_norm_top10 <- as.character(l_norm_top10)[!as.character(l_norm_top10) %in%
                                             c("DOUGLAS", "WASHINGTON PARK")]
l_total_top10 <- as.character(l_total_top10)
bus_norm_top10 <- as.character(bus_norm_top10)
bus_total_top10 <- as.character(bus_total_top10)
taxi_norm_dropoffs_top10 <- as.character(taxi_norm_dropoffs_top10)
taxi_norm_pickups_top10 <- as.character(taxi_norm_pickups_top10)
taxi_avg_ride_len_top10 <- as.character(taxi_avg_ride_len_top10)
traffic_speed_top10 <- as.character(traffic_speed_top10)

# Environment cleanup
rm(weather, i, l_stops, l_rides, bus_stops, bus_rides, taxi_dropoffs,
   taxi_pickups, day_type_ref, traffic, traffic_areas_join, traffic_filt,
   traffic_areas, inTrain)
save.image("chicago-traffic-transit.RData")
```