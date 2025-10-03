library(dplyr)
library(xts)
library(lubridate)

####Merging Avistaje Tracks
####Set WD here when working on laptop####
setwd("C:/Users/Jawor/Desktop/R_repos/playbackAnalysis/2025Avistaje")
getwd()

###files to be merged can be found in folder below
myFiles = list.files(path="C:/Users/Jawor/Desktop/R_repos/playbackAnalysis/2025Avistaje", pattern="*.csv")
myFiles
data <- lapply(myFiles, read.table, sep=",", header=TRUE)
combined.data <- do.call(rbind, data)
combined.data$file_origin <- row.names(combined.data)

###Merged data will then be output to file in folder below
###will need to add Observer column, change xutm yutm and altitude to number at 4 decimals
###then set up 15MIN column as logical using excel forumla =mod(minute(T2),15)=0
head(combined.data)
write.table(combined.data, file="C:/Users/Jawor/Desktop/R_repos/playbackAnalysis/temp.csv",row.names=TRUE,col.names=TRUE,sep=",")


###NOW FILTER TRACKS and aggregate at 15 min intervals
####Set WD here when working on laptop####
#setwd("C:/Users/Kelsey/Box Sync/Dissertate/LocationCleaning&Analyses")
#getwd()

####Set WD here when working at school#####
#setwd("C:/Users/kme479/Box Sync/Dissertate/LocationCleaning&Analyses")
#getwd()

#pull in Avistaje Tracks
Tracks<-read.csv(file.choose(), header = TRUE, sep = ",", as.is = T)
head(Tracks)
names(Tracks)


realtime<-as.POSIXct(Tracks$ltimeRounded,format="%m/%d/%y %H:%M")
realtime
Tracks-cbind(Tracks, realtime)
head(Tracks15Time)


ave_y<-aggregate(y_proj ~ realtime, Tracks15Time, mean)
ave_x<-aggregate(x_proj ~ realtime, Tracks15Time, mean)
ave_alt<-aggregate(altitude ~ realtime, Tracks15Time, mean)

output<-cbind(ave_x, ave_y[,2], ave_alt[,2])
head(output)

####ALWAYS CHANGE NAME OF OUTPUT FILE####
write.table(output, file="C:/Users/Jawor/Desktop/R_repos/playbackAnalysis/averagedPoints.csv",row.names=TRUE,col.names=TRUE,sep=",")

################################################################################
################################################################################
################################################################################
####################Averaged gps points using ChatGPT###########################

library(dplyr)
library(lubridate)
library(tidyverse)
library(leaflet)
library(geosphere)
library(sf)
library(fuzzyjoin)

track_files <- list.files("C:\\Users\\Jawor\\Desktop\\R_repos\\playbackAnalysis\\2025Avistaje", pattern = "\\.csv$", full.names = TRUE)

data <- lapply(track_files, function(f) {
  df <- read.table(f, sep = ",", header = TRUE)
  av_num <- str_extract(basename(f), "AV\\d+")
  df$file_origin <- f
  df$AV_ID <- av_num
  return(df)
})
combined.data <- do.call(rbind, data)
combined.data$file_origin <- row.names(combined.data)
names(combined.data)
combined.data <- combined.data |>
  mutate(time = ymd_hms(time)) |>
  select(tident, ident, Latitude, Longitude, altitude, time, ltime, file_origin, AV_ID)

sf_points_ll <- st_as_sf(combined.data, coords = c("Longitude", "Latitude"), crs = 4326)
sf_points_utm <- st_transform(sf_points_ll, crs = 32718)
utmData <- sf_points_utm |>
  st_coordinates() |>
  as.data.frame() |>
  cbind(sf_points_utm) |>
  with_tz(time, tzone = "America/Chicago")

time_seq <- seq(floor_date(min(utmData$time), "15 minutes"), ceiling_date(max(utmData$time), "15 minutes"), by = "15 mins")

avg_window <- function(target_time, data, window = 2) {
  subset <- data |>
    filter(time >= target_time - minutes(window),
           time <= target_time + minutes(window))
  
  if (nrow(subset) > 0) {
    subset |>
      group_by(AV_ID) |>
      summarise(
        target_time = target_time,
        mean_yProj = mean(Y, na.rm = TRUE),
        mean_xProj = mean(X, na.rm = TRUE),
        n_points= n(),
        .groups = "drop"
      )
  } else {
    tibble(
      AV_ID = NA_character_,
      target_time = target_time,
      mean_yProj = NA_real_,
      mean_xProj = NA_real_,
      n_points = 0
    )
  }  
}

avgPoints <- bind_rows(lapply(time_seq, avg_window, data = utmData))

avgPoints <- avgPoints |>
  filter(!is.na(mean_yProj), !is.na(mean_xProj))
names(avgPoints)

a1 <- st_as_sf(avgPoints, coords = c("mean_xProj", "mean_yProj"), crs = 32718, na.fail = FALSE)
b1 <- st_transform(a1, 4326)
c1 <- st_coordinates(b1)
avgPoints$lon <- c1[,1]
avgPoints$lat <- c1[,2]
plot(PButmData$X, PButmData$Y)




write.csv(avgPoints, file = "C:/Users/Jawor/Desktop/R_repos/playbackAnalysis/avgPoints.csv",row.names=TRUE,col.names=TRUE,sep=",")

avgPoints <- avgPoints |>
  mutate(date = as.Date(target_time), time = format(target_time, format = "%H:%M:%S"))

AVs <- read.csv(file.choose(), header = TRUE)
AVs <- AVs |>
  mutate(Obs.Sample.ID = as.numeric(str_remove(Obs.Sample.ID, "OS"))) |>
  filter(Obs.Sample.ID >= 40601 & Obs.Sample.ID <= 40656)
names(AVs)
AVs <- AVs |>
  filter(Taxon == "Lagothrix" & Follow.Data.Included == TRUE) |>
  mutate(Group = str_remove(Group, "Lagothrix")) |>
  select(Avistaje.ID, Group)

AVs <- AVs |>
  mutate(AV_ID = Avistaje.ID) |>
  select(AV_ID, Group)

avgPoints <- avgPoints |>
  mutate(AV_ID = if_else(AV_ID == "AV74111", "AV74112", AV_ID))

avgPoints <- avgPoints|>
  left_join(AVs, by = "AV_ID")


pbPoints <- read.csv(file.choose(), header = TRUE)

PB_points_ll <- st_as_sf(pbPoints, coords = c("Longitude", "Latitude"), crs = 4326, na.fail = FALSE)
PB_points_utm <- st_transform(PB_points_ll, crs = 32718)

PButmData <- PB_points_utm |>
  st_coordinates() |>
  as.data.frame() |>
  cbind(PB_points_utm) |>
  mutate(Time = paste0(Time, ":00"), Time = hms::as_hms(Time))

write.csv(PButmData, file = "C:/Users/Jawor/Desktop/R_repos/playbackAnalysis/PB_utm.csv",row.names=TRUE,col.names=TRUE,sep=",")
#Add latlon to PButmData


a <- st_as_sf(PButmData, coords = c("X", "Y"), crs = 32718, na.fail = FALSE)
b <- st_transform(a, 4326)
c <- st_coordinates(b)
PButmData$lon <- c[,1]
PButmData$lat <- c[,2]



plot(PButmData$X, PButmData$Y)

#Matching playbacks to 15 minute points
#First, clean avgPoints and PButmData
avgPoints <- avgPoints |>
  mutate(Group = str_trim(Group), Group = str_remove(Group,  "/"),
         target_time = force_tz(as.POSIXct(target_time), tzone = "America/Guayaquil"))|>
  select(-any_of(c("datetime", "Date")))

PButmData <- PButmData |>
  mutate(Group_ID = str_trim(Group_ID), Group_ID = str_remove(Group_ID, "/"), 
         datetime = ymd_hms(paste(Date, Time)), 
         datetime = force_tz(as.POSIXct(datetime), tzone = "America/Guayaquil"))

#Use fuzzyjoin to match playbacks to averaged points so that they all line up
PBmatched <- PButmData |>
  fuzzyjoin::difference_inner_join(
    avgPoints,
    by = c("datetime" = "target_time"),
    max_dist = as.difftime(15, units = "mins")) |>
  filter(Group_ID == Group) |>
  mutate(time_diff = abs(as.numeric(difftime(datetime, target_time, units = "secs")))) |>
  group_by(Playback_Number) |>
  slice_min(time_diff, with_ties = FALSE) |>
  ungroup()
names(PBmatched)
plot(PBmatched$X, PBmatched$Y)
plot(PBmatched$mean_xProj, PBmatched$mean_yProj)
  
#Calculate distance traveld after playback
distPostPB <- PBmatched |>
  rowwise()|>
  do({
    pb <- .
    group_id <- pb$Group_ID
    pb_time <- pb$target_time
    
    #Subset data to be specific for same group id
    gps_subset <- avgPoints |>
      filter(Group == group_id,
             target_time >= pb_time,
             target_time <= pb_time + hours(1)) |>
      arrange(target_time)
    
    full_hour_post <- nrow(gps_subset) == 5
    
    #Calculate distances 1 hour post playback
    if(nrow(gps_subset) > 1) {
      dists <- sqrt(diff(gps_subset$mean_xProj)^2 + diff(gps_subset$mean_yProj)^2)
      total_dist <- sum(dists, na.rm = TRUE)
      
      tibble(
        PB_Number = pb$Playback_Number,
        PB_time = pb$datetime,
        group_id = group_id,
        PB_species = pb$Playback_Species,
        Control = pb$Control,
        ModelUsed = pb$Model_Used,
        ModelOnly = pb$ModelOnly,
        Context = pb$Context,
        dist_1h_m = total_dist,
        full_hour_post = full_hour_post,
        start_datetime = min(gps_subset$target_time),
        start_x = gps_subset$mean_xProj[1],
        start_y = gps_subset$mean_yProj[1],
        end_datetime = max(gps_subset$target_time),
        end_x = gps_subset$mean_xProj[nrow(gps_subset)],
        end_y = gps_subset$mean_yProj[nrow(gps_subset)]
      )
    } else{
      tibble(
        Playback_Number = pb$Playback_Number,
        playback_time   = pb$datetime,
        group_id        = group_id,
        PB_species = pb$Playback_Species,
        Control = pb$Control,
        ModelUsed = pb$Model_Used,
        ModelOnly = pb$ModelOnly,
        Context = pb$Context,
        dist_1h_m       = NA_real_,
        full_hour_post = full_hour_post,
        start_datetime  = pb_time,
        start_x         = NA_real_,
        start_y         = NA_real_,
        end_datetime    = NA,
        end_x           = NA_real_,
        end_y           = NA_real_
      )
    }  
  }) |>
  ungroup()

distPrePB <- PBmatched |>
  rowwise()|>
  do({
    pb <- .
    group_id <- pb$Group_ID
    pb_time <- pb$target_time
    
    #Subset data to be specific for same group id
    gps_subset <- avgPoints |>
      filter(Group == group_id,
             target_time <= pb_time,
             target_time >= pb_time - hours(1)) |>
      arrange(target_time)
    full_hour_pre <- nrow(gps_subset) == 5
    #Calculate distances 1 hour post playback
    if(nrow(gps_subset) > 1) {
      dists <- sqrt(diff(gps_subset$mean_xProj)^2 + diff(gps_subset$mean_yProj)^2)
      total_dist <- sum(dists, na.rm = TRUE)
      
      tibble(
        PB_Number = pb$Playback_Number,
        PB_time = pb$datetime,
        group_id = group_id,
        PB_species = pb$Playback_Species,
        Control = pb$Control,
        ModelUsed = pb$Model_Used,
        ModelOnly = pb$ModelOnly,
        Context = pb$Context,
        dist_1h_pre_m = total_dist,
        full_hour_pre = full_hour_pre,
        start_datetime = min(gps_subset$target_time),
        start_x = gps_subset$mean_xProj[1],
        start_y = gps_subset$mean_yProj[1],
        end_datetime = max(gps_subset$target_time),
        end_x = gps_subset$mean_xProj[nrow(gps_subset)],
        end_y = gps_subset$mean_yProj[nrow(gps_subset)]
      )
    } else{
      tibble(
        Playback_Number = pb$Playback_Number,
        playback_time   = pb$datetime,
        group_id        = group_id,
        PB_species = pb$Playback_Species,
        Control = pb$Control,
        ModelUsed = pb$Model_Used,
        ModelOnly = pb$ModelOnly,
        Context = pb$Context,
        dist_1h_pre_m       = NA_real_,
        full_hour_pre = full_hour_pre,
        start_datetime  = pb_time,
        start_x         = NA_real_,
        start_y         = NA_real_,
        end_datetime    = NA,
        end_x           = NA_real_,
        end_y           = NA_real_
      )
    }  
  }) |>
  ungroup()

#Comparing to non-PB days
pbDates <- PBmatched |>
  mutate(pbdate = as.Date(target_time)) |>
  select(Group_ID, pbdate) |>
  distinct()

allDates <- avgPoints |>
  mutate(date = as.Date(target_time)) |>
  select(Group, date) |>
  distinct()

control_dates <- allDates |>
  anti_join(pbDates, by = c("Group" = "Group_ID", "date" = "pbdate"))

#Go through control days to calculate diastances in the same time window
controlDist <- PBmatched |>
  rowwise() |>
  do({
    pb <- .
    group_id <- pb$Group_ID
    pb_hour <- format(pb$target_time, "%H:%M:%S")
    
    #filter for control days for group
    ctrl_dates <- control_dates |>
      filter(Group == group_id) |>
      pull(date)
    
    #Calc distance from control date
    map_dfr(ctrl_dates, function(ctrl_date){
      start_time <- as.POSIXct(paste(ctrl_date, pb_hour), tz = "America/Guayaquil")
      end_time <- start_time + hours(1)
      
      gps_subset <- avgPoints |>
        filter(Group == group_id,
               target_time >= start_time, target_time <= end_time) |>
        arrange(target_time)
      full_hour_post <- nrow(gps_subset) == 5
      if(nrow(gps_subset) > 1) {
        dists <- sqrt(diff(gps_subset$mean_xProj)^2 + diff(gps_subset$mean_yProj)^2)
        total_dist <- sum(dists, na.rm = TRUE)
      } else {
        total_dist <- NA_real_
      }
      
      
      tibble(
        pbNumber = pb$Playback_Number,
        group_id = group_id,
        PB_species = pb$Playback_Species,
        Control = pb$Control,
        ModelUsed = pb$Model_Used,
        ModelOnly = pb$ModelOnly,
        Context = pb$Context,
        pbDate = pb$Date,
        pbTime = pb$Time,
        full_hour_post = full_hour_post,
        control_date = ctrl_date,
        Ctrlstart_datetime = gps_subset %>% slice(1) %>% pull(target_time),
        Ctrlstart_x         = gps_subset %>% slice(1) %>% pull(mean_xProj),
        Ctrlstart_y         = gps_subset %>% slice(1) %>% pull(mean_yProj),
        Ctrlend_datetime   = gps_subset %>% slice(n()) %>% pull(target_time),
        Ctrlend_x          = gps_subset %>% slice(n()) %>% pull(mean_xProj),
        Ctrlend_y          = gps_subset %>% slice(n()) %>% pull(mean_yProj),
        control_dist_m = total_dist
      )
    })
  }) |>
  ungroup()

control_summary <- controlDist |>
  filter(full_hour_post == TRUE, Control == FALSE) |>
  group_by(pbNumber, group_id) |>
  summarise(mean_control_dist = mean(control_dist_m, na.rm = TRUE), .groups = "drop")

# Join with playback distances
comparison <- distPostPB %>%
  left_join(control_summary, by = c("PB_Number" = "pbNumber")) |>
  filter(full_hour_post == TRUE)
PB_pred <- distPostPB |>
  filter(Control == FALSE, full_hour_post == TRUE)
predMean <- mean(PB_pred$dist_1h_m)
cMean <- mean(control_summary$mean_control_dist)

# Optional: paired t-test
t.test(comparison$dist_1h_m, comparison$mean_control_dist, paired = TRUE)
wilcox.test(comparison$dist_1h_m, comparison$mean_control_dist, paired = TRUE)

#Next steps: Prepping distance data to create GLMMs that we talked with tony about


