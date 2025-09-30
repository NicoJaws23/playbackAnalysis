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

Tracks15<-filter(Tracks, X15MIN == "TRUE" | X14MIN == "TRUE" | X29MIN == "TRUE" | X44MIN == "TRUE" | X59MIN == "TRUE")
head(Tracks15)

class(Tracks15$ltimeRounded)


realtime<-as.POSIXct(Tracks15$ltimeRounded,format="%m/%d/%y %H:%M")
realtime
Tracks15Time<-cbind(Tracks15, realtime)
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

track_files <- list.files("C:\\Users\\Jawor\\Desktop\\R_repos\\playbackAnalysis\\2025Avistaje", pattern = "\\.csv$", full.names = TRUE)
data <- lapply(track_files, read.table, sep=",", header=TRUE)
combined.data <- do.call(rbind, data)
combined.data$file_origin <- row.names(combined.data)
combined.data <- combined.data |>
  mutate(time = ymd_hms(time))

time_seq <- seq(floor_date(min(combined.data$time), "15 minutes"), ceiling_date(max(combined.data$time), "15 minutes"), by = "15 mins")

avg_window <- function(target_time, data, window = 2) {
  subset <- data |>
    filter(time >= target_time - minutes(window),
           time <= target_time + minutes(window))
  
  if (nrow(subset) > 0) {
    tibble(
      target_time = target_time,
      mean_lat = mean(subset$Latitude, na.rm = TRUE),
      mean_lon = mean(subset$Longitude, na.rm = TRUE),
      n_points = nrow(subset)
    )
  } else {
    tibble(
      target_time = target_time,
      mean_lat = NA_real_,
      mean_lon = NA_real_,
      n_points = 0
    )
  }
}

avgPoints <- bind_rows(lapply(time_seq, avg_window, data = combined.data))

avgPoints <- avgPoints |>
  filter(!is.na(mean_lat), !is.na(mean_lon))

write.csv(avgPoints, file = "C:/Users/Jawor/Desktop/R_repos/playbackAnalysis/avgPoints.csv",row.names=TRUE,col.names=TRUE,sep=",")

avgPoints2 <- avgPoints |>
  mutate(date = as.Date(target_time), time = format(target_time, format = "%H:%M:%S"))



pbPoints <- read.csv(file.choose(), header = TRUE)
pbPointsFilt <- pbPoints |>
  filter(symbol == "Flag")

playbacks <- pbPointsFilt |>
  mutate(dateTime = ymd_hms(wpt_class, tz = "UTC"), date = as.Date(dateTime), time = format(dateTime, format = "%H:%M:%S"))

playback_matches <- playbacks %>%
  rowwise() %>%
  mutate(
    match = list({
      tracks_today <- avgPoints2 %>% filter(date == as.Date(dateTime))
      if (nrow(tracks_today) == 0) {
        tibble(
          closest_track_time = NA,
          closest_lat = NA,
          closest_lon = NA,
          time_diff_sec = NA
        )
      } else {
        idx <- which.min(abs(tracks_today$target_time - dateTime))
        tibble(
          closest_track_time = tracks_today$target_time[idx],
          closest_lat = tracks_today$mean_lat[idx],
          closest_lon = tracks_today$mean_lon[idx],
          time_diff_sec = as.numeric(difftime(tracks_today$target_time[idx], dateTime, units = "secs"))
        )
      }
    })
  ) %>%
  unnest(match) %>%
  ungroup()
names(playback_matches)

leaflet() |>
  addTiles() |>
  addPolylines(data = avgPoints2, lng = ~mean_lon, lat = ~mean_lat, color = "blue") |>
  addCircleMarkers(data = avgPoints2, lng = ~mean_lon, lat = ~mean_lat, radius = 3, color = "red", popup = ~paste0("Time: ", target_time)) |>
  addCircleMarkers(data = playback_matches, lng = ~Longitude, lat = ~Latitude, radius = 5, color = "green", fillColor = "yellow", fillOpacity = 0.8, weight = 2, popup = ~paste0(ident))

leaflet(pbPointsFilt) |>
  addTiles() |>
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 3, color = "red", popup = ~paste0(ident))


tracks_with_dist <- avgPoints2 |>
  arrange(date, target_time) |>
  group_by(date) |>
  mutate(dist_m = distHaversine(cbind(mean_lon, mean_lat), cbind(lag(mean_lon), lag(mean_lat)))) |>
  ungroup()

playback_travel <- playback_matches |>
  rowwise() |>
  mutate(
    travel_1h_m = {
      tracks_today <- tracks_with_dist |> filter(date == as.Date(dateTime))
      t_start <- closest_track_time
      t_end <- t_start + hours(1)
      seg <- tracks_today |> filter(target_time >= t_start & target_time <= t_end)
      sum(seg$dist_m, na.rm = TRUE)
    }
  ) |>
  select(ident, date, time, travel_1h_m) |>
  ungroup()

avgPoints2 <- avgPoints2 |>
mutate(date = as.Date(target_time))
playback_days <- unique(as.Date(playback_matches$dateTime))
control_days <- setdiff(unique(avgPoints2$date), playback_days)
control_days <- as.Date(control_days, origin = "1970-01-01")

control_travel <- playback_matches |>
  rowwise() |>
  mutate(
    control_date = sample(control_days, 1), 
    control_travel_1h_m = {
      tracks_control <- tracks_with_dist |> filter(date == control_date)
      t_start <- hms::as_hms(closest_track_time)
      t_end <- t_start + 3600
      seg <- tracks_control |> filter(hms::as_hms(target_time) >= t_start & hms::as_hms(target_time) <= t_end)
      sum(seg$dist_m, na.rm = TRUE)
    }
  ) |>
  select(ident, control_date, control_travel_1h_m) |>
  ungroup()

comparison_df <- playback_travel |>
  left_join(control_travel, by = c("ident"))










