#Breaking up and getting summary stats of playback responses
library(tidyverse)
library(ggplot2)
f <- "C:\\Users\\Jawor\\Desktop\\R_repos\\playbackAnalysis\\PlaybackResponseData.csv"
df <- read_csv(f)

#Separate response types per playback
df2 <- df |>
  separate_rows("ResponseType", sep = "/")

#Get only responses data
df3 <- df2 |>
  filter(Response == TRUE) |>
  filter(ResponseType != "")

#Get percentage of how often a response occurred
response <- df3 |>
  group_by(ResponseType) |>
  summarise(count = n()) |>
  mutate(totalResp = 58, 
         percentResp = (count/totalResp)*100, 
         percentRoun = round(percentResp, digits = 2))

ggplot(response, mapping = aes(x = "", y = percentRoun, fill = ResponseType)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = percentRoun), position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0)

#Audio only
aud <- df3 |>
  filter(Model_Used == FALSE) |>
  mutate(ResponseLatencyFromFirstNote_seconds = as.numeric(gsub("/", "", ResponseLatencyFromFirstNote_seconds))) |>
  mutate(ResponseLength_seconds = as.numeric(gsub("/", "", ResponseLength_seconds)))


#ModelUsed
model <- df3 |>
  filter(Model_Used == TRUE, ModelOnly == FALSE) |>
  mutate(ResponseLatencyFromFirstNote_seconds = as.numeric(gsub("/", "", ResponseLatencyFromFirstNote_seconds))) |>
  mutate(ResponseLength_seconds = as.numeric(gsub("/", "", ResponseLength_seconds)))

#Resp by species
#Harpy
harpy <- df |>
  filter(Playback_Species == "Harpy Eagle") |>
  mutate(ResponseLatencyFromFirstNote_seconds = as.numeric(gsub("/", "", ResponseLatencyFromFirstNote_seconds))) |>
  mutate(ResponseLength_seconds = as.numeric(gsub("/", "", ResponseLength_seconds)))

meanLatency <- mean(harpy$ResponseLatencyFromFirstNote_seconds, na.rm = TRUE)
medianLatency <- median(harpy$ResponseLatencyFromFirstNote_seconds, na.rm = TRUE)
meanLength <- mean(harpy$ResponseLength_seconds, na.rm = TRUE)
medianLength <- median(harpy$ResponseLength_seconds, na.rm = TRUE)

harpyMetrics <- data.frame(Species = "Harpy Eagle", meanLatency, medianLatency, meanLength, medianLength)
#Jaguar
#All jaguar related experiments
jaguar <- df |>
  filter(Playback_Species == "Jaguar") |>
  mutate(ResponseLatencyFromFirstNote_seconds = as.numeric(gsub("/", "", ResponseLatencyFromFirstNote_seconds))) |>
  mutate(ResponseLength_seconds = as.numeric(gsub("/", "", ResponseLength_seconds)))

meanLatencyJ <- mean(jaguar$ResponseLatencyFromFirstNote_seconds, na.rm = TRUE)
medianLatencyJ <- median(jaguar$ResponseLatencyFromFirstNote_seconds, na.rm = TRUE)
meanLengthJ <- mean(jaguar$ResponseLength_seconds, na.rm = TRUE)
medianLengthJ <- median(jaguar$ResponseLength_seconds, na.rm = TRUE)

JaguarMetrics <- data.frame(Species = "Jaguar", meanLatencyJ, medianLatencyJ, meanLengthJ, medianLengthJ)

#Jaguar experiments with model

#Jaguar experiments without model