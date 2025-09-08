#Breaking up and getting summary stats of playback responses
library(tidyverse)
library(ggplot2)
f <- "C:\\Users\\Jawor\\Desktop\\R_repos\\playbackAnalysis\\PlaybackResponseData.csv"
df <- read_csv(f)
df2 <- df |>
  separate_rows("ResponseType", sep = "/")

df3 <- df2 |>
  filter(Response == TRUE) |>
  filter(ResponseType != "")

response <- df3 |>
  group_by(ResponseType) |>
  summarise(count = n())

ggplot(response, mapping = aes(x = ResponseType, y = count)) +
  geom_boxplot()
