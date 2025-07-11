library(tidyverse)
library(lubridate)

weather_csv <- read.csv("weather_data.csv", skip = 2)
colnames(weather_csv) <- c("time", "temperature",  "cloud_coverage", "rain")
weather_csv$time <- ymd_hm(weather_csv$time, tz = "America/New_York")
weather_clean <- weather_csv %>% drop_na()

sensor_csv <- read.csv("temp_sensor.csv")
colnames(sensor_csv) <- c("time", "temperature", "humidity")
#sensor_csv$time <- parse_date_time(sensor_csv$time, orders = c("ymd HMS", "ymd HM", "ymd_HMS", "ymd_HM"))
sensor_csv$time <- mdy_hm(sensor_csv$time, tz = "America/New_York")

#sensor_csv$time <- ymd_hm(sensor_csv$time, tz = "America/New_York")
sensor_clean <- sensor_csv %>% drop_na()

cloud_daily <- weather_clean %>%
  mutate(date = as_date(time)) %>%
  group_by(date) %>%
  summarize(mean_cloud = mean(cloud_coverage, na.rm = TRUE))

plot_weather_clouds <- ggplot(cloud_daily, aes(x = date, y = mean_cloud)) +
  geom_line(size = 1, color = "navy") + 
  labs(
    title = "Daily Mean Cloud Coverage",
    x = "Date",
    y = "Cloud Coverage (%)"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


plot_rain <- ggplot(weather_clean, aes(x = time, y = rain)) +
  geom_line(size = 0.8, color = "deepskyblue") +
  labs(
    title = "Time vs. Rainfall",
    x = "Time",
    y = "Rainfall (inches)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

plot_outside_temp <- ggplot(weather_clean, aes(x = time, y = temperature)) +
  geom_line(size = 1, color = "orange2") + 
  labs(
    title = "Outside Temperature Over Time",
    x = "Time",
    y = "Temperature (°F)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

plot_chamber_temp <- ggplot(sensor_clean, aes(x = time, y = temperature)) +
  geom_line(size = 1, color = "purple4") +  
  labs(
    title = "Chamber Temperature Over Time",
    x = "Time",
    y = "Temperature (°F)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

print(plot_weather_clouds)
print(plot_rain)
print(plot_outside_temp)
print(plot_chamber_temp)