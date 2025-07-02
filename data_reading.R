library(tidyverse)
library(lubridate)
library(viridis)

weather_csv <- read.csv("weather_data.csv", skip = 2)
colnames(weather_csv) <- c("time", "temperature", "rain", "precipitation", "cloud_coverage")
weather_csv$time <- ymd_hm(weather_csv$time, tz = "America/New_York")
weather_clean <- weather_csv %>% drop_na()

sensor_csv <- read.csv("temp_sensor.csv")
colnames(sensor_csv) <- c("time", "temperature", "humidity")
sensor_csv$time <- ymd_hm(sensor_csv$time, tz = "America/New_York")
sensor_clean <- sensor_csv %>% drop_na()

colnames(weather_clean)[colnames(weather_clean) == "temperature"] <- "temperature_weather"
colnames(sensor_clean)[colnames(sensor_clean) == "temperature"] <- "temperature_sensor"

temp_merge <- left_join(weather_clean, sensor_clean, by = "time")

plot_weather <- function(y_var, y_label, title, color_index = 1) {
  ggplot(weather_clean, aes(x = time, y = .data[[y_var]])) +
    geom_line(size = 0.8, color = viridis(5, option = "D")[color_index]) +
    labs(
      title = title,
      x = "Time",
      y = y_label
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
}

cloud_daily <- weather_clean %>%
  mutate(date = as_date(time)) %>%
  group_by(date) %>%
  summarize(mean_cloud = mean(cloud_coverage, na.rm = TRUE))

plot_weather_clouds <- ggplot(cloud_daily, aes(x = date, y = mean_cloud)) +
  geom_line(size = 1, color = viridis(1, option = "D")) +
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

temp_dual_plot <- ggplot(temp_merge, aes(x = time)) +
  geom_line(aes(y = temperature_weather, color = "Outside Temp"), size = 1) +
  geom_line(aes(y = temperature_sensor, color = "Chamber Temp"), size = 1, linetype = "dashed") +
  scale_color_viridis_d(option = "D", end = 0.8) +
  labs(
    title = "Time vs. Temperature (Outside vs. Chamber)",
    x = "Time",
    y = "Temperature (°F)",
    color = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

plot_rain <- ggplot(weather_clean, aes(x = time, y = rain)) +
  geom_line(size = 0.8, color = viridis(5, option = "D")[3]) +
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

print(plot_weather_clouds)
print(temp_dual_plot)
print(plot_rain)
