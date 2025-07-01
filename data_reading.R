library(tidyverse)
library(lubridate)

weather_csv <- read.csv("weather_data.csv", skip=2)

colnames(weather_csv) <- c("time", "temperature", "rain", "precipitation", "cloud_coverage")

# formatting time col properly
weather_csv$time <- ymd_hm(weather_csv$time, tz = "America/New_York")

# remove NaN (will not matter after the 10th though (no NaN))
weather_clean <- weather_csv %>% drop_na()


sensor_csv <- read.csv("temp_sensor.csv", skip=1)

colnames(sensor_csv) <- c("time", "temperature", "humidity")

# formatting time col properly

#sensor_csv$time <- ymd_hm(sensor_csv$time, tz = "America/New_York")

# remove NaN (will not matter after the 10th though (no NaN))
sensor_clean <- sensor_csv %>% drop_na()

plot_weather <- function(y_var, y_label, title) {
  ggplot(weather_clean, aes(x = time, y = .data[[y_var]])) +
    geom_line(size = 0.8) +
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

plot_sensor <- function(y_var, y_label, title) {
  ggplot(sensor_clean, aes(x = time, y = .data[[y_var]])) +
    geom_line(size = 0.8, group=1) +
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

plot_weather("temperature", "Temperature (°F)", "Time vs. Temperature")

plot_weather("cloud_coverage", "Cloud Coverage (%)", "Time vs. Cloud Coverage")

plot_weather("rain", "Rainfall (inches)", "Time vs. Rainfall")

plot_sensor("temperature", "Temperature (°F)", "Time vs. Temperature (Chamber)")
