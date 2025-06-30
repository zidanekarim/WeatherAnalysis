library("tidyverse")

weather_csv <- read.csv("weather_data.csv", skip=2)

colnames(weather_csv) <- c("time", "temperature", "rain", "precipitation", "cloud_coverage")

weather_csv$time <- as.POSIXct(weather_csv$time, format="%Y-%m-%dT%H:%M", tz="UTC")

plot(weather_csv$time, weather_csv$temperature,
     main="Time vs. Temp",
     xlab="Time",
     ylab="Temperature (°F)"
     )

plot(weather_csv$time, weather_csv$cloud_coverage,
     main="Time vs. Cloud Coverage",
     xlab="Time",
     ylab="Cloud Coverage (%)"
)


plot(weather_csv$time, weather_csv$rain,
     main="Time vs. Rainfall",
     xlab="Time",
     ylab="Rainfall (Inches)"
)
