library(forecast)
library(tseries)
library(urca)
library(stats4)
library(sarima)
library(ggplot2)
library(dplyr)
library(lubridate)
library(astsa)
library(gridExtra)

drug_data <- read.table("drug.txt", header = TRUE, sep = ",")
drug_data$date <- as.Date(drug_data$date)
data_ts <- ts(drug_data$value, start = c(1991, 7), frequency = 12)

plot(drug_data, 
     main="Monthly Anti-diabetic Drug Sales in Australia", 
     type = "l",
     xlab="Year", 
     ylab="Drug Sales")

decomp <- decompose(data_ts, type = "multiplicative")

par(mar = c(5, 4, 8, 4) + 2, las = 1)

plot(decomp$trend, xlab = "Time", ylab = "Value", type = "l", col = "blue", main = "Trend Plot", ylim = range(data_ts))
lines(data_ts, col = "black")
legend("topleft", legend = c("Trend", "Data"), lty = 1, col = c("blue", "black"))

plot(decomp$seasonal, xlab = "Time", ylab = "Value", type = "l", col = "red", main = "Seasonal Plot")

acf(data_ts, main = "Drug Original ACF Plot")
pacf(data_ts, main = "Drug Original PACF Plot")

