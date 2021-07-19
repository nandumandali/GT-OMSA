
rm(list = ls())


temp_data <- read.csv("temps.csv", header = TRUE)

temp_data_vct <- as.vector(unlist(temp_data[,-1]))
temp_data_ts <- ts(temp_data_vct, frequency = 123, start = 1996, end=2015)
plot.ts(temp_data_ts)

summer_forecast <- HoltWinters(temp_data_ts, alpha= 0.03, seasonal="additive")
plot(summer_forecast)
summer_forecast

summer_forecast <- HoltWinters(temp_data_ts, alpha= 0.03, beta=0.06, seasonal="additive")
plot(summer_forecast)
summer_forecast

summer_forecast <- HoltWinters(temp_data_ts, seasonal="additive")
plot(summer_forecast)
summer_forecast



seasonalfactors <- summer_forecast$fitted[,4]

seasonMatrix = as.data.frame(t(matrix(c(seasonalfactors[-1]), 123)))

write.table(seasonMatrix, "seasontrend.csv", FALSE, TRUE, ",")
