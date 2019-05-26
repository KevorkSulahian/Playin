data("AirPassengers")
class(AirPassengers)


#This is the start of the time series
start(AirPassengers)

#This is the end of the time series
end(AirPassengers)

#The cycle of this time series is 12months in a year
frequency(AirPassengers)

#The number of passengers are distributed across the spectrum
summary(AirPassengers)

plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))

#This will print the cycle across years.
cycle(AirPassengers)

#This will aggregate the cycles and display a year on year trend
plot(aggregate(AirPassengers, FUN = mean))

#Box plot across months will give us a sense on seasonal effect
boxplot(AirPassengers~cycle(AirPassengers))


## important shit

# 1 - The year on year trend clearly shows that the #passengers have been increasing without fail.
# 2 - The variance and the mean value in July and August is much higher than rest of the months.
# 3 - Even though the mean value of each month is quite different their variance is small. Hence,
#     we have strong seasonal effect with a cycle of 12 months or less.


# ARMA

#  ARMA model, AR stands for auto-regression and MA stands for moving average

library(tseries)

# We see that the series is stationary enough to do any kind of time series modelling.
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

acf(log(AirPassengers))

# Clearly, the decay of ACF chart is very slow, which means that the population is not stationary.
# We have already discussed above that we now intend to regress on the difference of logs rather than log directly.
# Let’s see how ACF and PACF curve come out after regressing on the difference.
par(mfrow=c(1,2))
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

# Pred?

(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

pred <- predict(fit, n.ahead = 10*12)

par(mfrow=c(1,1))
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))







