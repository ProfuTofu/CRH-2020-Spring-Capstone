#convert first column of dataset to an time-series object
TS_URATE <- ts(URATE[2],start=1948, frequency=12)
ggarrange(ggAcf(TS_URATE),ggPacf(TS_URATE))
#-------------------------------------------------

#kpss unit root test (statistical test of stationarity), H0 = stationary, HA = not stationary
TS_URATE %>% ur.kpss() %>% summary()

#-------------------------------------------------

#determine number of single differences to make data stationary given a level of significance of 0.05
URATEsingle_diffs <- ndiffs(TS_URATE, alpha=0.05, test="kpss")

#differencing by the reccomended number of first differences
D_URATE <- diff(TS_URATE,lag=1,differences=URATEsingle_diffs)

#-------------------------------------------------

#graphing time series
cbind("URATE" = TS_URATE,
      "URATE Differenced" = D_URATE) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") + ggtitle("Unemployment Rate")

#-------------------------------------------------

#forecasting time series with auto.arima
URATEmodel1 <- auto.arima(TS_URATE, seasonal=FALSE)

#forecast next 15 values, with confidence intervals
URATEforecast1 <-forecast(URATEmodel1, h=15, level=c(85,90,95)) 

#graph the forecast and past 24 observations
URATEgraph1 <- autoplot(URATEforecast1, include=24) 
URATEgraph1
#-------------------------------------------------

#determining parameters for ARIMA without auto.arima
#note that this is already differenced dataset

#ACF and PACF plots
D_URATE2 <- diff(TS_URATE,lag=1,differences=2)
ggarrange(ggAcf(D_URATE2),ggPacf(D_URATE2))

# INCOMPLETE #
URATEmodel2 <- arima(TS_URATE, order=c(4,1,1))
# INCOMPLETE #

URATEforecast2 <-forecast(URATEmodel2, h=15, level=c(85,90,95)) 
URATEforecast20 <-forecast(URATEmodel2, h=15, level=c(85,90,99)) 


URATEgraph2 <- autoplot(URATEforecast2, include=24)
URATEgraph2


#-------------------------------------------------

ggarrange(URATEgraph1,URATEgraph2)

URATE_residuals_1 <- as.vector(URATEmodel2[["residuals"]])
dates <- seq( from= 1948, to= 2019.666666666, by=0.083333333)
plot(URATE_residuals_1~dates)