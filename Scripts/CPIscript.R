#convert first column of dataset to an time-series object
TS_CPI <- ts(CPI[625:873,2],start=1999, frequency=12)

ggarrange(ggAcf(TS_CPI),ggPacf(TS_CPI))

#-------------------------------------------------
#kpss unit root test (statistical test of stationarity), H0 = stationary, HA = not stationary
TS_CPI %>% ur.kpss() %>% summary()
#in this case value of test statistic is 12.569
#critical values for 10%, 5%, 2.5%, and 1% significance are 0.347 0.463  0.574 0.739 so we fail to reject H0
#need to make time-series stationary

#-------------------------------------------------

#determine number of single differences to make data stationary given a level of significance of 0.05
CPIsingle_diffs <- ndiffs(TS_CPI, alpha=0.05, test="kpss")
#output is 2, so two single differences (second order differencing) is needed to make data stationary at alpha=0.05

#differencing by the reccomended number of first differences
D_CPI <- diff(TS_CPI,lag=1,differences=CPIsingle_diffs)

#-------------------------------------------------

#graphing time series
cbind("CPI (1982 to 1984 = 100)" = TS_CPI,
      "CPI Differenced" = D_CPI) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") + ggtitle("Consumer Price Index")

#----------------------------------------
---------

#forecasting time series with auto.arima
CPImodel1 <- auto.arima(TS_CPI, seasonal=FALSE)

#forecast next 15 values, with confidence intervals
CPIforecast1 <-forecast(CPImodel1, h=15, level=c(85,90,95)) 

#graph the forecast and past 24 observations
CPIgraph1 <- autoplot(CPIforecast1, include=36)
CPIgraph1


#-------------------------------------------------

#determining parameters for ARIMA without auto.arima
#note that this is already differenced dataset

#ACF and PACF plots
ggarrange(ggAcf(D_CPI),ggPacf(D_CPI))


#notice that the first three PACF values are negative and below the critical values (slightly overdifferenced)
#because of this, we should add 3 MA terms to the model
CPImodel2 <- arima(TS_CPI, order=c(3,1,3))

CPIforecast2 <-forecast(CPImodel2, h=15, level=c(85,90,95)) 

CPIgraph2 <- autoplot(CPIforecast2, include=36)
CPIgraph2


#-------------------------------------------------

ggarrange(CPIgraph1, CPIgraph2)

CPI_residuals_1 <- as.vector(CPImodel1[["residuals"]])
dates <- seq( from= 1999, to= 2019.666666666, by=0.083333333)
plot(CPI_residuals_1~dates)
