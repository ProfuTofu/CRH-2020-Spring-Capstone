#convert first column of dataset to an time-series object
## TS_RGDP <- ts(RGDP[2],start=c(1947,2), frequency=4 )
TS_RGDP <- ts(RGDP[144:289,2],start=c(1983,1), frequency=4 )

#-------------------------------------------------

ggarrange(ggAcf(TS_RGDP),ggPacf(TS_RGDP))

#kpss unit root test (statistical test of stationarity), H0 = stationary, HA = not stationary
TS_RGDP %>% ur.kpss() %>% summary()

#-------------------------------------------------

#determine number of single differences to make data stationary given a level of significance of 0.05
RGDPsingle_diffs <- ndiffs(TS_RGDP, alpha=0.05, test="kpss")

#differencing by the reccomended number of first differences
D_RGDP <- diff(TS_RGDP,lag=1,differences=RGDPsingle_diffs)
#-------------------------------------------------

#graphing time series
cbind("RGDP Percent Change" = TS_RGDP,
      "RGDP Differenced" = D_RGDP) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") + ggtitle("Real GDP Growth Rate (seasonally adjusted)")

#-------------------------------------------------

#forecasting time series with auto.arima
RGDPmodel1 <- auto.arima(TS_RGDP, seasonal=FALSE)

#forecast next 4 values, with confidence intervals
RGDPforecast1 <-forecast(RGDPmodel1, h=6, level=c(85,90,95)) 

#graph the forecast and past 48 observations
RGDPgraph1 <- autoplot(RGDPforecast1, include=48)
RGDPgraph1

RGDPresults1 <- as.data.frame(RGDPforecast1)

RGDP_residuals_1 <- as.vector(RGDPmodel1[["residuals"]])
dates <- seq( from= 1983, to= 2019.25, by=.25)
plot(RGDP_residuals_1~dates)

#-------------------------------------------------

#determining parameters for ARIMA without auto.arima
#note that this is already differenced dataset

#ACF and PACF plots
ggarrange(ggAcf(D_RGDP),ggPacf(D_RGDP))

# INCOMPLETE #
RGDPmodel2 <- arima(TS_RGDP, order=c(1,1,1))
# INCOMPLETE #

RGDPforecast2 <-forecast(RGDPmodel2, h=6, level=c(85,90,95)) 

RGDPgraph2 <- autoplot(RGDPforecast2, include=48)
RGDPgraph2

RGDPresults2 <- as.data.frame(RGDPforecast2)


#-------------------------------------------------

ggarrange(RGDPgraph1,RGDPgraph2)
