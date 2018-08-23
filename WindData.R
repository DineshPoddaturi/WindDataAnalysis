require(tidyverse)
require(tseries)
require(artfima)
require(arfima)
require(FitARMA)
require(gdata)
require(lubridate)
require(varhandle)
require(forecast)
require(xts)
require(plotly)
require(ggfortify)
require(gridExtra)
require(padr)
require(Metrics)
require(deseasonalize)
require(WiSEBoot)
require(readxl)


#Reading the data
windSpeed <- read_excel("Data/WindData_200mins.xlsx", sheet=2)
windSpeed <- windSpeed %>% as.data.frame()
names(windSpeed)

windSpeed_5m_S <- windSpeed %>% select(Seconds, ws_5m_s)
names(windSpeed_5m_S) <- c("Seconds", "Speed")
windSpeed_5m_S <- colMeans(matrix(windSpeed_5m_S$Speed, nrow=5)) %>% as.data.frame()
names(windSpeed_5m_S) <- "AvgSpeed"
windSpeed_5m_S <- windSpeed_5m_S %>% mutate(secs = 1:nrow(windSpeed_5m_S))
windSpeed_5m_S <- windSpeed_5m_S[,c(2,1)]
windSpeed_5m_S <- windSpeed_5m_S[-nrow(windSpeed_5m_S),]
WS_5m_S_plot <- ggplot(data = windSpeed_5m_S ,aes(x=secs, y = AvgSpeed))+geom_line(colour="turquoise")+
  theme(legend.position = "FALSE")+theme_gray()


windSpeed_40m_S <- windSpeed %>% select(Seconds, ws_40m_s)
names(windSpeed_40m_S) <- c("Seconds", "Speed")
windSpeed_40m_S <- colMeans(matrix(windSpeed_40m_S$Speed, nrow=5)) %>% as.data.frame()
names(windSpeed_40m_S) <- "AvgSpeed"
windSpeed_40m_S <- windSpeed_40m_S %>% mutate(secs = 1:nrow(windSpeed_40m_S))
windSpeed_40m_S <- windSpeed_40m_S[,c(2,1)]
windSpeed_40m_S <- windSpeed_40m_S[-nrow(windSpeed_40m_S),]
WS_40m_S_plot <- ggplot(data = windSpeed_40m_S ,aes(x=secs, y = AvgSpeed))+geom_line(colour="turquoise")+
  theme(legend.position = "FALSE")+theme_gray()



windSpeed_120m_S <- windSpeed %>% select(Seconds, ws_120m_s)
names(windSpeed_120m_S) <- c("Seconds", "Speed")
windSpeed_120m_S <- colMeans(matrix(windSpeed_120m_S$Speed, nrow=5)) %>% as.data.frame()
names(windSpeed_120m_S) <- "AvgSpeed"
windSpeed_120m_S <- windSpeed_120m_S %>% mutate(secs = 1:nrow(windSpeed_120m_S))
windSpeed_120m_S <- windSpeed_120m_S[,c(2,1)]
windSpeed_120m_S <- windSpeed_120m_S[-nrow(windSpeed_120m_S),]
WS_120m_S_plot <- ggplot(data = windSpeed_120m_S ,aes(x=secs, y = AvgSpeed))+geom_line(colour="turquoise")+
  theme(legend.position = "FALSE")+theme_gray()




windSpeed_5m_NW <- windSpeed %>% select(Seconds, ws_5m_nw)
names(windSpeed_5m_NW) <- c("Seconds", "Speed")
windSpeed_5m_NW <- colMeans(matrix(windSpeed_5m_NW$Speed, nrow=5)) %>% as.data.frame()
names(windSpeed_5m_NW) <- "AvgSpeed"
windSpeed_5m_NW <- windSpeed_5m_NW %>% mutate(secs = 1:nrow(windSpeed_5m_NW))
windSpeed_5m_NW <- windSpeed_5m_NW[,c(2,1)]
windSpeed_5m_NW <- windSpeed_5m_NW[-nrow(windSpeed_5m_NW),]
WS_5m_NW_plot <- ggplot(data = windSpeed_5m_NW ,aes(x=secs, y = AvgSpeed))+geom_line(colour="turquoise")+
  theme(legend.position = "FALSE")+theme_gray()


windSpeed_40m_NW <- windSpeed %>% select(Seconds, ws_40m_nwht)
names(windSpeed_40m_NW) <- c("Seconds", "Speed")
windSpeed_40m_NW <- colMeans(matrix(windSpeed_40m_NW$Speed, nrow=5)) %>% as.data.frame()
names(windSpeed_40m_NW) <- "AvgSpeed"
windSpeed_40m_NW <- windSpeed_40m_NW %>% mutate(secs = 1:nrow(windSpeed_40m_NW))
windSpeed_40m_NW <- windSpeed_40m_NW[,c(2,1)]
windSpeed_40m_NW <- windSpeed_40m_NW[-nrow(windSpeed_40m_NW),]
WS_40m_NW_plot <- ggplot(data = windSpeed_40m_NW ,aes(x=secs, y = AvgSpeed))+geom_line(colour="turquoise")+
  theme(legend.position = "FALSE")+theme_gray()



windSpeed_120m_NW <- windSpeed %>% select(Seconds, ws_120m_nwht)
names(windSpeed_120m_NW) <- c("Seconds", "Speed")
windSpeed_120m_NW <- colMeans(matrix(windSpeed_120m_NW$Speed, nrow=5)) %>% as.data.frame()
names(windSpeed_120m_NW) <- "AvgSpeed"
windSpeed_120m_NW <- windSpeed_120m_NW %>% mutate(secs = 1:nrow(windSpeed_120m_NW))
windSpeed_120m_NW <- windSpeed_120m_NW[,c(2,1)]
windSpeed_120m_NW <- windSpeed_120m_NW[-nrow(windSpeed_120m_NW),]

WS_120m_NW_plot <- ggplot(data = windSpeed_120m_NW, aes(x = secs, y = AvgSpeed)) + 
  geom_line(color="turquoise")+theme(legend.position = "FALSE")+theme_gray()

################### Average Wind Speed 5 meters above on South direction ######################################

south_5m <- windSpeed_5m_S$AvgSpeed


#################### Testing for stationarity using Augumented-Dickey Fuller test #########################

stat_5m_South <- adf.test(south_5m)

# Augmented Dickey-Fuller Test
# 
# data:  south_5m
# Dickey-Fuller = -4.8347, Lag order = 13, p-value = 0.01
# alternative hypothesis: stationary
# 
# Warning message:
#   In adf.test(south_5m) : p-value smaller than printed p-value

### From the above test the p-value is less than 0.01, so we reject the null hypothesis of existence of Unit root 
### and conclude that the data appear to be stationary. 
### From the plot, we can observe there is no significant seasonality or trend over the time.

################ Finding the best models using bestModels function #######################

# Here whittle estimation is used to find the best model appropriate for the data

south_5m_bestmodels <- bestModels(south_5m)

# > south_5m_bestmodels
#                   best       2nd best       3rd best      4th best
# AIC models   ARIMA(2,0,2) ARTFIMA(0,0,1) ARTFIMA(1,0,1) ARFIMA(1,0,2)
# AIC              -6291.19       -6290.06       -6289.26      -6286.07
# p(AIC)              1.000          0.568          0.381         0.077
# BIC models ARTFIMA(0,0,1)   ARIMA(2,0,2) ARTFIMA(1,0,1) ARFIMA(1,0,1)
# BIC              -6261.15       -6256.49       -6254.57      -6252.28
# p(BIC)              1.000          0.098          0.037         0.012

############ According to AIC
#### The best is ARIMA with p=2, q=2
#### 2nd best is ARTFIMA with p=0, q=1
#### 3rd best is ARTFIMA with p=1, q=1
#### 4th best is ARFIMA with p=1, q=2


########### According to BIC
# The best is ARTFIMA with p=0, q=1
# 2nd best is ARIMA with p=2, q=2
# 3rd best is ARTFIMA with p=1, q=1
# 4th best is ARFIMA with p=1, q=1



south_5m_ARTFIMA <- artfima(south_5m, glp = "ARTFIMA", arimaOrder = c(0,0,1), likAlg = "Whittle")

# ARTFIMA(0,0,1), MLE Algorithm: Whittle, optim: BFGS
# snr = 7.137, sigmaSq = 0.00918209918652608
# log-likelihood = 2233.6, AIC = -4457.2, BIC = -4428.28
# est.     se(est.)
# mean     2.4324009317 7.889594e-03
# d        0.3959996343 6.437732e-03
# lambda   0.0008240358 1.687020e-07
# theta(1) 0.0991263480 1.480822e-02

south_5m_ARIMA <- artfima(south_5m, glp = "ARIMA", arimaOrder = c(2,0,2), likAlg = "Whittle")

# ARIMA(2,0,2), MLE Algorithm: Whittle, optim: BFGS
# snr = 16.661, sigmaSq = 0.00423046683585833
# log-likelihood = 3151.56, AIC = -6291.13, BIC = -6256.43
# est.    se(est.)
# mean      2.4324009 0.007888666
# phi(1)    0.2842554 0.017403804
# phi(2)    0.6958443 0.045748241
# theta(1) -0.4000925 0.073161603
# theta(2)  0.3231031 0.015963744

south_5m_ARFIMA <- artfima(south_5m, glp = "ARFIMA", arimaOrder = c(1,0,2), likAlg = "Whittle")

# ARFIMA(1,0,2), MLE Algorithm: Whittle, optim: BFGS
# snr = 14.028, sigmaSq = 0.00497164581610796
# log-likelihood = 2956.73, AIC = -5901.45, BIC = -5866.75
# Warning: estimates converged to boundary!
#   est.    se(est.)
# mean      2.432400932 0.006310054
# d         0.489881971          NA
# phi(1)    0.199221729          NA
# theta(1) -0.001916746          NA
# theta(2) -0.164793742          NA









