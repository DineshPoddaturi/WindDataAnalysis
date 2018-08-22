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

















