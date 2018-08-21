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



