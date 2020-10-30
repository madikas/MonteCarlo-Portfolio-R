library(quantmod) 
library(tidyverse)
tickers=c("GOOGL", "AMZN", "PFE", "JPM", "CVX", "BA", "FAST", "INTC", 
          "T", "NWSA")

stocks <-lapply(tickers, function(x) {getSymbols(x, 
                                      from = "2015/01/01", 
                                      to = "2020/10/31",
                                      periodicity = "daily",
                                      auto.assign=FALSE)} )

names(stocks) = tickers

head(stocks$GOOGL)
