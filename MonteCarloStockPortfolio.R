library(quantmod) 
library(timeSeries)
library(fPortfolio)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)

#Vector of Stock Tickers
tickers=c("GOOGL", "AMZN", "PFE", "JPM", "CVX", "BA", "FAST", "INTC", 
          "T", "NWSA")

#Daily Prices for the last five years
stockPrices = NULL
for (ticker in tickers)
  stockPrices = cbind(stockPrices,
                      getSymbols.yahoo(ticker, from="2015-01-01",
                                       periodicity = "daily",
                                        auto.assign=FALSE)[,4])
#Keep dates that have closing prices for all tickers
stockPrices = stockPrices[apply(stockPrices, 1, function(x) all(!is.na(x))),]
#Rename Columns matching stock ticker names
colnames(stockPrices) = tickers

#Daily Rate of Change
stockReturns = na.omit(ROC(stockPrices, type="discrete"))
stockReturns = as.timeSeries(stockReturns)

#efficient frontier
effFrontier = portfolioFrontier(stockReturns, constraints = "LongOnly")

#Plot efficient frontier
#Plot options fportfolio
#1 - Plot efficient frontier
#2 - Plot Min Variance Portfolio
#3 - Plot Tangency Portfolio
#4 - Plot Risk Returns of each ticker
#5 - Plot Equal Weights Portfolio
#6 - Plot Two Asset Frontiers
#7 - Plot Monte Carlo Portfolios
#8 - Plot Sharpe Ratio
plot(effFrontier, 1)