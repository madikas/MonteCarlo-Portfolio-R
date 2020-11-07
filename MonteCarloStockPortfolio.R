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

# 1.Simulating Daily Returns for Individual Stock (Normal Distribution)

#Parameters for the geometric mean & std dev of daily return for stock 
m <- 0.003
s <- 0.0098
tradingdays<-253

#Vector of daily returns

set.seed(123)

dailyreturns_norm <-rnorm(n=tradingdays,m,s)
mean(dailyreturns_norm)
sd(dailyreturns_norm)
plot(density(dailyreturns_norm))

# Generating Daily Price from Returns 
STK_PRC <- 10
r <-dailyreturns_norm
stock_prices_norm <- c()
for (i in seq(tradingdays))
{
  STK_PRC <- STK_PRC*(1+r[i])
  stock_prices_norm <- c(stock_prices_norm,STK_PRC)
  
}
print(stock_prices_norm)

#2. Simulating Exponential Distribution for Daily Returns

lambda = 30
tradingdays = 253

set.seed(123456)
dailyreturns_exp = rexp(n=tradingdays, rate = lambda )
mean(dailyreturns_exp)
sd(dailyreturns_exp)
hist(dailyreturns_exp)

# Generating Daily Price from Returns 
stockPrice <- 0.06
r <-dailyreturns_exp
stock_prices_exp <- c()
for (i in seq(tradingdays))
{
  stockPrice <- stockPrice*(1+r[i])
  stock_prices_exp <- c(stock_prices_exp,stockPrice)
  
}
print(stock_prices_exp)