library(quantmod) 
library(timeSeries)
library(fPortfolio)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library ( plotly)
library(RiskPortfolios)

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

#MeanReturns and Covariance matrix
meanReturns=colMeans(stockReturns)
covariancematrix=cov(stockReturns)*tradingdays

#efficient frontier
effFrontier = portfolioFrontier(stockReturns, constraints = "LongOnly")

#Weights of Optimal Portfolios
#(mean variance, min variance, max diversificaiton)

meanvar=optimalPortfolio(covariancematrix,meanReturns, control=list(type='mv',constraint='lo'))
minvar=optimalPortfolio(covariancematrix,meanReturns, control=list(type='minvol',constraint='lo'))
maxdiv=optimalPortfolio(covariancematrix,meanReturns, control=list(type='maxdiv',constraint='lo'))
maxdec=optimalPortfolio(covariancematrix,meanReturns, control=list(type='maxdec',constraint='lo'))

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

# 2.Simulating Daily Returns for Individual Stock (LogNormal Distribution)

  set.seed(123)
  
  #Parameters for the geometric mean & std dev of daily return for stock 
  m <- 0.003
  s <- 0.0098
  
  meanlog<-log(m^2/sqrt(s^2+m^2))
  stdlog<- sqrt(log(1+(s^2/m^2)))
  
  #Vector of daily returns 
  dailyreturns<- rlnorm(n=tradingdays,meanlog,stdlog)
  mean(dailyreturns)
  sd(dailyreturns)
  plot(density(dailyreturns))
  
  # Generating Daily Price from Returns 
  STK_PRC <- 10
  r <-dailyreturns
  stock_prices <- c()
  for (i in seq(tradingdays))
  {
    STK_PRC <- STK_PRC*(1+r[i])
    stock_prices <- c(stock_prices,STK_PRC)
    print(stock_prices)
  }

#3. Creating a Portfolio of 5 Stocks 


