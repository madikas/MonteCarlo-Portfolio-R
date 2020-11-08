library(RiskPortfolios)
library(timeSeries)
library(fPortfolio)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
#Distribution generation of stock returns for a specified number of days 
#and type(normal, lognormal, exponential)
generate_distribution <- function(number, type) {
  tradingdays<-number
  m <- runif(1, min= 0.001, max = 0.05)
  s <- runif(1, min=0.000001, max = 0.05)
  lambda = runif(1, min=25, max=50)
  dailyreturns = c()
  minimumPrice = 0.001
  maximumPrice = 15
  if(type == "normal") {
    dailyreturns <-rnorm(n=tradingdays,m,s)
    mean(dailyreturns)
    sd(dailyreturns)
  } else if(type == "lognormal") {
    meanlog<-log(m^2/sqrt(s^2+m^2))
    stdlog<- sqrt(log(1+(s^2/m^2)))
    #Vector of daily returns 
    dailyreturns<- rlnorm(n=tradingdays,meanlog,stdlog)
    mean(dailyreturns)
    sd(dailyreturns)
  } else if(type == "exponential") {
    maximumPrice = 0.15
    dailyreturns = rexp(n=tradingdays, rate = lambda )
    mean(dailyreturns)
    sd(dailyreturns)
  } else {
    stop("No specified distribution")
  }
  # Generating Daily Price from Returns 
  STK_PRC <- runif(1, min = minimumPrice, max = maximumPrice)
  r <-dailyreturns
  stock_prices = c()
  for (i in seq(tradingdays))
  {
    STK_PRC <- STK_PRC*(1+r[i])
    stock_prices<- c(stock_prices,STK_PRC)
    
  }
  return(stock_prices)
}

#Stock distribution generation based on scenarios
scenario_stock_generation <- function(tradingdays, scenario) {
  tickers = c("A", "B", "C", "D", "E")
  stockPrices = NULL
  if(scenario == "allnormal") {
    for (ticker in tickers) {
      stockPrices = cbind(stockPrices,
                          generate_distribution(tradingdays, "normal"))
    }
  } else if(scenario == "mix") {
    for (ticker in tickers) {
      if(ticker == "B") {
        stockPrices = cbind(stockPrices,
                            generate_distribution(tradingdays, "exponential"))
      } else if(ticker == "C") {
        stockPrices = cbind(stockPrices,
                            generate_distribution(tradingdays, "lognormal"))
      } else {
        stockPrices = cbind(stockPrices,
                            generate_distribution(tradingdays, "normal"))
      }
    }
  } else {
    stop("No specified scenario")
  }
  colnames(stockPrices) = tickers
  return(stockPrices)
}
