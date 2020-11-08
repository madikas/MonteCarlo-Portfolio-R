library(RiskPortfolios)
library(timeSeries)
library(fPortfolio)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)

# Local functions
cbrt <- function (x) {
sign(x) * abs(x)^(1/3)
}

#
D_ret <- function(x) na.omit(ROC(x , type ="discrete"))

#U-quadratic shape distributed returns (with paramets a = -b) for 253 days
ruquad <- function(upper_bound , nsim) {
  b <- upper_bound
  a <- -b
  Beta <- (b+a)/2
  Alpha <- 12/((b-a)^3)
  unif <- runif(nsim , min = 0 , max = 1)
  df_unif <- as.data.frame(unif)
  df_uquad <- within(df_unif ,x <- (((3*unif)/Alpha)-((Beta-a)^3)))
  df_uquad <- apply(df_uquad , 2 , cbrt)
  uquad_ret <- df_uquad[,2]
  return(uquad_ret)
}


#Distribution generation of stock returns for a specified number of days 
#and type(normal, lognormal, exponential)
generate_distribution <- function(number, type) {
  tradingdays<-number
  m <- runif(1, min= -0.001, max = 0.001)
  s <- runif(1, min=0.05, max = 0.2)
  up <- runif(1 , min=0.03 , max = 0.1)
  lambda = runif(1, min=25, max=50)
  dailyreturns = c()
  minimumPrice = 0.0001
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
  } else if(type == "uquad") {
    dailyreturns = ruquad(up ,tradingdays)
    mean(dailyreturns)
    sd(dailyreturns)
  } 
  else {
    stop("No specified distribution")
  }
  # Generating Daily Price from Returns 
  STK_PRC <- runif(1, min = minimumPrice, max = maximumPrice)
  r <-dailyreturns
  stock_prices <- c()
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
       } else if(ticker == "D") {
          stockPrices = cbind(stockPrices,
                              generate_distribution(tradingdays, "uquad"))
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

simulations = 10
tradingdays = 253
scenario = "allnormal"

portfolio_returns = NULL


portfolio_returns = NULL
for (i in seq(simulations)) {
  stockPrices = scenario_stock_generation(tradingdays, scenario)
  print(stockPrices)
  stockReturns <- apply(stockPrices, 2 , D_ret)
  print(stockReturns)
  meanReturns=colMeans(stockReturns)
  covariancematrix=cov(stockReturns)*tradingdays
  print(covariancematrix)
  meanvar=optimalPortfolio(covariancematrix,meanReturns, control=list(type='mv',constraint='lo'))
  print(meanvar)
  minvar=optimalPortfolio(covariancematrix,meanReturns, control=list(type='minvol',constraint='lo'))
  maxdiv=optimalPortfolio(covariancematrix,meanReturns, control=list(type='maxdiv',constraint='lo'))
  maxdec=optimalPortfolio(covariancematrix,meanReturns, control=list(type='maxdec',constraint='lo'))    
  portfolio_returns = c(portfolio_returns, maxdec)
}

