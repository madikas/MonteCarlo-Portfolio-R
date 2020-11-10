library(ggplot2)
library(RiskPortfolios)
library(timeSeries)
library(fPortfolio)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(TTR)
library(ggpubr)
library(kableExtra)
library(DescTools)
# Local functions
cbrt <- function (x) {
  sign(x) * abs(x)^(1/3)
}
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
generate_distribution <- function(number, type, volatility) {
  tradingdays<-number
  m <- runif(1, min= 0.001, max = 0.05)
  if(volatility == "high") {
    s <- runif(1, min=0.5, max = 1)
  } else if(volatility == "low") {
    s <- runif(1, min=0.000001, max = 0.01)
  } else {
    s <- runif(1, min=0.000001, max = 0.05)
  }
  up <- runif(1 , min=0.03 , max = 0.1)
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
  } else if(type == "uquad") {
    dailyreturns = ruquad(up ,tradingdays)
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
                          generate_distribution(tradingdays, "normal",""))
    }
  } else if(scenario == "mix") {
    for (ticker in tickers) {
      if(ticker == "B") {
        stockPrices = cbind(stockPrices,
                            generate_distribution(tradingdays, "exponential",""))
      } else if(ticker == "C") {
        stockPrices = cbind(stockPrices,
                            generate_distribution(tradingdays, "lognormal",""))
      } else if(ticker == "D") {
        stockPrices = cbind(stockPrices,
                            generate_distribution(tradingdays, "uquad",""))
      } else {
        stockPrices = cbind(stockPrices,
                            generate_distribution(tradingdays, "normal",""))
      }
    }
  } else if(scenario=="low volatility") {
    for (ticker in tickers) {
      stockPrices = cbind(stockPrices,
                          generate_distribution(tradingdays, "normal", "low"))
    }
  } else if(scenario=="high volatility") {
    for (ticker in tickers) {
      stockPrices = cbind(stockPrices,
                          generate_distribution(tradingdays, "normal", "high"))
    }
  }else {
    stop("No specified scenario")
  }
  colnames(stockPrices) = tickers
  return(stockPrices)
}

#Rate of change calculation
D_ret = function(x) na.omit(ROC(x, type="discrete"))
#Monte Carlo Simulation for portfolios based on tradingdays, numsimulations and
#scenarios
portfolio_simulation <- function(simulations,tradingdays, scenario, figindex) {
  portfolio_returns = data.frame(meanvarReturn=NA, meanvarVariance=NA, meanvarSharpe=NA,  
                                 minvarReturn= NA, minvarVariance=NA, minvarSharpe=NA,
                                 maxdivReturn=NA, maxdivVariance=NA, maxdivSharpe=NA,
                                 maxdecReturn=NA, maxdecVariance=NA, maxdecSharpe=NA,
                                 equalweightsReturn=NA, equalweightsVariance=NA, equalweightsSharpe=NA)
  newrow = NULL
  for (i in seq(simulations)) {
    stockPrices = scenario_stock_generation(tradingdays, scenario)
    stockReturns = apply(stockPrices, 2, D_ret)
    stockReturns = as.timeSeries(stockReturns)
    meanReturns=as.matrix(colMeans(stockReturns))
    covariancematrix=as.matrix(cov(stockReturns)*tradingdays)
    meanvar=as.matrix(optimalPortfolio(covariancematrix,meanReturns, control=list(type='mv',constraint='lo')))
    minvar=as.matrix(optimalPortfolio(covariancematrix,meanReturns, control=list(type='minvol',constraint='lo')))
    maxdiv=as.matrix(optimalPortfolio(covariancematrix,meanReturns, control=list(type='maxdiv',constraint='lo')))
    maxdec=as.matrix(optimalPortfolio(covariancematrix,meanReturns, control=list(type='maxdec',constraint='lo')))
    equalweights = as.matrix(c(0.2,0.2,0.2,0.2,0.2))
    meanvarReturn =  t(meanvar) %*% meanReturns
    meanvarVariance =  t(meanvar) %*% covariancematrix %*% meanReturns
    meanvarSharpe = meanvarReturn / sqrt(meanvarVariance)
    minvarReturn =  t(minvar) %*% meanReturns
    minvarVariance =  t(minvar) %*%  covariancematrix %*% meanReturns
    minvarSharpe = minvarReturn / sqrt(minvarVariance)
    maxdivReturn =  t(maxdiv) %*% meanReturns
    maxdivVariance =  t(maxdiv) %*%  covariancematrix %*% meanReturns
    maxdivSharpe = maxdivReturn / sqrt(maxdivVariance)
    maxdecReturn =  t(maxdec) %*% meanReturns
    maxdecVariance =  t(maxdec) %*%  covariancematrix %*% meanReturns
    maxdecSharpe = maxdecReturn / sqrt(maxdecVariance)
    equalweightsReturn =  t(equalweights) %*% meanReturns
    equalweightsVariance =  t(equalweights) %*%  covariancematrix %*% meanReturns
    equalweightsSharpe = equalweightsReturn / sqrt(equalweightsVariance)
    newrow = c(meanvarReturn,meanvarVariance, meanvarSharpe, minvarReturn, minvarVariance, minvarSharpe,
               maxdivReturn,maxdivVariance, maxdivSharpe, maxdecReturn, maxdecVariance, maxdecSharpe,
               equalweightsReturn, equalweightsVariance, equalweightsSharpe)
    portfolio_returns = rbind(portfolio_returns[1:i,],newrow,portfolio_returns[-(1:i),])
    if (i==1) {
      stockReturns1 = as.data.frame(stockReturns)
      stockPrices1 = as.data.frame(stockPrices)
      stockPrices1 = cbind(days=as.numeric(rownames(stockPrices1)) , stockPrices1)
      
      p1 <- ggplot(data = stockReturns1 , aes(x=A))+
        geom_histogram(color="black" , fill="white")+
        geom_vline(aes(xintercept=mean(A)), color="blue", linetype="dashed",size=1) +
        labs(title = "Distribution of the daily returns of stock A" , x="Daily returns" , y="Frequency")+
        theme(plot.title = element_text(face="bold" , hjust = 0.5))
      
      p2 <- ggplot(data = stockReturns1 , aes(x=B))+
        geom_histogram(color="black" , fill="white")+
        geom_vline(aes(xintercept=mean(B)), color="blue", linetype="dashed",size=1) +
        labs(title = "Distribution of the daily returns of stock B" , x="Daily returns" , y="Frequency")+
        theme(plot.title = element_text(face="bold" , hjust = 0.5))
      
      p3 <- ggplot(data = stockReturns1 , aes(x=C))+
        geom_histogram(color="black" , fill="white")+
        geom_vline(aes(xintercept=mean(C)), color="blue", linetype="dashed",size=1) +
        labs(title = "Distribution of the daily returns of stock C" , x="Daily returns" , y="Frequency")+
        theme(plot.title = element_text(face="bold" , hjust = 0.5))
      
      p4 <- ggplot(data = stockReturns1 , aes(x=D))+
        geom_histogram(color="black" , fill="white")+
        geom_vline(aes(xintercept=mean(D)), color="blue", linetype="dashed",size=1) +
        labs(title = "Distribution of the daily returns of stock D" , x="Daily returns" , y="Frequency")+
        theme(plot.title = element_text(face="bold" , hjust = 0.5))
      
      p5 <- ggplot(data = stockReturns1 , aes(x=A))+
        geom_histogram(color="black" , fill="white")+
        geom_vline(aes(xintercept=mean(A)), color="blue", linetype="dashed",size=1) +
        labs(title = "Distribution of the daily returns of stock E" , x="Daily returns" , y="Frequency")+
        theme(plot.title = element_text(face="bold" , hjust = 0.5))
      
      h2 <- ggplot()+
        geom_line(data=stockPrices1 , aes(x=days ,y=A ,color = "darkred")) +
        geom_line(data=stockPrices1 , aes(x=days ,y=B ,color = "blue")) +
        geom_line(data=stockPrices1 , aes(x=days ,y=C ,color = "green")) +
        geom_line(data=stockPrices1 , aes(x=days ,y=D, color = "orange")) +
        geom_line(data=stockPrices1 , aes(x=days ,y=E, color = "yellow")) +
        scale_color_discrete(name = "Stock Prices" , labels=c("A","B","C","D","E"))+
        labs(title = "Simulated stock price over the period of 253 trading days" , x="Trading day" , y="Stock price")+
        theme(plot.title = element_text(face="bold", hjust = 0.5))
      
    }
  }
  figure<-ggarrange(p1,p2,p3,p4,p5,h2,
                     ncol=3,nrow=2)
  annotate_figure(figure,
                  top = text_grob(paste("Portfolio of Stocks ",scenario) , color = "black", face = "bold", size = 14),
                  fig.lab = paste("Figure ",figindex), fig.lab.face = "bold")
  results <- list(portfolio_returns , stockPrices, figure)
  return(results)
}