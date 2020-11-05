library(quantmod) 
library(timeSeries)
library(fPortfolio)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library ( plotly)

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
                                

# Calculate mean returns and covariance matrix for all 10 stocks 

meanReturns=colMeans(stockReturns)
covariancematrix=cov(stockReturns)*253

#Monte Carlo simulation with 5000 simulations of different weights for our portfolio

#Part 1 - Creating Empty Vectors and Matrix for 
  #Simulation of 5000 different portfolios with different weights
  
  num_simulations=5000
  
  #Matrix to store weights 
  all_wts= matrix(nrow=num_simulations, ncol=length(tickers))
  
  #Vector to store portfolio returns
  port_returns=vector('numeric',length=num_simulations)
  
  #Vector to store std dev
  port_risk=vector('numeric',length=num_simulations)
  
  #Vector to store SharpeRatio
  sharpe_ratio=vector('numeric',length=num_simulations)
  
#Part 2 - MonteCarlo Simulation
  
  set.seed(500)
  for (i in seq_along(port_returns)){
    weights<-runif(length(tickers))
    weights<-weights/sum(weights)
    
    all_wts[i,]<- weights
    
    port_ret<- sum(weights*meanReturns)
    port_ret <- (port_ret+1)^253 -1
    
    port_returns[i]<-port_ret
    
    port_sd<-sqrt(t(weights)%*%(covariancematrix%*%weights))
    port_risk[i]<-port_sd
    
    sr<-port_ret/port_sd
   sharpe_ratio[i]<-sr
    
  }
    
#Storing all the values in a table 
  
  portfolio_values=data.frame(all_wts,Return=port_returns,Risk=port_risk,SharpeRatio=sharpe_ratio)
  
  #Looking at the first values in our matrix
  head(portfolio_values)
  
  #Important Portfolios
  
  min_var <- portfolio_values[which.min(portfolio_values$Risk),]
  max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]


#Plot the Efficient Frontier
  
  efficientfrontier <- portfolio_values %>%
    ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
    geom_point() +
    theme_classic() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = 'Annualized Risk',
         y = 'Annualized Returns',
         title = " Efficient Frontier") +
    geom_point(aes(x = Risk,
                   y = Return), data = min_var, color = 'red') +
    geom_point(aes(x = Risk,
                   y = Return), data = max_sr, color = 'red') +
    annotate('text', x = 0.24, y = 0.25, label = "Tangency Portfolio") +
    annotate('text', x =0.21 , y =0.05 , label = "Minimum variance portfolio") +
    annotate(geom = 'segment', x = 0.19, xend = 0.21,  y = 0.09, 
             yend = 0.07, color = 'red', arrow = arrow(type = "open")) +
    annotate(geom = 'segment', x = 0.212, xend = 0.23,  y = 0.23, 
             yend = 0.235, color = 'red', arrow = arrow(type = "open"))
  
  ggplotly(efficientfrontier)
