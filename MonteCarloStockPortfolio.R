library(ggplot2)
library(RiskPortfolios)
library(timeSeries)
library(fPortfolio)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
source("Simulation.R")
#stock = scenario_stock_generation(253,"allnormal")
#stock

#MonteCarlo simulation of portfolio strategies 
#given different parameters(combination of distributions)
simulations = 10
tradingdays = 253
scenario = "allnormal"

portfolio_returns = NULL
for (i in seq(simulations)) {
  stockPrices = scenario_stock_generation(tradingdays, scenario)
  print(stockPrices)
  stockReturns = na.omit(ROC(stockPrices, type="discrete"))
  stockReturns = as.timeSeries(stockReturns)
  meanReturns=colMeans(stockReturns)
  covariancematrix=cov(stockReturns)*tradingdays
  print(covariancematrix)
  meanvar=optimalPortfolio(covariancematrix,meanReturns, control=list(type='mv',constraint='lo'))
  minvar=optimalPortfolio(covariancematrix,meanReturns, control=list(type='minvol',constraint='lo'))
  maxdiv=optimalPortfolio(covariancematrix,meanReturns, control=list(type='maxdiv',constraint='lo'))
  maxdec=optimalPortfolio(covariancematrix,meanReturns, control=list(type='maxdec',constraint='lo'))    
  portfolio_returns = c(portfolio_returns, maxdec)
}