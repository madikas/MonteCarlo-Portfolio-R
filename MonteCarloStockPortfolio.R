source("Simulation.R")
#MonteCarlo simulation of portfolio strategies 
#given different parameters(combination of distributions)
simulations = 1000
tradingdays = 253
scenario = "allnormal"

portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario)
portfolio_returns
colMeans(na.omit(portfolio_returns))

scenario = "mix"
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario)
portfolio_returns
colMeans(na.omit(portfolio_returns))

scenario = "low volatility"
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario)
portfolio_returns
colMeans(na.omit(portfolio_returns))

scenario = "high volatility"
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario)
portfolio_returns
colMeans(na.omit(portfolio_returns))