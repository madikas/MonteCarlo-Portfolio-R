# Testing Portfolio Optimization Strategies using Monte Carlo Simulation Methodology
HEC Montreal Statistical Learning MATH 60603A

Authors: Madi Kassymbekov; Abderezzak Amimer; Stephanie Vu

## Abstract
Portfolio optimization consists of determining the optimal proportions of total capital to invest in each particular asset in the portfolio. Many different 
strategies exist to define optimal weights in a portfolio however, it remains hard for portfolio managers to decide which optimization strategy is best suited for 
a given set of risky assets. The optimal portfolio strategies examined in this paper are the mean variance, minimum variance, max diversification and 
max decorrelation portfolios. The goal of this experiment is to determine what investment strategy performs best depending on the composition of stocks in a given
portfolio. Four different scenarios are tested where portfolios are composed of stocks with different daily return distributions. 
The portfolio scenarios were as follows: a portfolio of 5 stocks that are normally distributed, a portfolio of 5 normally distributed stocks with low volatility, 
a portfolio of 5 normally distributed stocks with high volatility and a portfolio of 5 different stocks with 4 different distributions. 
First, yearly stock returns are generated for each stock depending on the specified distribution given a set of constraints. 
Subsequently,  mean return and covariance matrix are generated and stored for each portfolio. A Monte Carlo simulation is utilized in order to generate 1000 
different sets of portfolios for each scenario. In order to evaluate the portfolio’s performance, a confidence interval of the expected return is estimated as well
as it’s variance and Sharpe ratio. Based on Monte Carlo simulations, the mean variance portfolio strategy consistently outperformed the remaining strategies under
the constraints set in this study. Applying an optimization strategy is found to be superior to equally distributing portfolio weights in all scenarios but the high
daily returns volatility.

## Research Question
The goal of this experiment is to determine what investment strategy performs best depending on the composition of stocks in a given portfolio. Four different scenarios are tested where portfolios are composed of stocks with different distributions. Such analysis will allow financial managers to easily choose the most favorable optimization strategy to implement when performing portfolio optimization depending on the distribution of stocks held in the portfolio.

## Methodology

To evaluate the performance of each portfolio optimization strategy, a set of 5 stock daily returns are randomly generated for a period of 253 trading days. A set of 4 different scenarios were evaluated in order to determine what portfolio compositions perform best under each optimization strategy. The four portfolio compositions examined are as follows:

+ A portfolio of 5 stocks that are normally distributed 

+ A portfolio of 5 normally distributed stocks with low volatility

+ A portfolio of 5 normally distributed stocks with high volatility

+ A portfolio of 5 different stocks with 4 different distributions

A set of daily stock returns is randomly generated for five stocks in a given portfolio. The generation of daily stock return differs depending on the specified distributions chosen from a normal distribution, lognormal distribution, exponential or u-quadratic distribution.

Normal distribution, lognormal and exponential distribution were randomly generated using functions in R. While the U-quadratic distribution was derived from a variable transformation. 

Once returns have been generated, the daily stock prices were derived from the returns and stored as a time series. Subsequently, daily returns are then derived using the ROC function in R for each stock in the portfolio. Once the daily returns have been derived a vector for the mean returns of each stock in the portfolio is stored. Additionally, a covariance matrix is also formulated for the given stocks in the specified portfolio and stored.

Once expected returns and covariance matrix is calculated for the given portfolio optimal portfolio weights are calculated for each optimization strategy using the optimalPortfolio function in R. This function takes as an input the mean returns vector as well as the corresponding covariance matrix for the given portfolio. The output is a set of optimal weights for each optimization strategy.A Monte Carlo simulation is utilized in order to generate 1000 different sets of portfolios for each scenario. For each portfolio, a confidence interval of the expected return is estimated as well as it’s variance and Sharpe ratio in order to allow evaluation of the portfolio’s performance.The strategies are also compared to a benchmark consisting of a portfolio with equal weight in each asset.

## Conclusion

The Monte Carlo simulation study shows that composition of stock portfolios does not have an impact on selection of the best portfolio optimization strategy. For all scenarios tested, the mean variance portfolio strategy is shown to consistently outperform all other portfolio optimization strategies. This optimization strategy yielded both the highest return and Sharpe ratio for all scenarios. This indicates that by using the mean variance strategy, investors can ensure they obtain the best return to risk ratio. Moreover, applying an optimization strategy is found to be superior to equally distributing portfolio weights in all scenarios with the exception of the high daily returns volatility. As such, utilization of portfolio optimization strategies is recommended for any portfolio manager.

## References

Choueifaty, Y., & Coignard, Y. (2008). Toward Maximum Diversification. *The Journal of Portfolio Management*, 35(1), 40-51. doi:10.3905/jpm.2008.35.1.40

Christoffersen, Peter and Errunza, Vihang R. and Jacobs, Kris and Jin, Xisong.(2010). *Is the Potential for International Diversification Disappearing?* SSRN: https://ssrn.com/abstract=1573345 or http://dx.doi.org/10.2139/ssrn.1573345

Clarke, R., Silva, H. D., & Thorley, S. (2010). Minimum-Variance Portfolio Composition. *The Journal of Portfolio Management*, 101116223821055. doi:10.3905/jpm.2010.2010.1.009

Clarke, R., Silva, H. D., & Thorley, S. (2012). Risk Parity, Maximum Diversification, and Minimum Variance: An Analytic Perspective. *SSRN Electronic Journal*. doi:10.2139/ssrn.1977577

Markowitz, H. (1952). Portfolio Selection. *The Journal of Finance*, 7(1), 77. doi:10.2307/2975974

Christoffersen, Peter and Errunza, Vihang R. and Jacobs, Kris and Jin, Xisong.(2010). *Is the Potential for International Diversification Disappearing?* SSRN: https://ssrn.com/abstract=1573345 or http://dx.doi.org/10.2139/ssrn.1573345
