
library(tidyverse)
library(quantmod)

# The stocks used for the simulation
tickers <- c("AMZN" ,"GOOGL" , "PFE" , "JPM" ,"CVX", "BA", "FAST", "INTC","T", "NWSA")

# The data download from Yahoo finance 
getSymbols(tickers, from="2015-10-30", to="2020-10-31" , auto.assign = TRUE )

# Using closing price to calculate the return
closing <- cbind(AMZN$AMZN.Close , GOOGL$GOOGL.Close , PFE$PFE.Close
                 , JPM$JPM.Close , CVX$CVX.Close , 
                 BA$BA.Close , FAST$FAST.Close, INTC$INTC.Close, T$T.Close,
                 NWSA$NWSA.Close)

# two functions to be able to use apply on columns
D_ret <- function(x) ROC(x , type ="discrete")
D_ave <- function(x) mean(x) 

#Daily returns, average, variance and covariance matrix
Returns <- apply(closing , 2 , D_ret)
Returns <- Returns[-c(1),]
Av_ret <-as.matrix(colMeans(Returns))
Var_cov <- cov(Returns)

# The following code is a function that returns a graph of the efficient frontier when you
# provide a covariance matrix of different returns , a specific rate of return.
# Optimization problem to be solved : Min of portfolio variance in function of weights
# To solve this problem, a specific return needs to be specified here we will simply use 8% 

Porfolio_variance <- function(Cov_matrix , average_return ,rate_return) {
  
  N <- nrow(Cov_matrix)
  
  Aug <- as.matrix(Cov_matrix)
  
  V_1 <- as.matrix(c(rep(1,N)))
  
  Aug_1 <- cbind(Aug ,average_return, V_1 )
  
  average_return_t <- as.matrix(t(average_return))
  
  average_return_t <- c(average_return_t , 0 , 0)
  
  V_1_t <- as.matrix(t(V_1))
  
  V_1_t <- c(V_1_t , 0, 0 )
  
  Augmented_matrix_1 <- rbind(Aug_1 ,average_return_t,V_1_t)
  
  inverse <- solve( Augmented_matrix_1)
 
  B_vect <- as.matrix(c(rep(0, N) ,rate_return , 1 ))
  
  wt <- inverse %*% B_vect
  
  stock_wt <- wt[1:N]
  
  stock_wt_t <- t(stock_wt)
  
  port_return <- stock_wt_t %*% average_return
  
  port_variance <- stock_wt_t %*% Cov_matrix %*% stock_wt
  
  port_std <- sqrt(port_variance)
  
  return(port_std)
}

Frontier <- function(lower_bound_r,upper_bound_r, number ,Cov_matrix , average_return) {
  
  increments = (upper_bound_r - lower_bound_r)/number
  
  ret <- seq(lower_bound_r , upper_bound_r , increments)
  
  rows = number + 1
  
  df <- data.frame(Expected_return = ret , Standard_deviation= 1:rows)
  
  for (i in 1:rows ) {
    df[i , 2] <- Porfolio_variance(Cov_matrix , average_return , df[i,1])
  }
  
  return(df)
}


# Test with a preset portfolio to make sure the frontier makes sense :
Var_cov_2 <- matrix(c(0.4 , 0.03 , 0.02 , 0.00 , 0.03 , 0.2 , 0.00 , -0.06 ,
                      0.02 , 0.00 , 0.3 , 0.03 , 0.00 , -0.06 , 0.03 , 0.1) , ncol= 4)

Av_ret_2 <- matrix(c(0.06 , 0.05 , 0.07 , 0.08))

Test_front <- Frontier(0.03 , 0.09 , 13 , Var_cov_2 , Av_ret_2)

Front_1 <- Frontier(0.01 , 0.15 , 30 , Var_cov , Av_ret)

plot_1 <- ggplot(data=Test_front , aes(x =Standard_deviation , y =Expected_return )) +
  geom_point() +
  ggtitle("Efficient Frontier") +
  xlab("Portfolio standard deviation") +
  ylab("Portfolio Expected return")
plot_1

plot_2 <- ggplot(data=Front_1 , aes(x =Standard_deviation , y =Expected_return )) +
  geom_point() +
  ggtitle("Efficient Frontier") +
  xlab("Portfolio standard deviation") +
  ylab("Portfolio Expected return")
plot_2
