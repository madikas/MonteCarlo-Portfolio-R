rm(list = ls())

library(tidyverse)
library(ggplot2)
library(RiskPortfolios)

# Simulating the returns 

#real cubic root of a number (r returns NaN...)
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
  uquad_ret <- as.data.frame(df_uquad[,2])
  return(uquad_ret)
}

# Simulatin daily returns scenario with mixed dist.
set.seed(1995)
ndays <- 253

#Uquad distribution 
a <-runif(1 , min= 1 , max = 10)









