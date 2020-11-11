source("Simulation.R")
#Plots for portfolio Returns 
library(ggpubr)
library(kableExtra)
library(DescTools)
#MonteCarlo simulation of portfolio strategies 
#given different parameters(combination of distributions)
simulations = 1000
tradingdays = 253

scenario = "allnormal"
set.seed(123)
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario, "1")
price_norm = portfolio_returns[[9]]
allnorm_ret=na.omit(portfolio_returns[[1]])
allnorm_mean=round(colMeans(na.omit(portfolio_returns[[1]])),6)

scenario = "mix"
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario, "2")
p_norm = portfolio_returns[[3]]
p_exp = portfolio_returns[[4]]
p_log = portfolio_returns[[5]]
p_uquad = portfolio_returns[[6]]
price_mix = portfolio_returns[[9]]
mix_ret=na.omit(portfolio_returns[[1]])
mix_ret_mean=round(colMeans(na.omit(portfolio_returns[[1]])),6)

scenario = "low volatility"
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario, "3")
p_low = portfolio_returns[[7]]
price_low = portfolio_returns[[9]]
lowvol_ret=na.omit(portfolio_returns[[1]])
lowvol_mean=round(colMeans(na.omit(portfolio_returns[[1]])),6)

scenario = "high volatility"
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario, "4")
p_high = portfolio_returns[[8]]
price_high = portfolio_returns[[9]]
highvol_ret=na.omit(portfolio_returns[[1]])
highvol_mean=round(colMeans(na.omit(portfolio_returns[[1]])),6)


#All Normal
a1<- ggplot(allnorm_ret, aes(x=meanvarReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(meanvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Mean Variance",x="Return", y = "Frequency")+
  theme_classic()

a2<- ggplot(allnorm_ret, aes(x=minvarReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(minvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Min Variance",x="Return", y = "Frequency")+
  theme_classic()

a3<- ggplot(allnorm_ret, aes(x=maxdivReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(maxdivReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Diversification",x="Return", y = "Frequency")+
  theme_classic()

a4<-ggplot(allnorm_ret, aes(x=maxdecReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(maxdecReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Decorrelation",x="Return", y = "Frequency")+
  theme_classic()

a5<-ggplot(allnorm_ret, aes(x=equalweightsReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(equalweightsReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Equal Weights",x="Return", y = "Frequency")+
  theme_classic()


figure1 <- ggarrange(a1,a2,a3,a4,a5,
                   ncol=3,nrow=2)

figure1 <- annotate_figure(figure1,
                top = text_grob("Portfolio of All Normal Stocks", color = "black", face = "bold", size = 12),
                fig.lab = "Figure 3", fig.lab.face = "bold")


#Mixed
b1<- ggplot(mix_ret, aes(x=meanvarReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(meanvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Mean Variance",x="Return", y = "Frequency")+
  theme_classic()

b2<- ggplot(mix_ret, aes(x=minvarReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(minvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Min Variance",x="Return", y = "Frequency")+
  theme_classic()

b3<- ggplot(mix_ret, aes(x=maxdivReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(maxdivReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Diversification",x="Return", y = "Frequency")+
  theme_classic()

b4<-ggplot(mix_ret, aes(x=maxdecReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(maxdecReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Decorrelation",x="Return", y = "Frequency")+
  theme_classic()

b5<-ggplot(mix_ret, aes(x=equalweightsReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(equalweightsReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Equal Weights",x="Return", y = "Frequency")+
  theme_classic()


figure2 <-ggarrange(b1,b2,b3,b4,b5,
                   ncol=3,nrow=2)

figure2 <-annotate_figure(figure2,
                top = text_grob("Portfolio of Mixed Distribution Stocks", color = "black", face = "bold", size = 12),
                fig.lab = "Figure 6", fig.lab.face = "bold")



#Low Volatility

c1<- ggplot(lowvol_ret, aes(x=meanvarReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(meanvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Mean Variance",x="Return", y = "Frequency")+
  theme_classic()

c2<- ggplot(lowvol_ret, aes(x=minvarReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(minvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Min Variance",x="Return", y = "Frequency")+
  theme_classic()

c3<- ggplot(lowvol_ret, aes(x=maxdivReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(maxdivReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Diversification",x="Return", y = "Frequency")+
  theme_classic()

c4<-ggplot(lowvol_ret, aes(x=maxdecReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(maxdecReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Decorrelation",x="Return", y = "Frequency")+
  theme_classic()

c5<-ggplot(lowvol_ret, aes(x=equalweightsReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(equalweightsReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Equal Weights",x="Return", y = "Frequency")+
  theme_classic()


figure3 <-ggarrange(c1,c2,c3,c4,c5,
                   ncol=3,nrow=2)

figure3 <-annotate_figure(figure3,
                top = text_grob("Portfolio of Low Volatility Stocks", color = "black", face = "bold", size = 13),
                fig.lab = "Figure 4", fig.lab.face = "bold")


#High volatility

d1<- ggplot(highvol_ret, aes(x=meanvarReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(meanvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Mean Variance",x="Return", y = "Frequency")+
  theme_classic()

d2<- ggplot(highvol_ret, aes(x=minvarReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(minvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Min Variance",x="Return", y = "Frequency")+
  theme_classic()

d3<- ggplot(highvol_ret, aes(x=maxdivReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(maxdivReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Diversification",x="Return", y = "Frequency")+
  theme_classic()

d4<-ggplot(highvol_ret, aes(x=maxdecReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(maxdecReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Decorrelation",x="Return", y = "Frequency")+
  theme_classic()

d5<-ggplot(highvol_ret, aes(x=equalweightsReturn)) +
  geom_histogram(fill="white", color="black", bins=20)+
  geom_vline(aes(xintercept=mean(equalweightsReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Equal Weights",x="Return", y = "Frequency")+
  theme_classic()


figure4 <-ggarrange(d1,d2,d3,d4,d5,
                   ncol=3,nrow=2)

figure4 <-annotate_figure(figure4,
                top = text_grob("Portfolio of High Volatility Stocks", color = "black", face = "bold", size = 12),
                fig.lab = "Figure 5 ", fig.lab.face = "bold")

#Confidence Intervals

#All Normal

allnorm_CI=MeanCI(allnorm_ret$meanvarReturn, conf.level = 0.95,na.rm=TRUE)
allnorm_CI=rbind(allnorm_CI,MeanCI(allnorm_ret$minvarReturn, conf.level = 0.95,na.rm=TRUE),
                 MeanCI(allnorm_ret$maxdivReturn, conf.level = 0.95,na.rm=TRUE),
                 MeanCI(allnorm_ret$maxdecReturn, conf.level = 0.95,na.rm=TRUE),
                 MeanCI(allnorm_ret$equalweightsReturn, conf.level = 0.95,na.rm=TRUE))
allnorm_CI=round(allnorm_CI,6)

#Mixed
mix_CI=MeanCI(mix_ret$meanvarReturn, conf.level = 0.95,na.rm=TRUE)
mix_CI=rbind(mix_CI,MeanCI(mix_ret$minvarReturn, conf.level = 0.95,na.rm=TRUE),
             MeanCI(mix_ret$maxdivReturn, conf.level = 0.95,na.rm=TRUE),
             MeanCI(mix_ret$maxdecReturn, conf.level = 0.95,na.rm=TRUE),
             MeanCI(mix_ret$equalweightsReturn, conf.level = 0.95,na.rm=TRUE))
mix_CI=round(mix_CI,6)

#Low Volatility
low_CI=MeanCI(lowvol_ret$meanvarReturn, conf.level = 0.95,na.rm=TRUE)
low_CI=rbind(low_CI,MeanCI(lowvol_ret$minvarReturn, conf.level = 0.95,na.rm=TRUE),
             MeanCI(lowvol_ret$maxdivReturn, conf.level = 0.95,na.rm=TRUE),
             MeanCI(lowvol_ret$maxdecReturn, conf.level = 0.95,na.rm=TRUE),
             MeanCI(lowvol_ret$equalweightsReturn, conf.level = 0.95,na.rm=TRUE))
low_CI=round(low_CI,6)

#High Volatility
high_CI=MeanCI(highvol_ret$meanvarReturn, conf.level = 0.95,na.rm=TRUE)
high_CI=rbind(high_CI,MeanCI(highvol_ret$minvarReturn, conf.level = 0.95,na.rm=TRUE),
              MeanCI(highvol_ret$maxdivReturn, conf.level = 0.95,na.rm=TRUE),
              MeanCI(highvol_ret$maxdecReturn, conf.level = 0.95,na.rm=TRUE),
              MeanCI(highvol_ret$equalweightsReturn, conf.level = 0.95,na.rm=TRUE))
high_CI=round(high_CI,6)

#Tables 

#All norm
t1=data.frame(rbind(allnorm_mean[1:3],allnorm_mean[4:6],allnorm_mean[7:9],allnorm_mean[10:12],allnorm_mean[13:15]))
t1=cbind(t1,allnorm_CI[,2:3])
rownames(t1)<-c("Mean Variance","Min Variance","Max Diversification","Max Decorrelation","Equal Weights")
colnames(t1)<- c("Return (%)","Variance (%)","Sharpe", "Lower CI","Upper CI")
t1[1:2]<-t1[1:2]*100
t1[4:5]<-t1[4:5]*100
t1


t1 <- t1 %>%
  kbl(caption = "Portfolio of All Normal Stocks") %>%
  kable_styling(full_width = F, html_font = "Cambria", latex_options = "hold_position")

#Mixed
t2=data.frame(rbind(mix_ret_mean[1:3],mix_ret_mean[4:6],mix_ret_mean[7:9],mix_ret_mean[10:12],mix_ret_mean[13:15]))
t2=cbind(t2,mix_CI[,2:3])
rownames(t2)<-c("Mean Variance","Min Variance","Max Diversification","Max Decorrelation","Equal Weights")
colnames(t2)<- c("Return (%)","Variance (%)","Sharpe", "Lower CI","Upper CI")
t2[1:2]<-t2[1:2]*100
t2[4:5]<-t2[4:5]*100
t2

t2<- t2 %>%
  kbl(caption = "Portfolio of Mixed Distribution Stocks") %>%
  kable_styling(full_width = F, html_font = "Cambria", latex_options = "hold_position")


#Low Volatility
t3=data.frame(rbind(lowvol_mean[1:3],lowvol_mean[4:6],lowvol_mean[7:9],lowvol_mean[10:12],lowvol_mean[13:15]))
t3=cbind(t3,low_CI[,2:3])
rownames(t3)<-c("Mean Variance","Min Variance","Max Diversification","Max Decorrelation","Equal Weights")
colnames(t3)<- c("Return (%)","Variance (%)","Sharpe", "Lower CI","Upper CI")
t3[1:2]<-t3[1:2]*100
t3[4:5]<-t3[4:5]*100
t3

t3<- t3 %>%
  kbl(caption = "Portfolio of Low Volatility Stocks") %>%
  kable_styling(full_width = F, html_font = "Cambria", latex_options = "hold_position")


#High Volatility
t4=data.frame(rbind(highvol_mean[1:3],highvol_mean[4:6],highvol_mean[7:9],highvol_mean[10:12],highvol_mean[13:15]))
t4=cbind(t4,high_CI[,2:3])
rownames(t4)<-c("Mean Variance","Min Variance","Max Diversification","Max Decorrelation","Equal Weights")
colnames(t4)<- c("Return (%)","Variance (%)","Sharpe", "Lower CI","Upper CI")
t4[1:2]<-t4[1:2]*100
t4[4:5]<-t4[4:5]*100
t4

t4<-t4 %>%
  kbl(caption = "Portfolio of High Volatility Stocks") %>%
  kable_styling(full_width = F, html_font = "Cambria", latex_options = "hold_position")

#Daily returns plot


figure <-ggarrange(p_norm,p_exp,p_log,p_uquad,p_low,p_high,
                  ncol=3,nrow=2)
  
figure <-annotate_figure(figure,
                top = text_grob("Stock distribution", color = "black", face = "bold", size = 12),
                fig.lab = "Figure 1 ", fig.lab.face = "bold")


figure_0 <-ggarrange(price_norm,price_mix,price_high,price_low,
                  ncol=2,nrow=2)

figure_0 <-annotate_figure(figure_0,
                top = text_grob("Stock movement", color = "black", face = "bold", size = 12),
                fig.lab = "Figure 2 ", fig.lab.face = "bold")