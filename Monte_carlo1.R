source("Simulation.R")
#MonteCarlo simulation of portfolio strategies 
#given different parameters(combination of distributions)
simulations = 1000
tradingdays = 253
scenario = "allnormal"
set.seed(123)
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario)
portfolio_returns
allnorm_ret=data.frame(portfolio_returns)
allnorm_mean=round(colMeans(na.omit(portfolio_returns)),6)

scenario = "mix"
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario)
portfolio_returns
mix_ret=portfolio_returns
mix_ret_mean=round(colMeans(na.omit(portfolio_returns)),6)

scenario = "low volatility"
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario)
portfolio_returns
lowvol_ret=portfolio_returns
lowvol_mean=round(colMeans(na.omit(portfolio_returns)),6)

scenario = "high volatility"
portfolio_returns = portfolio_simulation(simulations, tradingdays, scenario)
portfolio_returns
highvol_ret=portfolio_returns
highvol_mean=round(colMeans(na.omit(portfolio_returns)),6)

#Plots for portfolio Returns 

library(ggpubr)
library(kableExtra)
library(DescTools)

#All Normal
a1<- ggplot(allnorm_ret, aes(x=meanvarReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(meanvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Mean Variance",x="Return", y = "Frequency")+
  theme_classic()

a2<- ggplot(allnorm_ret, aes(x=minvarReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(minvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Min Variance",x="Return", y = "Frequency")+
  theme_classic()

a3<- ggplot(allnorm_ret, aes(x=maxdivReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(maxdivReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Diversification",x="Return", y = "Frequency")+
  theme_classic()

a4<-ggplot(allnorm_ret, aes(x=maxdecReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(maxdecReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Decorrelation",x="Return", y = "Frequency")+
  theme_classic()

a5<-ggplot(allnorm_ret, aes(x=equalweightsReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(equalweightsReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Equal Weights",x="Return", y = "Frequency")+
  theme_classic()


figure1<-ggarrange(a1,a2,a3,a4,a5,
                   ncol=3,nrow=2)

annotate_figure(figure1,
                top = text_grob("Portfolio of All Normal Stocks", color = "black", face = "bold", size = 14),
                fig.lab = "Figure 1", fig.lab.face = "bold")


#Mixed
b1<- ggplot(mix_ret, aes(x=meanvarReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(meanvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Mean Variance",x="Return", y = "Frequency")+
  theme_classic()

b2<- ggplot(mix_ret, aes(x=minvarReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(minvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Min Variance",x="Return", y = "Frequency")+
  theme_classic()

b3<- ggplot(mix_ret, aes(x=maxdivReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(maxdivReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Diversification",x="Return", y = "Frequency")+
  theme_classic()

b4<-ggplot(mix_ret, aes(x=maxdecReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(maxdecReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Decorrelation",x="Return", y = "Frequency")+
  theme_classic()

b5<-ggplot(mix_ret, aes(x=equalweightsReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(equalweightsReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Equal Weights",x="Return", y = "Frequency")+
  theme_classic()


figure2<-ggarrange(b1,b2,b3,b4,b5,
                   ncol=3,nrow=2)

annotate_figure(figure2,
                top = text_grob("Portfolio of Mixed Distribution Stocks", color = "black", face = "bold", size = 14),
                fig.lab = "Figure 2", fig.lab.face = "bold")


#Low Volatility

c1<- ggplot(lowvol_ret, aes(x=meanvarReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(meanvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Mean Variance",x="Return", y = "Frequency")+
  theme_classic()

c2<- ggplot(lowvol_ret, aes(x=minvarReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(minvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Min Variance",x="Return", y = "Frequency")+
  theme_classic()

c3<- ggplot(lowvol_ret, aes(x=maxdivReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(maxdivReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Diversification",x="Return", y = "Frequency")+
  theme_classic()

c4<-ggplot(lowvol_ret, aes(x=maxdecReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(maxdecReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Decorrelation",x="Return", y = "Frequency")+
  theme_classic()

c5<-ggplot(lowvol_ret, aes(x=equalweightsReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(equalweightsReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Equal Weights",x="Return", y = "Frequency")+
  theme_classic()


figure3<-ggarrange(c1,c2,c3,c4,c5,
                   ncol=3,nrow=2)

annotate_figure(figure3,
                top = text_grob("Portfolio of Low Volatility Stocks", color = "black", face = "bold", size = 14),
                fig.lab = "Figure 3", fig.lab.face = "bold")


#High volatility

d1<- ggplot(highvol_ret, aes(x=meanvarReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(meanvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Mean Variance",x="Return", y = "Frequency")+
  theme_classic()

d2<- ggplot(highvol_ret, aes(x=minvarReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(minvarReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Min Variance",x="Return", y = "Frequency")+
  theme_classic()

d3<- ggplot(highvol_ret, aes(x=maxdivReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(maxdivReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Diversification",x="Return", y = "Frequency")+
  theme_classic()

d4<-ggplot(highvol_ret, aes(x=maxdecReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(maxdecReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Max Decorrelation",x="Return", y = "Frequency")+
  theme_classic()

d5<-ggplot(highvol_ret, aes(x=equalweightsReturn)) +
  geom_histogram(fill="white", color="black")+
  geom_vline(aes(xintercept=mean(equalweightsReturn,na.rm=TRUE)), color="blue",
             linetype="dashed")+
  labs(title="Equal Weights",x="Return", y = "Frequency")+
  theme_classic()


figure4<-ggarrange(d1,d2,d3,d4,d5,
                   ncol=3,nrow=2)

annotate_figure(figure4,
                top = text_grob("Portfolio of High Volatility Stocks", color = "black", face = "bold", size = 14),
                fig.lab = "Figure 4 ", fig.lab.face = "bold")

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


t1 %>%
  kbl(caption = "Table 1: Portfolio of All Normal Stocks") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Mixed
t2=data.frame(rbind(mix_ret_mean[1:3],mix_ret_mean[4:6],mix_ret_mean[7:9],mix_ret_mean[10:12],mix_ret_mean[13:15]))
t2=cbind(t2,allnorm_CI[,2:3])
rownames(t2)<-c("Mean Variance","Min Variance","Max Diversification","Max Decorrelation","Equal Weights")
colnames(t2)<- c("Return (%)","Variance (%)","Sharpe", "Lower CI","Upper CI")
t2[1:2]<-t2[1:2]*100
t2[4:5]<-t2[4:5]*100
t2

t2 %>%
  kbl(caption = "Table 2: Portfolio of Mixed Distribution Stocks") %>%
  kable_classic(full_width = F, html_font = "Cambria")


#Low Volatility
t3=data.frame(rbind(lowvol_mean[1:3],lowvol_mean[4:6],lowvol_mean[7:9],lowvol_mean[10:12],lowvol_mean[13:15]))
t3=cbind(t3,low_CI[,2:3])
rownames(t3)<-c("Mean Variance","Min Variance","Max Diversification","Max Decorrelation","Equal Weights")
colnames(t3)<- c("Return (%)","Variance (%)","Sharpe", "Lower CI","Upper CI")
t3[1:2]<-t3[1:2]*100
t3[4:5]<-t3[4:5]*100
t3

t3 %>%
  kbl(caption = "Table 3: Portfolio of Low Volatility Stocks") %>%
  kable_classic(full_width = F, html_font = "Cambria")


#High Volatility
t4=data.frame(rbind(highvol_mean[1:3],highvol_mean[4:6],highvol_mean[7:9],highvol_mean[10:12],highvol_mean[13:15]))
t4=cbind(t4,high_CI[,2:3])
rownames(t4)<-c("Mean Variance","Min Variance","Max Diversification","Max Decorrelation","Equal Weights")
colnames(t4)<- c("Return (%)","Variance (%)","Sharpe", "Lower CI","Upper CI")
t4[1:2]<-t4[1:2]*100
t4[4:5]<-t4[4:5]*100
t4

t4 %>%
  kbl(caption = "Table 4: Portfolio of High Volatility Stocks") %>%
  kable_classic(full_width = F, html_font = "Cambria")