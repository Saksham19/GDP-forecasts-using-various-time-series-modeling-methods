#Sierra Leone GDP Forecast till 2024
#We have yearly data for gross domestic product,gross domestic capital formation, household exp and govt exp all in usd 
#DATA FROM CEIC - 1964 TO 2017 (we will forecast till 2024)
#DATASET cleaned and final version is available here: https://drive.google.com/file/d/1n-bYIHYcu3tpROmsZtSsPIuRPyyh_27y/view?usp=sharing
library(fpp2)
library(tseries)
#importing the dataset
library(readxl)
sl<-read_excel("Sierra_Leone_gdp.xlsx")
sl<-ts(sl[,-1],freq=1) #taking a freq=12 means we are taking the data every 12 years, gives us a better look at the trend 
gdp_sl<-sl[,1]
#checking for trends, seasonality etc.
autoplot(gdp_sl,xlab="years",ylab="gdp(usd)")+
  ggtitle("gdp(usd) for Sierra Leone")+
  theme(plot.title=element_text(size=7,face="bold"))
#can see an overall inc trend(inspite the fall over the last decade or so)
#no trend evident
#can verify for trends:
ggseasonplot(gdp_sl)
#is not seasonal (atleast not for yearly data)
#if we want even more detailed view at trends, seasonality:
gdp_stl<-mstl(gdp_sl)
autoplot(gdp_stl) #gives us the trend separately via removing the seasonally adjusted data

#we can check for autocorr among the given variables:
library(GGally)
GGally::ggpairs(as.data.frame(sl))
#actually there doesn't seem to much autocorr. this is good!

#trying to use the multiple linear reg model to see if we can get a reliable prediction
#first we can verify how many varibles (out of 4) we should consider to get the best multiple linear reg model
#based on the cv

GE<-sl[,2]
PE<-sl[,3]
GFCF<-sl[,4]
NE<-sl[,5]
#first, we confirm that the var are not corr individually (basically same thing as the scatterplot above)
a1<-sl%>%
  as.data.frame()%>%
  ggplot(aes(x=gdp_sl,y=GE))+
  ylab("govt exp")+xlab("gdp")+
  ggtitle("gdp and ge scatterplot")+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  theme(plot.title = element_text(size=7,face="bold"))
a2<-sl%>%
  as.data.frame()%>%
  ggplot(aes(x=gdp_sl,y=PE))+
  ylab("PE")+xlab("gdp")+
  ggtitle("gdp and PE scatterplot")+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  theme(plot.title = element_text(size=7,face="bold"))
a3<-sl%>%
  as.data.frame()%>%
  ggplot(aes(x=gdp_sl,y=GFCF))+
  ylab("GFCF")+xlab("gdp")+
  ggtitle("gdp and GFCF scatterplot")+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  theme(plot.title = element_text(size=7,face="bold"))
a4<-sl%>%
  as.data.frame()%>%
  ggplot(aes(x=gdp_sl,y=NE))+
  ylab("NE")+xlab("gdp")+
  ggtitle("gdp and ge scatterplot")+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  theme(plot.title = element_text(size=7,face="bold"))
library(gridExtra)
grid.arrange(a1,a2,a3,a4)
#gdp,ge and pe do seem to be positively linked actually. might be worth removing gfcf from our multiple reg model. can confirm via the following codes anyway:


#given we have 4 var, there are 2^4 possibilities i.e. 16 cases to check
fit.1<-tslm(gdp_sl~GE+PE+GFCF+NE)
CV1<-CV(fit.1)
fit.2<-tslm(gdp_sl~GE+PE+GFCF)
CV2<-CV(fit.2)
fit.3<-tslm(gdp_sl~GE+PE+NE)
CV3<-CV(fit.3)
fit.4<-tslm(gdp_sl~GE+GFCF+NE)
CV4<-CV(fit.4)
fit.5<-tslm(gdp_sl~PE+GFCF+NE)
CV5<-CV(fit.5)
fit.6<-tslm(gdp_sl~GE+PE)
CV6<-CV(fit.6)
fit.7<-tslm(gdp_sl~GFCF+NE)
CV7<-CV(fit.7)
fit.8<-tslm(gdp_sl~GE+GFCF)
CV8<-CV(fit.8)
fit.9<-tslm(gdp_sl~GE+NE)
CV9<-CV(fit.9)
fit.10<-tslm(gdp_sl~PE+GFCF)
CV10<-CV(fit.10)
fit.11<-tslm(gdp_sl~PE+NE)
CV11<-CV(fit.11)
fit.12<-tslm(gdp_sl~GE)
CV12<-CV(fit.12)
fit.13<-tslm(gdp_sl~PE)
CV13<-CV(fit.13)
fit.14<-tslm(gdp_sl~GFCF)
CV14<-CV(fit.14)
fit.15<-tslm(gdp_sl~NE)
CV15<-CV(fit.15)
fit.16<-tslm(gdp_sl~trend+season) #no season factor incl
CV16<-CV(fit.16)
rbind(CV1,CV2,CV3,CV4,CV5,CV6,CV7,CV8,CV9,CV10,CV11,CV12,CV13,CV14,CV15,CV16)
#CHECK AICc
#1st model with all the factors has the least AICc
#hence we go with that for forecasting
fit.mlr<-tslm(gdp_sl~GE+PE+GFCF+NE)
summary(fit.mlr)
#p and t values are quite useless in terms of forecasting so just move to forecast command
#first we see how good this model is via plotting the fitted values on actual values
autoplot(gdp_sl,series="data")+
  autolayer(fitted(fit.mlr),series="fitted")+
  xlab("years")+ylab("")+
  ggtitle("gdp Seirra Leone")+
  guides(colour=guide_legend(title=""))
#to see if the residuals are autocorr or not:
checkresiduals(fit.mlr)
#the p value for bg test is too big for us to reject the null hyp (HO: autocorr=white noise) hence the model has autocorr thats probably just whitenoise. GOOD!
#forecast:
fit.mlr2<-tslm(gdp_sl~trend)
fit.mlr2f<-forecast(fit.mlr,h=54)
fit.mlr2f
autoplot(fit.mlr2f)
#not worth it, gives weird forecasts

#the ets method (error, trend and cylce )
fit.ets<-ets(gdp_sl)
fit.ets
#ETS(M,A,N) i.e. multiplicative error, additive trend and no seasonality
fit.etsf<-forecast(fit.ets,h=8)
fit.etsf
autoplot(fit.etsf)
#the pi seem to be quite big
#can check residuals 
checkresiduals(fit.etsf)
#the p value is small enough to reject null. autocorr is probably not whitenoise. not good news

#as we dont have seasonality, ets+stl model is not worth checking for

#arima method
#this data can probably use some box-cox transformation:
gdp_sl_bc<-BoxCox(gdp_sl,lambda=BoxCox.lambda(gdp_sl))
fit.arima<-auto.arima(gdp_sl_bc,approximation=FALSE,stepwise = FALSE)
fit.arima
#ARIMA(2,1,0) with drift (remember, no seasonality in this data)

#before we forecast, we have to see if this arima model is the best or not
ggtsdisplay(gdp_sl_bc)
#check the acf plots and pacf plots to get potential arima models
ndiffs(gdp_sl_bc)
nsdiffs(gdp_sl_bc) #no seasonal so this will give an error
#as per the pacf plots, there is  alag at 1 and 7(can ignore 7 tbh) and acf is declining. can have arima (1,1,0)
#Trying to plot arima(1,1,0)
fit.arima2<-Arima(gdp_sl_bc,order=c(1,1,0))
fit.arima2
#to see which one is actually the better arima model, we choose the one with the lower AICc:
summary(fit.arima)
#aicc is -103.81
summary(fit.arima2)
#aicc is -89.68
#so, we use the second arima as the model for our forecasts
fit.arima2f<-forecast(fit.arima2,h=8)
fit.arima2f
autoplot(fit.arima2f)
#essentially gives a flat forecast

#combination model:
train<-subset(gdp_sl,end=length(gdp_sl)-11)
h<-11
mlr_fit<-tslm(train~trend)
MLR<-forecast(mlr_fit,h=h)
ETS<-forecast(ets(train),h=h)
ARIMA<-forecast(Arima(train,order=c(1,1,0)),h=h)
Combination<-(MLR[["mean"]]+ETS[["mean"]]+ARIMA[["mean"]])/3

autoplot(gdp_sl)+
  autolayer(MLR,series="MLR",PI=FALSE)+
  autolayer(ETS,series="ETS",PI=FALSE)+
  autolayer(ARIMA,series="ARIMA",PI=FALSE)+
  autolayer(Combination,series="Combination",PI=FALSE)+
  xlab("years")+ylab("gdp")+
  ggtitle("gdp forecast for Sierra Leone(8yrs)")+
  theme(plot.title=element_text(size=7,face="bold"))

#as per the graph, it seems like ets was the closest 
#we need to check the accuracy to determine which model offers us the best predictions
c(MLR=accuracy(MLR,gdp_sl)["Test set","RMSE"],
  ETS=accuracy(ETS,gdp_sl)["Test set","RMSE"],
  aRIMA=accuracy(ARIMA,gdp_sl)["Test set","RMSE"],
  combination=accuracy(Combination,gdp_sl)["Test set","RMSE"])
#ets is the best one (as has the min real means squared error)


#hence, our final forecast for the gdp of Sierra Leone till 2024 is :
fit.ets<-ets(gdp_sl)
forecast(fit.ets,h=8)
autoplot(forecast(fit.ets,h=8))
