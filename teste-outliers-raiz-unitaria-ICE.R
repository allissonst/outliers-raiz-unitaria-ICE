options(scipen = 9999)
library(stats)
library(ggplot2)
library(fpp2)
library(quantmod)
library(seasonal)
library(ggthemes)
library(dplyr)
library(dygraphs)
library(tseries)
library(lmtest)
library(normtest)
library(FinTS)
library(nonlinearTseries)
library(openxlsx)
library(urca)
library(BETS)
library(tsoutliers)
library(expsmooth)
library(fma)
library(forecast)

#Importando base de dados
dados<-read.xlsx("ice.xlsx")

#separando as variáveis
ICE<-dados[,2]

#séries temporais
ICE<-ts(ICE, frequency = 12, start=c(2001,01))

plot(ICE)

#Verificando Outliers na Série:
outliers.ice=tsoutliers::tso(ICE, types=c("AO","LS","TC","IO","LSL"),
                             maxit.iloop=10)

outliers.ice

plot(outliers.ice)

auto.arima(ICE)

#Teste de Raiz Unitária ADF

#Sem drift
adf_ice=ur.df(ICE, type="none", lags=13, selectlags = "AIC")
summary(adf_ice)

#Com drift
adf_ice=ur.df(ICE, type="drift", lags=13, selectlags = "AIC")
summary(adf_ice)

#Com tendência
adf_ice=ur.df(ICE, type="trend", lags=13, selectlags = "AIC")
summary(adf_ice)

#Teste de Raiz Unitária PP

#Com constante
pp_ice=ur.pp(ICE, c("Z-tau"), lags=c("short"), model=c("constant"))
summary(pp_ice)
pp_ice@teststat
pp_ice@cval

#Com tendência
pp_ice=ur.pp(ICE, type=c("Z-tau"), lags=c("short"), model=c("trend"))
summary(pp_ice)
pp_ice@teststat
pp_ice@cval

#Teste de Raiz Unitária KPSS

#Com constante
kpss_ice=ur.kpss(ICE, type="mu", lags=c("long"))
summary(kpss_ice)

#Com constante e tendência
kpss_ice=ur.kpss(ICE, type="tau", lags=c("long"))
summary(kpss_ice)

#Teste de Raiz Unitária ERS

#Com constante
ers_ice=ur.ers(ICE, model=c("constant"), lag.max=15)
summary(ers_ice)

#Com constante e tendência
ers_ice=ur.ers(ICE, model=c("trend"), lag.max=15)
summary(ers_ice)

#Teste de Raiz Unitária Zivot-Andrews

#Com constante
za_ice=ur.za(ICE, model="intercept")
summary(za_ice)
plot(za_ice)

#Com tendência
za_ice=ur.za(ICE, model="trend")
summary(za_ice)
plot(za_ice)

#Com constante e tendência
za_ice=ur.za(ICE, model="both")
summary(za_ice)
plot(za_ice)