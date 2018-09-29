library(readxl)
ConsumoE <- read_excel("C:/econometria/ConsumoE.xlsx",
                       col_types = c("text", "numeric"))
View(ConsumoE)
View(ConsumoE)
library(readxl)
ConsumoE <- read_excel("C:/econometria/ConsumoE.xlsx",
                       col_types = c("text", "numeric"))
View(ConsumoE)
View(ano)
ano <- ano[,-1]
View(ConsumoE)
ConsumoE <- ConsumoE[,-1]
consumo <- ts(ConsumoE$consumo, start = 1990, frequency = 1)
plot(consumo)
hist(consumo)
colnames(consumo)[1] <- "variacao"
colnames(consumo)[1] <- "consumo"
TesteDF_Variacao_none <- ur.df(consumo, "none",lags = 0)
install.packages("urca")                                                        #Instala pacote "Urca"
library("urca")                                                                 #Carrega Pacote
library(readxl)
install.packages("urca")
TesteDF_Variacao_none <- ur.df(consumo, "none",lags = 0)
TesteDF_consumo_none <- ur.df(consumo, "none",lags = 0)
colnames(ConsumoE)
dados_diarios <- ts(ConsumoE, start = 1990, frequency = 365)
plot(dados_diarios, col= "blue", main="consumo", xlab="Dias")
plot(dados_diarios, col= "blue", main="consumo", xlab="ano")
TesteDF_consumo_none <- ur.df(consumo, "none",lags = 0)
TesteDF_consumo_none <- ur.df(ConsumoE, "none",lags = 0)
TesteDF_Variacao_none <- ur.df(variacao, "none",lags = 0)
TesteDF_consumo_none <- ur.df(consumo, "none",lags = 0)            #Teste DF-DickFuller sem drift e sem tendencia
summary(TesteDF_consumo_none)
TesteDF_ConsumoE_none <- ur.df(ConsumoE, "none",lags = 0)            #Teste DF-DickFuller sem drift e sem tendencia
summary(TesteDF_ConsumoE_none)
TesteDF_consumo_none <- ur.df(consumo, "none",lags = 0)            #Teste DF-DickFuller sem drift e sem tendencia
summary(TesteDF_consumo_none)
TesteDF_consumo_none <- ur.df(consumo, "none",lags = 0)
install.packages("urca")
library("urca")                                                                 #Carrega Pacote
library(readxl)
TesteDF_consumo_none <- ur.df(consumo, "none",lags = 0)            #Teste DF-DickFuller sem drift e sem tendencia
summary(TesteDF_consumo_none)
library(readxl)      #Carregando o pacote Readxl, que efetua leitura dos arquivos do Excel
library(urca)
acf(ConsumoE$consumo, main="Inflacao Mensal")   #FAC - Função de Autocorrelação
pacf(ConsumoE$consumo, main="Inflacao Mensal")
acf(ConsumoE$consumo, main="consumo")   #FAC - Função de Autocorrelação
pacf(ConsumoE$consumo, main="consumo")
acf(ConsumoE$consumo, main="consumo")   #FAC - Função de Autocorrelação
pacf(ConsumoE$consumo, main="consumo")  #FACP-Funçao de Autocorrelação Parcial
acf(ConsumoE$consumo, main="consumo")   #FAC - Função de Autocorrelação
pacf(ConsumoE$consumo, main="consumo")  #FACP-Funçao de Autocorrelação Parcial
AR1 <- arima(consumo,order = c(1,0,0))  #Modelo Autorregressivo de ordem 1
AR1
install.packages("readxl")     #Instala o pacote Readxl
install.packages("ggplot2")
install.packages("readxl")
library(readxl)                 #Carrega o pacote Reaxl
library(ggplot2)
MA2 <- arima(consumo,order=c(0,0,2))
MA2
ARMA12 <- arima(consumo,order = c(1,0,2))
ARMA12
library(readxl)
AIC(AR1)
BIC(AR1)
AIC(MA1)
AIC(MA2)
BIC(MA2)
AIC(MA1)
> library(readxl)                 #Carrega o pacote Reaxl
> library(ggplot2)
> MA1 <- arima(consumo,order=c(0,0,1))
> MA1
install.packages("readxl")     #Instala o pacote Readxl
install.packages("ggplot2")
install.packages("readxl")
library(readxl)                 #Carrega o pacote Reaxl
library(ggplot2)
AR1 <- arima(consumo, order = c(1,0,0))  #Gera a regressão autoregressiva de ordem um, nomeada aqui de AR1
AR1
MA1 <- arima(Consumo,order=c(0,0,1))
MA1
MA1 <- arima(consumo,order=c(0,0,1))
MA1
MA2 <- arima(consumo,order=c(0,0,2))
MA2
ARMA11 <- arima(consumo,order = c(1,0,1))
ARMA11
ARMA12 <- arima(consumo,order = c(1,0,2))
ARMA12
library(readxl)
AIC(AR1)
BIC(AR1)
AIC(MA1)
BIC(MA1)
AIC(MA2) #Extrai a estatÃ???stica AIC do modelo AR1
BIC(MA2)
AIC(ARMA11)
BIC(ARMA11)
AIC(ARMA12)
BIC(ARMA12)
ARMA12