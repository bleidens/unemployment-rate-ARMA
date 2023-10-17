# carregando os pacotes que serão usados
library(forecast)
library(BTSR)
library(readr)
# carregando os dados
dados <-  read_delim("ipeadata-tx-des.csv", 
                 delim = ";", escape_double = FALSE, 
                 locale = locale(decimal_mark = ","), 
                 col_names = c("Data","tx_desocup","c"),
                 skip=1,
                 trim_ws = TRUE)[,-3]
# definindo a variável de interesse
y <-ts(dados$tx_desocup/100,freq=12,start = c(2012,3))
# # graficos interessantes
# hist(y)
# plot(y)
# monthplot(y)
# acf(y)
# pacf(y)
# criando conjnto de treino
n<-length(y)
ntest<-6
y_train<-y[1:(n-ntest)]
y_test<-y[(n-ntest+1):n]
# ajustando o modelo ARIMA
fit1<-auto.arima(y_train)
fitted1<-fit1$fitted
forecast1<- predict(fit1,ntest)$pred
# ajustando o modelo barma
cont<-0
order<-matrix(NA,15,4)
for(i in 0:3){
  for(j in 0:3){
    if(i==0 && j==0) next
    cont<-cont+1
    print(c("i=",i,"j=",j))
    barma<-summary(BARFIMA.fit(y_train,p=i,d=F,q=j,
                       report=F,info=T))
    karma<-try(summary(KARFIMA.fit(y_train,p=i,d=F,q=j,
                               report=F,info=T)),silent = T)
    if(class(karma)=="try-error") # a classe dos objetos que cont?m o erro, 
    {suppressWarnings(karma$aic<-1e50)}
    order[cont,]<-c(i,j,barma$aic,karma$aic)
  }
}

print(order)
order<-order[1:14,]

orbarma<-order[which(order[,3]==min(order[,3])),c(1:3)]
orkarma<-order[which(order[,4]==min(order[,4])),c(1:2,4)]

barma<-BARFIMA.fit(y_train,p=orbarma[1],d=F,q=orbarma[2],
                   info=T,report=F)
forecastb<- predict(barma,nnew=ntest)$forecast

karma<-KARFIMA.fit(y_train,p=orkarma[1],d=F,q=orkarma[2],
                   info=T,report=F)
forecastk<- predict(karma,nnew=ntest)$forecast

results_insample<-rbind(
  forecast::accuracy(fit1$fitted, y_train),
  forecast::accuracy(barma$fitted.values, y_train),
  forecast::accuracy(karma$fitted.values, y_train)
)[,c(3,2,5)]

results_insample

results<-rbind(
  forecast::accuracy(forecast1, y_test),
  forecast::accuracy(forecastb, y_test),
  forecast::accuracy(forecastk, y_test)
)[,c(3,2,5)]

results
