#------------------------------------#
#-----Cambiando el Directorio--------#
#------------------------------------#
# Antes de nada, limpiamos el workspace, por si hubiera alg?n dataset o informaci?n cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#------------------------------------#
# La base de datos KAGGLE
#------------------------------------#
#
BC <- read.csv(file = 'archive/train.csv', header = T, sep = ",")
head(BC,2)


#------------------------------------#
# CURVA DE DEMANDA
#------------------------------------#
SBC <-BC[sample(1:nrow(BC), 10000, replace=FALSE),]


plot(SBC$num_orders, SBC$checkout_price)
lm(SBC$num_orders ~ SBC$checkout_price)
abline(lm(SBC$num_orders ~ SBC$checkout_price),col=c("blue"), lty=c(2), lwd=c(3))

plot(log(SBC$num_orders), log(SBC$checkout_price))
lm(log(SBC$num_orders) ~ log(SBC$checkout_price))
abline(lm(log(SBC$num_orders) ~ log(SBC$checkout_price)),col=c("blue"), lty=c(2), lwd=c(3))



#------------------------------------#
# ANÁLISIS EXPLORATORIO: SEGMENTACIÓN
#------------------------------------#
library(psych)
describe(BC)

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(dplyr)

head(BC,2)


GRUPO <- dplyr::group_by(BC,center_id)
TABSDB <- dplyr::summarise(GRUPO
                           ,n = n()
                           ,AvgPrice = sum(as.numeric(checkout_price*num_orders))/sum(as.numeric(num_orders))
)
TABSDB <- as.data.frame(TABSDB)
TABSDB
plot(TABSDB$center_id, TABSDB$AvgPrice)


GRUPO <- dplyr::group_by(BC,meal_id)
TABSDB <- dplyr::summarise(GRUPO
                           ,n = n()
                           ,AvgPrice = sum(as.numeric(checkout_price*num_orders))/sum(as.numeric(num_orders))
)
TABSDB <- as.data.frame(TABSDB)
TABSDB
plot(TABSDB$meal_id, TABSDB$AvgPrice)


SDB <- BC[BC$meal_id==2956,]
head(SDB,5)
plot(SDB$num_orders, SDB$checkout_price)

#------------------------------------#
# ELASTICIDAD
#------------------------------------#
SBC <-BC[sample(1:nrow(BC), 10000, replace=FALSE),]

par(mfrow=c(1,1))
boxplot(SBC$num_orders,  ylab="dIAG")

plot(SBC$num_orders, SBC$checkout_price, pch=16)
lines(smooth.spline(SBC$num_orders, SBC$checkout_price, df=3),col="red")


plot(SBC$num_orders, SBC$checkout_price)
lm(SBC$num_orders ~ SBC$checkout_price)
abline(lm(SBC$num_orders ~ SBC$checkout_price),col=c("blue"), lty=c(2), lwd=c(3))
lines(smooth.spline(SBC$num_orders, SBC$checkout_price, df=3),col="red", lty=c(2), lwd=c(3))

plot(log(SBC$num_orders), log(SBC$checkout_price))
lm(log(SBC$num_orders) ~ log(SBC$checkout_price))
abline(lm(log(SBC$num_orders) ~ log(SBC$checkout_price)),col=c("blue"), lty=c(2), lwd=c(3))
lines(smooth.spline(log(SBC$num_orders), log(SBC$checkout_price), df=3),col="red", lty=c(2), lwd=c(3))


Parametros <- data.frame()
for (i in unique(BC$meal_id)) {
  SDB  <- BC[BC$meal_id == i,]
  mod <- lm(log(num_orders) ~ log(checkout_price) ,SDB)
  
  options(scipen=100)
  summary(mod)
  Segmento = i
  Elasticidad = as.numeric(mod$coefficients[2])
  param <- cbind(Segmento,Elasticidad)
  Parametros <-data.frame(rbind(Parametros,param))
  remove(param)
  cat(paste("Ejecutando Segmento :",i,Sys.time(),"\n",sep = ""))
  paste("/*****************/","\n",sep = "")
  
}
Parametros <- Parametros[order(-abs(Parametros$Elasticidad)),] 
Parametros


#------------------------------------#
# REGRESIÓN
#------------------------------------#
TABSDB <- TABSDB[order(abs(TABSDB$n)),] 
TABSDB

head(BC,2)

SDB  <- BC[BC$meal_id == 2290,]
mod <- lm(log(num_orders) ~ log(checkout_price)
          + emailer_for_promotion
          + homepage_featured
          ,SDB)
mod

#------------------------------------#
# Prueba de Significancia de la Regresión
# Buscando Parsimonia
#------------------------------------#

#Utilizando INTUICIÓN ESTADÍSTICA
(tss <- sum((log(SDB$num_orders)-mean(log(SDB$num_orders)))^2))
(rss <- deviance(mod))
df.residual(mod)

## Estadístico de Prueba
(fstat <- ((tss-rss)/3)/(rss/df.residual(mod)))

# p-valor
1-pf(fstat,4,df.residual(mod))


sqrt(fstat)
#Estadístico de Prueba t
(tstat <- summary(mod)$coef[2,3])


mod2 <- lm(log(num_orders) ~  emailer_for_promotion
          + homepage_featured
          ,SDB)
mod2



#Utilizando ANOVA 
library(faraway)
library(car)

anova(mod2,mod)
linearHypothesis(mod, c("log(checkout_price) = 0"))


#----------------------------------------------#
# Intervalos de Confianza para los Coeficientes
#----------------------------------------------#
############################
confint(mod)

#---------------------------------------------------------#
# Elipsoide de Confianza para un Subvector de Coeficientes
#---------------------------------------------------------#
library(ellipse)
plot(ellipse(mod,c(2,3)),type="l")
points(0,0)
points(coef(mod)[2],coef(mod)[3],pch=18)
abline(v=confint(mod)[2,],lty=2)
abline(h=confint(mod)[3,],lty=2)



#---------------------------------------------------------#
# Test de Heteroscedasticidad
#---------------------------------------------------------#
library(lmtest)

ncvTest(mod)
bptest(mod,varformula = ~ fitted.values(mod),studentize=FALSE)

## Normalidad
qqnorm(residuals(mod9),ylab="Residuos")
qqline(residuals(mod9))


qqnorm(rstudent(mod9),ylab="Residuos Estudentizados")
qqline(rstudent(mod9))

hist(residuals(mod9))

# Datos Simulados
par(mfrow=c(3,3))
for(i in 1:9) qqnorm(rnorm(50))
# Distribución Asimétrica
for(i in 1:9) qqnorm(exp(rnorm(50))) 
# Distribución con colas largas (leptocúrtica)
for(i in 1:9) qqnorm(rcauchy(50))
# Distribución con colas cortas (platicúrtica)
for(i in 1:9) qqnorm(runif(50))
par(mfrow=c(1,1))

# Test de normalidad
shapiro.test(residuals(mod9))

# Envelopes
source(file.choose())
source(envel.norm.R)
envel.norm(mod9)  

## Residuos autocorrelacionados

# Gráfico de Residuos a través del tiempo
plot(residuals(mod9), ylab="Residuos")
abline(h=0)


# Prueba de Durbin-Watson
library(lmtest)
dwtest(mod9)


#############################
# Ejemplo: Datos  Simulados #
#############################
set.seed(33423423)
x<-seq(1:10)
e<-rnorm(n = 10,mean = 0,sd = 3)
y<-3+1.5*x+e
mod1<-lm(y~x)
summary(mod1)
mean(mod1$residuals^2)

#Escenario 1: Cuando x=5, y se incrementa en 10 unidades
y1<-y
y1[5]<-y1[5]+10
mod2<-lm(y1~x)
summary(mod2)
mean(mod2$residuals^2)

#Escenario 2: Cuando x=10, y se incrementa en 10 unidades
y2<-y
y2[10]<-y2[10]+10
mod3<-lm(y2~x)
summary(mod3)
mean(mod3$residuals^2)

par(mfrow=c(1,3))
plot(x,y,ylim = c(0,31),
     main="Datos Originales Simulados")
lines(x, fitted(mod1), col="blue")
plot(x,y1,ylim = c(0,31),
     main="Y con incremento de 10 en x=5")
lines(x, fitted(mod2), col="blue")
plot(x,y2,ylim = c(0,31),
     main="Y con incremento de 10 en x=10")
lines(x, fitted(mod3), col="blue")
par(mfrow=c(1,1))

#Diagnóstico
source(diag.norm.R)
source(file.choose())
diag.norm(mod1)  
diag.norm(mod2)
diag.norm(mod3)

###########################################################################
# Ejemplo: Delivery                                                       #
###########################################################################

delivery = scan("delivery.dat", list(tempo=0, ncaixas=0, distancia=0))
attach(delivery)

#Análisis Descriptivo
boxplot(tempo, ylab="Tiempo Gastado")
boxplot(ncaixas, ylab="Número de Cajas")
boxplot(distancia, ylab="Distancia Recorrida")

plot(ncaixas, tempo, ylab="Tiempo Gastado",
     xlab="Número de Cajas", pch=16)
lines(smooth.spline(ncaixas, tempo, df=3))

plot(distancia, tempo, ylab="Tiempo Gastado",
     xlab="Distancia Percorrida", pch=16)
lines(smooth.spline(distancia, tempo, df=3))

ajuste1.delivery = lm(tempo ~ ncaixas + distancia)
summary(ajuste1.delivery)

#Diagnóstico
source(diag.norm.R)
diag.norm(ajuste1.delivery)  
diag.norm(ajuste1.delivery,iden=1)  

ajuste2.delivery = lm(tempo ~ ncaixas + distancia, subset=-9)
summary(ajuste2.delivery)
diag.norm(ajuste2.delivery)  


#---------------------------------------------------------#
# Intervalos de Confianza y Predicción
#---------------------------------------------------------#
x0 <- data.frame(checkout_price=1000,emailer_for_promotion=1,homepage_featured=1)
str(predict(mod,x0,se=TRUE))
predict(mod,x0,interval="confidence")
predict(mod,x0,interval="prediction")

grid <- seq(0,100,1)
p <- predict(mod,x0,se=T,interval="confidence")
matplot(grid,p$fit,lty=c(1,2,2),type="l",xlab="pop15",ylab="sr")



