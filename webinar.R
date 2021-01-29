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
          # + emailer_for_promotion
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


mod2 <- lm(log(num_orders) ~  
             # emailer_for_promotion
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
qqnorm(residuals(mod),ylab="Residuos")
qqline(residuals(mod))


qqnorm(rstudent(mod),ylab="Residuos Estudentizados")
qqline(rstudent(mod))

hist(residuals(mod))


# Test de normalidad
shapiro.test(residuals(mod))


## Residuos autocorrelacionados

# Gráfico de Residuos a través del tiempo
par(mfrow=c(1,1))
plot(residuals(mod), ylab="Residuos")
abline(h=0)


# Prueba de Durbin-Watson
library(lmtest)
dwtest(mod)


# Prueba de Hausman: Endeogeneity
library(AER) #for `ivreg()`
library(broom) #for `glance(`) and `tidy()`
library(knitr) #for making neat tables with `kable()`
library(stargazer) 
library(car) #for `hccm()` robust standard errors
library(knitr) #for making neat tables with `kable()`
library(sandwich)
library(stargazer) 


LP.ols <- lm(log(checkout_price) ~ 
              # + emailer_for_promotion
            + homepage_featured
            ,SDB)
kable(tidy(LP.ols), digits=4, align='c',caption=
        "Primera ecuación 2SLS para Q")


LP.Hat <- fitted(LP.ols)
Q.2sls <- lm(log(num_orders) ~ LP.Hat              
             + emailer_for_promotion
             + homepage_featured
             ,SDB)
kable(tidy(Q.2sls), digits=4, align='c',caption=
        "Segunda ecuación 2SLS para Q")




mod.ols <- lm(log(num_orders) ~ log(checkout_price)
              # + emailer_for_promotion
              + homepage_featured
              ,SDB)
mod.iv <- ivreg(log(num_orders) ~ log(checkout_price)
                # + emailer_for_promotion
                + homepage_featured |
                  + emailer_for_promotion
                + homepage_featured, data=SDB)
stargazer(mod.ols, Q.2sls, mod.iv, 
          title="Ecuación de la demanda: OLS, 2SLS, IV models",
          header=FALSE, 
          type="text", # "html" or "latex" (in index.Rmd)
          keep.stat="n",  # what statistics to print
          omit.table.layout="n",
          star.cutoffs=NA,
          digits=4, 
          #  single.row=TRUE,
          intercept.bottom=FALSE, #moves the intercept coef to top
          column.labels=c("OLS","2SLS", "IV email"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption="Variable dependiente: Log Demanda",
          model.names=FALSE,
          star.char=NULL) #supresses the stars)



LP.ols <- lm(log(checkout_price) ~ 
              + emailer_for_promotion
              + homepage_featured
              ,SDB)
summary(LP.ols)


tab <- tidy(LP.ols)
kable(tab, digits=4,
      caption="Ecuación LP")

linearHypothesis(LP.ols, c("emailer_for_promotion=0"))


#TEST DE HAUSMAN


summary(mod.iv, diagnostics=TRUE)




Parametros2 <- data.frame()
for (i in unique(BC$meal_id)) {
  SDB  <- BC[BC$meal_id == i,]
  mod <- lm(log(num_orders) ~ log(checkout_price) 
            + emailer_for_promotion
            + homepage_featured
            ,SDB)
  
  options(scipen=100)
  summary(mod)
  Segmento = i
  ElasticidadM = as.numeric(mod$coefficients[2])
  param <- cbind(Segmento,ElasticidadM)
  Parametros2 <-data.frame(rbind(Parametros2,param))
  remove(param)
  cat(paste("Ejecutando Segmento :",i,Sys.time(),"\n",sep = ""))
  paste("/*****************/","\n",sep = "")
  
}
Parametros2 <- Parametros2[order(-abs(Parametros2$Elasticidad)),] 
Parametros2

elast <- merge(Parametros,Parametros2,by="Segmento")
elast

library(ggplot2)

ggplot(elast, aes(x=Segmento)) + 
  geom_line(aes(y = Elasticidad), color = "blue",size =1) + 
  geom_line(aes(y = ElasticidadM), color="black",size =1) 


