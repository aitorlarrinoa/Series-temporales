
# Cargamos las librerías necesarias
library(readr)
library(readxl)

library(tidyverse)
library(forecast)
library(tsibble)
library(feasts)
library(tsoutliers)
library(TSA)
library(xts)
library(dynlm)

##################################### Lectura y vistazo del fichero
data <- read_excel("/Users/aitor/Desktop/Máster Ciencia de Datos/Series temporales/Series-temporales/data/HW02/data.xlsx") 

tail(data)

#### Hallamos todos los datos
ts_colgate <- ts(data$Colgate, start=c(1958,1), end=c(1963,16), frequency=52)
ts_crest <- ts(data$Crest, start=c(1958,1), end=c(1963,16), frequency=52)

ts_data <- cbind(ts_colgate, ts_crest)
ts_data

#### Hallamos el training
ts_colgate_train <- head(ts_colgate, length(ts_colgate)-16)
ts_crest_train <- head(ts_crest, length(ts_crest)-16)

ts_data_train <- cbind(ts_colgate_train, ts_crest_train)
ts_data_train

#### Hallamos el testing
ts_colgate_test <- tail(ts_colgate, 16)
ts_crest_test <- tail(ts_crest, 16)

ts_data_test <- cbind(ts_colgate_test, ts_crest_test)

### Hacemos el gráfico de las cuotas de mercado de Crest y Colgate
autoplot(ts_data) +
  labs(title = "Cuota de mercado de Crest y Colgate",
       x="Tiempo semanal")

##################################### Modelos autoarima.

modelo_colgate <- auto.arima(ts_colgate_train,lambda=0)
summary(modelo_colgate)

modelo_crest <- auto.arima(ts_crest_train,lambda=0)
summary(modelo_crest)

### análisis de residuos

# análisis de residuos para Colgate
ggtsdisplay(modelo_colgate$residuals, theme=theme_bw())

# análisis de residuos para crest
ggtsdisplay(modelo_crest$residuals, theme=theme_bw())



### Test de Box-Ljung 

# Este test nos servirá para ver la dependencia lineal conjunta de los residuos.
# Comenzamos con Colgate
Box.test(modelo_colgate$residuals,lag=52, fitdf=2, type="Lj")

# Realizamos el test para Crest
Box.test(modelo_crest$residuals,lag=52, fitdf=4, type="Lj")


### Predicciones de los modelos ARIMA con horizonte de predicción e 16 semanas

# Comenzamos con Colgate
f_colgate = forecast(modelo_colgate,h=16)

# Predicciones para Crest
f_crest = forecast(modelo_crest, h=16)


### Visualizamos la gráfica de las predicciones

# Para Colgate
autoplot(f_colgate) +
  ggtitle("COLGATE: Predicción Colgate") +
  labs(x = "", y="Cuota de mercado") +
  theme_bw()

# Para Crest
autoplot(f_crest) +
  ggtitle("CREST: Predicción Crest") +
  labs(x = "", y="Cuota de mercado") + 
  theme_bw()

### Comparamos ahora la realidad con las predicciones:

# Colgate
pred_colgate <- cbind(f_colgate, ts_colgate_test)
  
colnames(pred_colgate) <- c("Colgate_pred", "IC_80_inf","IC_80_sup","IC_95_inf","IC_95_sup","Colgate_real")

ggplot(as_tibble(pred_colgate)) +
  ggtitle("COLGATE: Comparación de la predicción y la realidad") +
  geom_line(aes(x=1:16, y = Colgate_pred, col="predicción")) + 
  geom_line(aes(x=1:16, y = Colgate_real, col="realidad")) 

autoplot(pred_colgate) +
  ggtitle("COLGATE: Comparación de la predicción y la realidad")

# Crest
pred_crest <- cbind(f_crest, ts_crest_test)

colnames(pred_crest) <- c("Crest_pred", "IC_80_inf","IC_80_sup","IC_95_inf","IC_95_sup","Crest_real")

ggplot(as_tibble(pred_crest)) +
  ggtitle("Crest: Comparación de la predicción y la realidad") +
  geom_line(aes(x=1:16, y = Crest_pred, col="predicción")) + 
  geom_line(aes(x=1:16, y = Crest_real, col="realidad")) 

autoplot(pred_crest) +
  ggtitle("CREST: Comparación de la predicción y la realidad")



##################################### Modelos de intervención

### Detección de outliers para colgate y crest

# Colgate 
outliers_colgate <- tso(y = ts_colgate_train, types = c("AO", "LS", "TC"),discard.method = "bottom-up", 
    tsmethod = "auto.arima", args.tsmethod = list(allowdrift = FALSE, ic = "bic"))

# Mostramos los resultados
outliers_colgate

# Gráfica de los resultados
plot(outliers_colgate)

# Crest
outliers_crest <- tso(y = ts_crest_train, types = c("AO", "LS", "TC"),discard.method = "bottom-up", 
    tsmethod = "auto.arima", args.tsmethod = list(allowdrift = FALSE, ic = "bic"))

# Mostramos los resultados
outliers_crest

# Gráfica de los resultados
plot(outliers_crest)

### Modelos de intervención. 

# COLGATE

int_data=as.ts(ts_colgate)

dummies=data.frame(
  AO5950=1*(seq(ts_colgate)==102),
  LS6032=1*(seq(ts_colgate)>=136))


mod_int=arimax(int_data,order=c(3,0,0),
               seasonal=list(order=c(1,0,0),period=52),
               xreg=dummies,
               method='ML')
mod_int

# CREST

int_data_2=as.ts(ts_crest)

dummies_2=data.frame(
  LS6032=1*(seq(ts_crest)>=136),
  AO6111=1*(seq(ts_crest)==167),
  TC6140=1*(seq(ts_crest)==196))


mod_int_2=arimax(int_data_2,order=c(0,1,1),
               #seasonal=list(order=c(0,1,1),period=52),
               xreg=dummies_2,
               method='ML')
mod_int_2

##################################### Función de transferencia

# Estimación v(B) 

mod0 = dynlm(ts_colgate_train ~ L(ts_crest_train, 0:15) + L(ts_colgate_train, 1))

summary(mod0)

# Veamos si los residuos son ruido blanco
forecast::tsdisplay(mod0$residuals)

# Hacemos la modelización con ARIMAX

# Necesitamos el orden del ARIMA para introducirlo en ARIMAX
auto.arima(ts_colgate_train, lambda=0)

mod0_arimax <- arimax(ts_colgate_train,
               order=c(0,1,1),
               include.mean=TRUE,
               #seasonal=list(order=c(1,0,0),period=52),
               xtransf=ts_crest_train,
               transfer=list(c(0,15)),
               method="ML")

summary(mod0_arimax)

#Mostramos los residuos
forecast::tsdisplay(mod0_arimax$residuals, main = "Residuos del modelo ARIMAX")

mod0_arimax$coef
plot(mod0_arimax$coef[2:17],type="h", 
     main="Gráfico de los coeficientes del modelo ARIMAX",
     xlab="",
     ylab="Valor de los coeficientes")

# valores de b, r y s: b = r = s = 0

# Una vez tenemos el ruido blanco, realizamos los cálculos para estimar la función de transferencia

mod_t <- arimax(ts_colgate_train,
                order=c(0,1,1),
                include.mean=FALSE,
                xtransf = ts_crest_train,
                transfer=list(c(0,0)),
                method="ML")
mod_t

mod_f <- arimax(ts_colgate_train,
                order=c(0,1,1),
                include.mean=FALSE,
                fixed = c(NA,NA),
                xtransf = ts_crest_train,
                transfer=list(c(0,0)),
                method="ML") #r y #s+b
mod_f

forecast::tsdisplay(mod_f$residuals)

############################################################################### FIN