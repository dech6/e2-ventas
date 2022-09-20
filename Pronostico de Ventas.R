library(tidyverse)
library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)
library(fpp2)
library(readxl)
library(astsa)
library(tseries)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(openxlsx)
library(car)
library(urca)
##-------------------------------------------------------------------------------------##
#Cargamos los datos
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

Ventas_Fecha_organizada <- read_excel("./DATOS DE VENTAS FECHA MODIFICADA.xlsx", 
                                      sheet = "BASE")

##----------------------------------------------------------------------------------------##
### Organizamos los datos 
datos <- Ventas_Fecha_organizada
names(datos)[15]= "ventas"
names(datos)[9]= "productos"
datos$fecha <- as.Date(datos$fecha) #-> Funcion para establecer el tipo de dato como fecha
class(datos$fecha)
#datos$fecha1 <- NULL #-> Funcion para borrar datos 

##-----------------------------------------------------------------##
# Creamos un nuevo data frame con las columnas necesarias para el analisis

datos_filtrados <- datos %>% select(c(ventas,fecha))
datos_filtrados

#Resumimos las ventas, para tenerlas por meses 
ventas_mensuales <- datos_filtrados %>%
  
  mutate(mes = format(fecha,"%m"), year = format(fecha,"%Y")) %>%
  
  group_by(mes,year) %>%  #Resumimos las ventas por mes y a?o con la funcion group by
  
  summarise(ventas = sum(ventas))

ventas_mensuales <- ventas_mensuales[with(ventas_mensuales,order(ventas_mensuales$year)),]
ventas_mensuales

#Corregimos los valores NA que da?an la estimacion
ventas_mensuales1 <- ventas_mensuales[-c(1),]


###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#Declaramos los datos en una variable como serie de tiempo

y <- ts(ventas_mensuales1["ventas"] , start = c(2013,1), end = c(2022,5), frequency = 12)
y


##Exploramos los datos
autoplot(y) + ggtitle("Grafico de ventas en el tiempo e2 Energia eficiente") +
  ylab("VR TOTAL M$000")+theme_modern_rc()


#Estacionalidad en los datos
#Hay problemas de tendecia que lo arreglamos aplicando la primera diferencia

ddatos <- diff(y)
adf.test(ddatos, alternative = "stationary")

ggseasonplot(ddatos) + ggtitle("Grafico estacional: cambios en las ventas mensuales") +
  ylab("VR TOTAL M$000")+ theme_modern_rc()

decom = decompose(ddatos)
autoplot(decom)

#------------------------------------------------------------------------------------------#



#------------------------------------------------------------------------------------#
#-------------#
#-------#
# Pronostico del modelo de ventas totales 
modelo_arima <- auto.arima(y,d=1,D=1, stepwise = FALSE , approximation = FALSE , trace = TRUE)
print(modelo_arima)
checkresiduals(modelo_arima)


pronostico <- forecast(modelo_arima, h=6, level = c(95))
autoplot(pronostico) +ggtitle("Pronostico de ventas proximos 6 meses")+ theme_bw()

#Exportamos el data frame a excel

df.pronostico <- as.data.frame(pronostico)



write.xlsx(df.pronostico , file = "pronostico_ventas.xlsx")




#-------------------------------#
#--------------------------------------------#
#Estimacion por los 10 mejores productos

# 1- Seleccionamos los datos que queremos

datos_filtrados1 <- datos %>% select(c(ventas,fecha,productos))
datos_filtrados1

# 2- Resumimos las ventas, para tenerlas por productos 
ventas_mensuales <- datos_filtrados1 %>%
  
  group_by(productos) %>%  #Resumimos las ventas por productos, y encontramos los porductos con mas ventas con la funcion group by
  
  summarise(ventas = sum(ventas))

ventas_productos <- ventas_mensuales[with(ventas_mensuales,order(decreasing = TRUE,ventas_mensuales$ventas)),]
ventas_productos

#3- filtramos por los 10 productos con mas ventas
ventas_fil_prod<- ventas_productos %>% filter(ventas >= ventas[10])

#-------------------------------------------------------------------------------------------#
#4- data frame final para estimacion del modelo de los 10 productos mas vendidos

ventas_mensuales_productos <- datos_filtrados1 %>%
  
  mutate(mes = format(fecha,"%m"), year = format(fecha,"%Y")) %>%
  
  group_by(mes,year,productos) %>%  #Resumimos las ventas por mes y a?o con la funcion group by
  
  summarise(ventas = sum(ventas)) %>% 
  
  filter(productos == ventas_fil_prod$productos[1] | productos == ventas_fil_prod$productos[2]
         | productos == ventas_fil_prod$productos[3] | productos == ventas_fil_prod$productos[4] 
         | productos == ventas_fil_prod$productos[5] | productos == ventas_fil_prod$productos[6] 
         | productos == ventas_fil_prod$productos[7] | productos == ventas_fil_prod$productos[8] 
         | productos == ventas_fil_prod$productos[9] | productos == ventas_fil_prod$productos[10])


ventas_mensuales_productos <- ventas_mensuales_productos[with(ventas_mensuales_productos,order(decreasing = FALSE,ventas_mensuales_productos$year)),]
ventas_mensuales_productos

###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#5- Seguimos arreglando los datos para poder hacer la proyeccion


productos_wide <- pivot_wider(ventas_mensuales_productos, names_from = productos,
                              values_from = ventas) 

#Data farme, remplazado valores NA por 0 (No se vendieron)

 productos_wide[is.na(productos_wide)] <- 0

#---------------------------------------------------------------------------------#
##Exportamos datos a excel

top10_ventas<- as.data.frame(productos_wide)

write.xlsx(productos_wide , "ventas_top.xlsx")


###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
# a) Primer proyeccion

serie1 <- productos_wide %>% select(2,4) #Data frame nuevo con el primer producto
#serie1 <- serie1[!(is.na(serie1[3])),]  #Borramos los valores NA de la fila del producto


#Creamos la serie de tiempo
dd <- ts(serie1[3], start= 2013,frequency = 12)
dd


#Rellenamos los valores NA de la serie (Interpolacion)
#serie1_NA_estimado<- na.interp(dd)
nombres <- names(productos_wide)

#Graficamos las ventas del producto
autoplot(dd)+ ggtitle("Grafico de ventas ", nombres[4])+ ylab("Valor de ventas") +
  theme_minimal()


###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#Estimacion 

ddatos1 <- diff(serie1_NA_estimado)
adf.test(ddatos1, alternative = "stationary")

#Aplicando diferencias , observamos que la prueba si es estacionaria


############ MODELO ARIMA
modelo_arima1 <- auto.arima(dd,d=1,D=1, stepwise = FALSE , approximation = FALSE , trace = TRUE)
print(modelo_arima1)
checkresiduals(modelo_arima1)



pronostico1 <- forecast(modelo_arima1, h=6, level = c(95))
autoplot(pronostico1) +ggtitle("Pronostico de ventas proximos 6 meses", nombres[4])+ theme_minimal()

df.pronostico1 <- as.data.frame(pronostico1)


###----------------------------------------------------------------------##
##----------------------------------------------------------------------##

# B) Segunda proyeccion

#seleccionamos los datos
serie2 <- productos_wide %>% select(2,5) 


#Creamos la serie de tiempo
dd1<- ts(serie2[3],start = 2013, frequency = 12)
dd1


#Graficamos las ventas del producto
autoplot(dd1)+ ggtitle("Grafico de ventas ", nombres[5])+ ylab("Valor de ventas") +
  theme_minimal()


###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#Estimacion 


############ MODELO ARIMA
modelo_arima2 <- auto.arima(dd1,d=1,D=1, stepwise = FALSE , approximation = FALSE , trace = TRUE)
print(modelo_arima2)
checkresiduals(modelo_arima2)

#Aplicamos el pronostico
pronostico2 <- forecast(modelo_arima2, h=6, level = c(95))
autoplot(pronostico2) +ggtitle("Pronostico de ventas proximos 6 meses", nombres[5])+ theme_minimal()

#Guardamos el pronostico en un data frame

df.pronostico2 <- as.data.frame(pronostico2)


###----------------------------------------------------------------------##
##----------------------------------------------------------------------##

# C) Tercera proyeccion

serie3 <- productos_wide %>% select(2,6) 


#Creamos serie de tiempo
dd3<- ts(serie3[3],start = 2013,frequency = 12)
dd3


#Graficamos las ventas del producto
autoplot(dd3)+ ggtitle("Grafico de ventas ", nombres[6])+ ylab("Valor de ventas") +
  theme_minimal()


###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#Estimacion 


############ MODELO ARIMA
modelo_arima3 <- auto.arima(dd3,d=1,D=1, stepwise = FALSE , approximation = FALSE , trace = TRUE)
print(modelo_arima3)
checkresiduals(modelo_arima3)

#Aplicamos el pronostico
pronostico3 <- forecast(modelo_arima3, h=6, level = c(95))
autoplot(pronostico3) +ggtitle("Pronostico de ventas proximos 6 meses", nombres[6])+ theme_minimal()

#Guardamos el pronostico en un data frame

df.pronostico3 <- as.data.frame(pronostico3)#Graficamos las ventas del producto




###----------------------------------------------------------------------##
##----------------------------------------------------------------------##

# d) cuarta proyeccion

serie4 <- productos_wide %>% select(2,7)


#Creamos serie de tiempo
dd4<- ts(serie4[3],start = 2013, frequency = 12)
dd4

#Graficamos las ventas del producto
autoplot(dd4)+ ggtitle("Grafico de ventas ", nombres[7])+ ylab("Valor de ventas") +
  theme_minimal()


###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#Estimacion 


############ MODELO ARIMA
modelo_arima4 <- auto.arima(dd4,d=1,D=1, stepwise = FALSE , approximation = FALSE , trace = TRUE)
print(modelo_arima4)
checkresiduals(modelo_arima4)

#Aplicamos el pronostico
pronostico4 <- forecast(modelo_arima3, h=6, level = c(95))
autoplot(pronostico3) +ggtitle("Pronostico de ventas proximos 6 meses", nombres[7])+ theme_minimal()

#Guardamos el pronostico en un data frame

df.pronostico4 <- as.data.frame(pronostico4)#Graficamos las ventas del producto



###----------------------------------------------------------------------##
##----------------------------------------------------------------------##

# e) Quinta proyeccion

serie5 <- productos_wide %>% select(2,8) 


#Creamos serie de tiempo
dd5<- ts(serie5[3],start = 2013, frequency = 12)
dd5
 
#Graficamos las ventas del producto
autoplot(dd5)+ ggtitle("Grafico de ventas ", nombres[8])+ 
  ylab("Valor de ventas") + theme_minimal()


###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#Estimacion 


############ MODELO ARIMA
modelo_arima5 <- auto.arima(dd5,d=1,D=1, stepwise = FALSE ,
                            approximation = FALSE , trace = TRUE)
print(modelo_arima5)
checkresiduals(modelo_arima5)

#Aplicamos el pronostico
pronostico5 <- forecast(modelo_arima5, h=6, level = c(95))

autoplot(pronostico5) +
  ggtitle("Pronostico de ventas proximos 6 meses", nombres[8])+ 
  theme_minimal()

#Guardamos el pronostico en un data frame

df.pronostico3 <- as.data.frame(pronostico3)#Graficamos las ventas del producto



###----------------------------------------------------------------------##
##----------------------------------------------------------------------##











































###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#Explicacion del modelo arima
adf.test(y, alternative = "stationary")

par(mfrow=c(2,1), mar=c(3,3,3,1)+.1) 
acf(ts(ddatos, frequency = 1))
pacf(ts(ddatos, frequency = 1))


modelo1 = arima(y,order = c(1,1,1))
modelo1

#Diagnostico del modelo
tsdiag(modelo1)


error= residuals(modelo1)
plot(error)


