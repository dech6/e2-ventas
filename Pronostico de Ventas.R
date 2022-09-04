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
library(rio)
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
# Creamos un nuevo data frame con las columnas necesarias para el analisi

datos_filtrados <- datos %>% select(c(ventas,fecha))
datos_filtrados

#Resumimos las ventas, para tenerlas por meses 
ventas_mensuales <- datos_filtrados %>%
  
  mutate(mes = format(fecha,"%m"), año = format(fecha,"%Y")) %>%
  
  group_by(mes,año) %>%  #Resumimos las ventas por mes y a?o con la funcion group by
  
  summarise(ventas = sum(ventas))

ventas_mensuales <- ventas_mensuales[with(ventas_mensuales,order(ventas_mensuales$año)),]
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
pronostico <- forecast(modelo_arima, h=6, level = c(95))
autoplot(pronostico) +ggtitle("Pronostico de ventas proximos 6 meses")+ theme_bw()

df.pronostico <- as.data.frame(pronostico)

write.xlsx(df.pronostico , "pronostico_ventas.xlsx")




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
  
  mutate(mes = format(fecha,"%m"), año = format(fecha,"%Y")) %>%
  
  group_by(mes,año,productos) %>%  #Resumimos las ventas por mes y a?o con la funcion group by
  
  summarise(ventas = sum(ventas)) %>% 
  
  filter(productos == ventas_fil_prod$productos[1] | productos == ventas_fil_prod$productos[2]
         | productos == ventas_fil_prod$productos[3] | productos == ventas_fil_prod$productos[4] 
         | productos == ventas_fil_prod$productos[5] | productos == ventas_fil_prod$productos[6] 
         | productos == ventas_fil_prod$productos[7] | productos == ventas_fil_prod$productos[8] 
         | productos == ventas_fil_prod$productos[9] | productos == ventas_fil_prod$productos[10])


ventas_mensuales_productos <- ventas_mensuales_productos[with(ventas_mensuales_productos,order(decreasing = FALSE,ventas_mensuales_productos$año)),]
ventas_mensuales_productos

###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#5- Seguimos arreglando los datos para poder hacer la proyeccion


productos_wide <- pivot_wider(ventas_mensuales_productos, names_from = productos,
                              values_from = ventas) 
nombres<-names(productos_wide)

#---------------------------------------------------------------------------------#
##Exportamos datos a excel

top10_ventas<- as.data.frame(productos_wide)

write.xlsx(productos_wide , "ventas_top.xlsx")


###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
# a) Primer proyeccion

serie1 <- productos_wide %>% select(2,4) #Data frame nuevo con el primer producto
serie1 <- serie1[!(is.na(serie1[3])),]  #Borramos los valores NA de la fila del producto



dd <- ts(serie1[3], start= c(2013,01),end =c(2022,02),frequency = 12)
dd
autoplot(dd)+ ggtitle("Grafico de ventas ", nombres[4]) + ylab("VR TOTAL M$000")+theme_modern_rc()

###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#Estimacion 









###----------------------------------------------------------------------##
##----------------------------------------------------------------------##

# B) Segunda proyeccion

serie2 <- productos_wide %>% select(2,5) %>% filter(año > 2019)
#serie2 <- serie2[!(is.na(serie2[3])),] #Borramos los valores NA


#Creamos la serie de tiempo
dd1<- ts(serie2[3],start = c(2020,1),end = c(2022,2),frequency = 12)
dd1
#Rellenamos los valores NA de la serie (Interpolacion)
serie2_NA_estimado<- na.interp(dd1)


#Graficamos las ventas del producto
autoplot(serie2_NA_estimado, series="Interpolated") +
  autolayer(dd1, series="Original") +
  scale_colour_manual(
    values=c(`Interpolated`="red",`Original`="sky blue"))+ ggtitle("Grafico de ventas ", nombres[5])+ ylab("Valor de ventas") +
  theme_modern_rc()





###----------------------------------------------------------------------##
##----------------------------------------------------------------------##
#Estimacion 









###----------------------------------------------------------------------##
##----------------------------------------------------------------------##

# C) Tercera proyeccion

serie3 <- productos_wide %>% select(2,6) %>% filter(año > 2019)


#Creamos serie de tiempo
dd2<- ts(serie3[3],start = c(2020,1),end = c(2022,2),frequency = 12)
dd2


#Interpolacion
serie3_NA_estimado <- na.interp(dd2)


#Graficamos las ventas del producto
autoplot(serie3_NA_estimado, series="Interpolated") +
  autolayer(dd2, series="Original") +
  scale_colour_manual(
    values=c(`Interpolated`="red",`Original`="sky blue"))+ ggtitle("Grafico de ventas ", nombres[6])+ ylab("Valor de ventas") +
  theme_modern_rc()















































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


