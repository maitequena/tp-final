setwd("C:/Users/maiti/OneDrive/Escritorio/labo/tp-final/T y Td_era5_BsAs_horario_jul_2013_2022.nc-20231122T232800Z-001")
getwd()
rm(list=ls())

#librerias que pido
library(lubridate)
require(lubridate)

library(ncdf4)
require(ncdf4)

library(ggplot2)
require(ggplot2)

library(dplyr)
require(dplyr)

#El archivo contiene datos horarios T y Td ERA5 (2m). 
#Puntos de reticula cercano a la ciudad de Buenos Aires para
#los 31 dias de Julio para los anios 2013-2022
#7440 datos (10 anios con 31 dias con un dato por hora)
#3 long y 3 lat
nc<-nc_open("T y Td_era5_BsAs_horario_jul_2013_2022.nc") #solo lectura
class(nc)

head(nc)
length(nc) #lista de 15

nc_td<-ncvar_get(nc,"d2m")#(lon,lat,time)
dim(nc_td)
nc_t<-ncvar_get(nc,"t2m")#(lon,lat,time)
dim(nc_t)

head(nc_t)
head(nc_td)
lon<-ncvar_get(nc,varid="longitude")
lat<-ncvar_get(nc,varid="latitude")

time<-ncvar_get(nc,varid="time")
class(time)

dato_faltante<- -32767 #defino datos faltantes
nc_t[which(nc_t==dato_faltante)]<-NA
nc_td[which(nc_td==dato_faltante)]<-NA

head(time) #transformo con lubridate
fecha_utc<-ymd_hms("1900-1-1 00:00:00")+hours(time)
head(fecha_utc)

fecha_buenos_aires<-as.POSIXct(fecha_utc,
                               tz="America/Argentina/Buenos_Aires") #transformo las fechas a hora local de bs as
class(fecha_buenos_aires) #se utiliza para fechas
head(fecha_buenos_aires)

dimnames(nc_t)<-list(lon=lon,lat=lat,time=as.character(fecha_buenos_aires)) #nombro las dim
dimnames(nc_td)<-list(lon=lon,lat=lat,time=as.character(fecha_buenos_aires))

str(nc_t) #resumen
str(nc_td)

################################################################################
#inciso a)
nc_t_bsas<-nc_t[1,1,] #me quedo con los primeros valores de long y lat 
nc_td_bsas<-nc_td[1,1,] #los cuales corresponden a bs as
#estos archivos si tienen 7440 datos 
head(nc_t_bsas)
class(nc_t_bsas)

#este vector contiene las fechas del periodo de analisis (sin repetir)
fecha<-c(ymd("2013-06-30"), seq(ymd("2013-07-01"), ymd("2022-07-31"), by ="day"), ymd("2022-08-01"))
head(fecha)

#extraigo anio, mes, dia y hora
anio<-format(fecha_buenos_aires, "%Y")
mes<-format(fecha_buenos_aires, "%m")
dia<-format(fecha_buenos_aires, "%d")
hora<-format(fecha_buenos_aires, "%H")

fecha_df<-data.frame(anio, mes, dia, hora)

diferencia<-nc_t_bsas-nc_td_bsas
head(diferencia)

dif_df_sinfecha<-data.frame("diferencia"=diferencia)
diferencia_df<-cbind(fecha_df,dif_df_sinfecha)

################################################################################
#incico b)

nuevo_df<-data.frame()

for (i in 1:nrow(dif_df_sinfecha)) {
  if (dif_df_sinfecha$diferencia[i] <=3) {
    fecha_niebla_df<-data.frame(anio=fecha_df$anio[i],
                                  mes=fecha_df$mes[i],
                                  dia=fecha_df$dia[i],
                                  hora=fecha_df$hora[i],
                                  diferencia=dif_df_sinfecha$diferencia[i])
    nuevo_df <- rbind(nuevo_df, fecha_niebla_df)
  }
}
#al vizualizar el df veo que los datos que no correspondian a julio no corresponden a horarios donde haya niebla

cant_por_anio<-c()
ciclo<-0
repeat {
  cant_por_mes<-0
  for (i in 1:(nrow(nuevo_df)-1)) {
    if (nuevo_df$anio[i]==2013+ciclo & nuevo_df$mes[i]=="07" & nuevo_df$dia[i]!=nuevo_df$dia[i+1]) { #el mes esta de mas
      cant_por_mes<-cant_por_mes+1
    }
  }
  cant_por_anio<-c(cant_por_anio,cant_por_mes)
  ciclo <- ciclo + 1
  if (2013+ciclo==2023) {
    break
  }
}
cant_por_anio

################################################################################
#inciso c)

frecuencia_horaria<-table(nuevo_df$hora)
head(frecuencia_horaria)
class(frecuencia_horaria)

#Creo un grafico de barras
barplot(frecuencia_horaria, 
        main="Frecuencia horaria de la niebla
        de Julio para BS As 2013-2022", 
        xlab="hora local", 
        ylab="Frecuencia")#chequear

################################################################################
#inciso d)

datos_boxplot<-c(169,171,185,197,201,213,222,227,225,220,193,115,91,80,66,63,64,73,92,131,139,144,150,163)
#lo hice de esta forma porque estaba teniendo problemas para graficar y crei que podia ser por la forma en la cual le daba los datos al box plot
horas<-as.character(0:23)

boxplot_df <- data.frame(x = 0:23, y = datos_boxplot)

boxplot(datos_boxplot , horizontal = TRUE, main = "Diagrama boxplot", xlab = "Valores")


################################################################################
#inciso e)
#busco el evento de mayor duracion
#la mayor cantidad de horas continuas con niebla
#aqui utilizo la libreria dplyr

#combino las primeras columnas en una columna de tipo POSIXct
nuevo_df<-nuevo_df %>%
  mutate(fecha_junta=as.POSIXct(paste(anio, mes, dia, hora, sep="-"), format="%Y-%m-%d-%H"))

#calculo la diferencia en horas entre cada fila y la siguiente
nuevo_df<-nuevo_df %>%
  arrange(fecha_junta) %>%
  mutate(dif_horas=difftime(lead(fecha_junta), fecha_junta, units="hours"))

duracion<-c()
contador<-0

for (i in 1:3592) {
  if ((nuevo_df$dif_horas[i]==1)==T) {
    contador<-contador+1
    duracion<-c(duracion,contador)
    if (nuevo_df$dif_horas[i]!=nuevo_df$dif_horas[i+1]) {
      contador<-0
    }
  }
  if (nuevo_df$dif_horas[i]!=1) {
    duracion<-c(duracion,contador)
  }
}

nuevo_df$duracion<-c(duracion,0,0)

niebla_max<-max(nuevo_df$duracion)
niebla_max
pos_niebla_max<-which(nuevo_df$duracion==niebla_max)
nuevo_df$fecha_junta[pos_niebla_max]
#fueron 116hs seguidas de niebla hasta el 10-7-2017 a las 14

t_bsas_c<-(nc_t_bsas[(3210-116-24+1):(3210+24)])-273
td_bsas_c<-(nc_td_bsas[(3210-116-24+1):(3210+24)])-273

hs_niebla<-c(seq(19,23,by=1),rep(seq(0,23,by=1),6),seq(0,14,by=1))

datos_niebla_df<-data.frame("t"=t_bsas_c,"td"=td_bsas_c,"horas"=hs_niebla)
niebla_antes_df<-data.frame("t"=t_bsas_c[0:24],"td"=td_bsas_c[0:24],"horas"=hs_niebla[0:24])
niebla_durante_df<-data.frame("t"=t_bsas_c[25:140],"td"=td_bsas_c[25:140],"horas"=hs_niebla[25:140])
niebla_despues_df<-data.frame("t"=t_bsas_c[141:164],"td"=td_bsas_c[141:164],"horas"=hs_niebla[141:164])


antes <- ggplot(niebla_antes_df, aes(x = horas)) +
  geom_line(aes(y = t, color = "T"), size = 0.3, alpha = 0.3) +
  geom_line(aes(y = td, color = "Td"), size = 0.3, alpha = 0.3) +
  labs(title = "24hs antes", subtitle = "4/7/17 19hs al 5/7/17 19hs", caption = "Elaborado por Maite Gonzalez") +
  scale_color_manual(name = "Leyenda", values = c("T" = "black", "Td" = "blue")) +
  ylab("°C")

antes


despues <- ggplot(niebla_despues_df, aes(x = horas)) +
  geom_line(aes(y = t, color = "T"), size = 0.3, alpha = 0.3) +
  geom_line(aes(y = td, color = "Td"), size = 0.3, alpha = 0.3) +
  labs(title = "24hs despues", subtitle = "10/7/17 14hs al 11/7/17 14hs", caption = "Elaborado por Maite Gonzalez") +
  scale_color_manual(name = "Leyenda", values = c("T" = "black", "Td" = "blue")) +
  ylab("°C")

despues

