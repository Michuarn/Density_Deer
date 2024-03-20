par(mfrow = c(1,3), mar = c(5, 4, 2, 1), oma = c(0, 0, 0, 0))
setwd("D:/Tesis-MR/FEBRERO")
transectos_21<-read.csv2("T_2021_dep.csv")
transectos_22<-read.csv2("T_2022_dep.csv")
transectos_23<-read.csv2("T_2023_dep.csv")


#Grafica del 2021##
cutpoints_21<-seq(0,550,by=50)
hist(transectos_21$distance,xlab= 'Distancia (m)', ylab='Frecuencia (# observaciones)',
     main='2021',
     col='palevioletred',breaks=cutpoints_21, 
     width = 500 )

##

#Grafica del 2022##
cutpoints_22<-seq(0,750,by=50)
hist(transectos_22$distance,xlab= 'Distancia (m)', ylab='Frecuencia (# observaciones)',
     main='2022',
     col='skyblue',breaks=cutpoints_22,
     width = 500 )
##

#Grafica del 2023##
cutpoints_23<-seq(0,900,by=50)
hist(transectos_23$distance,xlab= 'Distancia (m)', ylab='Frecuencia (# observaciones)',
     main='2023',
     col='yellowgreen',breaks=cutpoints_23,
     width = 500 )
##
par(mfrow = c(1, 1))

#----------------------------------------------------------------------------------------#

                      ############## AJUSTE DE LAS FUNCIONES DETECCIÓN ########

library(readxl)
library(dplyr)
library(Distance)
library(ggplot2)

par(mfrow = c(1,3), mar = c(5, 4, 2, 1), oma = c(0, 0, 0, 0))

transectos_2021.model4 <- ds(transectos_21, key='hr', adjustment= 'cos')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2021.model4$ddf)
#graficar la función de detección...
plot(transectos_2021.model4, breaks=cutpoints_21, xlab="Distancia",ylab="Probabilidad de detección", main='2021')

##

transectos_2022.model4 <- ds(transectos_22, key='hr', adjustment= 'cos')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2022.model4$ddf)
#graficar la función de detección...
plot(transectos_2022.model4, breaks=cutpoints_22, xlab="Distancia",ylab="Probabilidad de detección", main='2022')

##

transectos_2023.model4 <- ds(transectos_23, key='hr', adjustment= 'cos')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2023.model4$ddf)
#graficar la función de detección...
plot(transectos_2023.model4, breaks=cutpoints_23, xlab="Distancia",ylab="Probabilidad de detección", main='2023')

#----------------------------------------------------------------------------------------#

                        ############## GRAFICO QQ-PLOT ##############
par(mfrow = c(1,3), mar = c(5, 4, 2, 1), oma = c(0, 0, 0, 0))
#Grafica Q-Q...
gds_21<-gof_ds(transectos_2021.model4, main='2021')
#genero un vector de numeros del 0 al 1 para crear una linea...
line_1<-seq(-0.5,1.5,by=0.5)
#ploteo una linea roja sobre el grafico anterior...
lines(line_1,line_1,col='red',lwd=1.3)

##

#Grafica Q-Q...
gds_22<-gof_ds(transectos_2022.model4, main='2022')
#genero un vector de numeros del 0 al 1 para crear una linea...
line_2<-seq(-0.5,1.5,by=0.5)
#ploteo una linea roja sobre el grafico anterior...
lines(line_2,line_2,col='red',lwd=1.3)

##
#Grafica Q-Q...
gds_23<-gof_ds(transectos_2023.model4, main='2023')
#genero un vector de numeros del 0 al 1 para crear una linea...
line_3<-seq(-0.5,1.5,by=0.5)
#ploteo una linea roja sobre el grafico anterior...
lines(line_3,line_3,col='red',lwd=1.3)

