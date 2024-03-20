library(readxl)
library(dplyr)
library(Distance)
library(ggplot2)
#Cargar achivo .csv...
#cargar directorio
setwd("D:/Tesis-MR/FEBRERO")
transectos_23_HBE <- read.csv2("T_2023_dep_habitats.csv")
#Obtener una vista previa de los datos...
head(transectos_23_HBE,n=3)
tail(transectos_23_HBE,n=3)
#ver dimensiones del data frame venados_2021...
dim(transectos_23_HBE)
#resumen_estadistico de la variable Distance...
summary(transectos_23_HBE$distance)
#Histograma de distancia...
# $=sirve para acceder a una columna individual de un data frame...
cutpoints<-seq(0,900,by=50)
hist(transectos_23_HBE$distance,xlab= 'Distancia (m)', ylab='Frecuencia (# observaciones)',
     main='Distribución de distancias perpendiculares para el venado de cola blanca',
     col='orange',breaks=cutpoints)

                            ###################### PRUEBAS DE MODELOS ########################
                                    ### MODELO 1 ###
transectos_2023_HBE.model1 <- ds(transectos_23_HBE, key='hn', adjustment= 'cos')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2023_HBE.model1$ddf)
#graficar la función de detección#
plot(transectos_2023_HBE.model1, breaks=cutpoints)
#ver objetos de ds para el modelo1...
names(transectos_2023_HBE.model1)
names(transectos_2023_HBE.model1$ddf)
names(transectos_2023_HBE.model1$dht)
#Evaluar la BONDAD DE AJUSTE para el modelo 1...
gof_ds(transectos_2023_HBE.model1)

                                    ### MODELO 2 ###
transectos_2023_HBE.model2 <- ds(transectos_23_HBE, key='hn', adjustment= 'herm')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2023_HBE.model2$ddf)
#graficar la función de detección...
plot(transectos_2023_HBE.model2, breaks=cutpoints)
#ver objetos de ds para el modelo1...
names(transectos_2023_HBE.model2)
names(transectos_2023_HBE.model2$ddf)
names(transectos_2023_HBE.model2$dht)
#Evaluar la BONDAD DE AJUSTE para el modelo 1....
gof_ds(transectos_2023_HBE.model2)

                                  ### MODELO 3 ###
transectos_2023_HBE.model3 <- ds(transectos_23_HBE, key='hn', adjustment= 'poly')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2023_HBE.model3$ddf)
#graficar la función de detección...
plot(transectos_2023_HBE.model3, breaks=cutpoints)
#ver objetos de ds para el modelo1...
names(transectos_2023_HBE.model3)
names(transectos_2023_HBE.model3$ddf)
names(transectos_2023_HBE.model3$dht)
#Evaluar la BONDAD DE AJUSTE para el modelo 1....
gof_ds(transectos_2023_HBE.model3)

                                        ### MODELO 4 ###
transectos_2023_HBE.model4 <- ds(transectos_23_HBE, key='hr', adjustment= 'cos')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2023_HBE.model4$ddf)
#graficar la función de detección...
plot(transectos_2023_HBE.model4, breaks=cutpoints)
#ver objetos de ds para el modelo1...
names(transectos_2023_HBE.model4)
names(transectos_2023_HBE.model4$ddf)
names(transectos_2023_HBE.model4$dht)
#Evaluar la BONDAD DE AJUSTE para el modelo (valor numerico)...
gof_ds(transectos_2023_HBE.model4)
#Grafica Q-Q...
gds_23<-gof_ds(transectos_2023_HBE.model4)
#genero un vector de numeros del 0 al 1 para crear una linea...
line_3<-seq(-0.5,1.5,by=0.5)
#ploteo una linea roja sobre el grafico anterior...
lines(line_3,line_3,col='red',lwd=1.3)

                                      ### MODELO 5 ###
transectos_2023_HBE.model5 <- ds(transectos_23_HBE, key='hr', adjustment= 'herm')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2023_HBE.model5$ddf)
#graficar la función de detección...
plot(transectos_2023_HBE.model5, breaks=cutpoints)
#ver objetos de ds para el modelo1...
names(transectos_2023_HBE.model5)
names(transectos_2023_HBE.model5$ddf)
names(transectos_2023_HBE.model5$dht)
#Evaluar la BONDAD DE AJUSTE para el modelo....
gof_ds(transectos_2023_HBE.model5)

                                      ### MODELO 6 ###
transectos_2023_HBE.model6 <- ds(transectos_23_HBE, key='hr', adjustment= 'poly')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2023_HBE.model6$ddf)
#graficar la función de detección...
plot(transectos_2023_HBE.model6, breaks=cutpoints)
#ver objetos de ds para el modelo1...
names(transectos_2023_HBE.model6)
names(transectos_2023_HBE.model6$ddf)
names(transectos_2023_HBE.model6$dht)
#Evaluar la BONDAD DE AJUSTE para el modelo....
gof_ds(transectos_2023_HBE.model6)

                                      ### MODELO 7 ###
transectos_2023_HBE.model7 <- ds(transectos_23_HBE, key='unif', adjustment= 'cos')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2023_HBE.model7$ddf)
#graficar la función de detección...
plot(transectos_2023_HBE.model7, breaks=cutpoints)
#ver objetos de ds para el modelo1...
names(transectos_2023_HBE.model7)
names(transectos_2023_HBE.model7$ddf)
names(transectos_2023_HBE.model7$dht)
#Evaluar la BONDAD DE AJUSTE para el modelo....
gof_ds(transectos_2023_HBE.model7)

                                    ### MODELO 8 ###
#transectos_2023_HBE.model8 <- ds(transectos_23_HBE, key='unif', adjustment= 'herm')
#resumen del modelo en relación con la función de detección estimada...
#summary(transectos_2023_HBE.model8$ddf)
#graficar la función de detección...
#plot(transectos_2023_HBE.model8, breaks=cutpoints)
#ver objetos de ds para el modelo1...
#names(transectos_2023_HBE.model8)
#names(transectos_2023_HBE.model8$ddf)
#names(transectos_2023_HBE.model8$dht)
#Evaluar la BONDAD DE AJUSTE para el modelo....
#gof_ds(transectos_2023_HBE.model8)

                                        ### MODELO 9 ###
transectos_2023_HBE.model9 <- ds(transectos_23_HBE, key='unif', adjustment= 'poly')
#resumen del modelo en relación con la función de detección estimada...
summary(transectos_2023_HBE.model9$ddf)
#graficar la función de detección...
plot(transectos_2023_HBE.model9, breaks=cutpoints)
#ver objetos de ds para el modelo1...
names(transectos_2023_HBE.model9)
names(transectos_2023_HBE.model9$ddf)
names(transectos_2023_HBE.model9$dht)
#Evaluar la BONDAD DE AJUSTE para el modelo....
gof_ds(transectos_2023_HBE.model9)

                                      #RESUMEN DEL AIC DE LOS MODELOS USADOS#
AIC(transectos_2023_HBE.model1,transectos_2023_HBE.model2,transectos_2023_HBE.model3,
    transectos_2023_HBE.model4,transectos_2023_HBE.model5,transectos_2023_HBE.model6, 
    transectos_2023_HBE.model7, transectos_2023_HBE.model9)

#Tabla de comparación de modelos para datos de transectos lineales de Venados de Cola Blanca en el año 2021#
library(knitr)
resultados <- summarize_ds_models(transectos_2023_HBE.model1, transectos_2023_HBE.model2,
                                  transectos_2023_HBE.model3,transectos_2023_HBE.model4,
                                  transectos_2023_HBE.model5,transectos_2023_HBE.model6,
                                  transectos_2023_HBE.model7, transectos_2023_HBE.model9,
                                  output = "plain")
resultados_models_23_HBE <- as.data.frame(resultados)
#descargar mis resultados#
write.csv2(resultados_models_23_HBE, file = "Resultados_models_2023_HBE.csv", row.names = FALSE)

                            # RESULTADOS DE DENSIDAD DEL MEJOR MODELO - MODELO 4 #
summary(transectos_2023_HBE.model4)
#RESULTADOS DENSIDAD...
transectos_2023_HBE.model4$dht$individuals$D
resul.final <- transectos_2023_HBE.model4$dht$individuals$D
resultados_final <- as.data.frame(resul.final)
Resultados_23_HBE_km2<-data.frame(Label=resul.final$Label,
                      Densidad=resul.final$Estimate *1000000,
                      'Desviacion_estand(se)'=resul.final$se*1000000,
                      'Coef_variacion(cv)'=resul.final$cv,
                      'LowerControlLimit(lcl)'=resul.final$lcl*1000000,
                      'UpperControlLimit(ucl)'=resul.final$ucl*1000000,
                      'Grados_libertad(df)'=resul.final$df)
Resultados_23_HBE_km2
#descargar mis resultados#
write.csv2(Resultados_23_HBE_km2,file="Resultados_densidad_2023_HBE.csv", row.names = FALSE)

