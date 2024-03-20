                         #########PRUEBA DE NORMALIDAD DE DATOS###########
setwd("D:/Tesis-MR/FEBRERO")
#Normalidad datos 2021#
resultados_2021 <- read.csv2("Resultados_densidad_2021.csv") 
resultado_shapiro_2021 <- shapiro.test(resultados_2021$Densidad[1:13])
print(resultado_shapiro_2021)
#resultado: los datos NO siguen una distribucion normal ya que 0.0005294< 0.05

#Normalidad datos 2022#
resultados_2022 <- read.csv2("Resultados_densidad_2022.csv") 
resultado_shapiro_2022 <- shapiro.test(resultados_2022$Densidad[1:7])
print(resultado_shapiro_2022)
#resultado: los datos SI siguen una distribucion normal ya que 0.08914 > 0.05

#Normalidad datos 2023#
resultados_2023 <- read.csv2("Resultados_densidad_2023.csv") 
resultado_shapiro_2023 <- shapiro.test(resultados_2023$Densidad[1:27])
print(resultado_shapiro_2023)
#resultado: los datos NO siguen una distribucion normal ya que 0.0001901 < 0.05

                                #Normalidad datos 2023-habitats#
#arbustal#
resultados_2023 <- read.csv2("Resultados_densidad_2023.csv") 
resultado_shapiro_2023 <- shapiro.test(resultados_2023$Densidad[1:6])
print(resultado_shapiro_2023)
#resultado: los datos NO siguen una distribucion normal ya que 0.04335 < 0.05

#arenal#
resultados_2023 <- read.csv2("Resultados_densidad_2023.csv") 
resultado_shapiro_2023 <- shapiro.test(resultados_2023$Densidad[7:9])
print(resultado_shapiro_2023)
#resultado: los datos SI siguen una distribucion normal ya que  0.3504 > 0.05

#herbazal#

resultados_2023 <- read.csv2("Resultados_densidad_2023.csv") 
resultado_shapiro_2023 <- shapiro.test(resultados_2023$Densidad[10:27])
print(resultado_shapiro_2023)
#resultado: los datos NO siguen una distribucion normal ya que 0.001572 < 0.05


             ########### KRUSKAL WALLIS PARA COMPARACIÓN DE HABITATS 2023 ####
setwd("D:/Tesis-MR/FEBRERO")

#Importo mi csv con las densidades de cada transecto y con su etiqueta de habitat
#que les corresponde
density_habitats_2023<-read.csv2("kw_habitats_2023.csv")
#Realizo el kruskal-wallis
kruskal_result <- kruskal.test( Density ~ Region.Label,data=density_habitats_2023)
#Presento los resultados en pantalla
print(kruskal_result)
#Un valor de p (0.6131) > a 0.05 indica que no se rechaza la hipótesis nula, 
#es decir que NO hay diferencias significativas entre las medianas de los grupos
#en este caso Kruskal-wallis afirma que las medianas de los grupos son iguales

             

                 # BOX PLOT DE KRUSKAL WALLIS PARA HABITATS 2023 #
library(ggplot2)
ggplot(density_habitats_2023,aes(x=Region.Label,y=Density,fill=Region.Label))+
  geom_boxplot()+
  scale_fill_manual(values = c('ARBUSTAL'="yellowgreen",
                               'ARENAL'="lemonchiffon","HERBAZAL"="darkseagreen"))+
  geom_dotplot(binaxis='y', stackdir='center',dotsize = 0.4,
               fill="black") + labs(x = "Hábitats",  y = "Densidad (Ind/km2)") + 
  guides(fill = guide_legend(title = "Hábitats")) + 
  theme(legend.background = element_rect(color = "black", size = 0.5),
        legend.margin = margin(10, 10, 10, 10), 
        axis.title.x = element_text (size=12),
        axis.title.y = element_text(size=12))


                             # MEDIANAS Para cada habitat #
median(density_habitats_2023[density_habitats_2023$Region.Label =="ARBUSTAL", ]$Density)
median(density_habitats_2023[density_habitats_2023$Region.Label =="ARENAL", ]$Density)    
median(density_habitats_2023[density_habitats_2023$Region.Label =="HERBAZAL", ]$Density)     

                                  # RESUMEN ESTADISTICO #
# arbustal#
library(dplyr)
resultados_2023 <- read.csv2("Resultados_densidad_2023.csv") 
resumen_estadistico_2023_B<- summarize(resultados_2023[1:6,],
                                      N = n(),                # Número de observaciones
                                      Media = mean(Densidad), # Media
                                      Mediana = median(Densidad), # Mediana
                                      Moda = as.numeric(names(sort(table(Densidad), decreasing = TRUE)[1])), # Moda
                                      Desviacion_Estandar = sd(Densidad), # Desviación estándar
                                      Minimo = min(Densidad), # Valor mínimo
                                      Maximo = max(Densidad),  # Valor máximo
                                      IQR = IQR(Densidad),      # Rango intercuartílico
                                      Percentil_25 = quantile(Densidad, 0.25), # Percentil 25
                                      Percentil_75 = quantile(Densidad, 0.75)  # Percentil 75
)
print(resumen_estadistico_2023_B)
write.csv2(resumen_estadistico_2023_B,file = "resu_arbustal_23.csv")
#arenal
library(dplyr)
resultados_2023_ <- read.csv2("Resultados_densidad_2023.csv") 
resumen_estadistico_2023_E <- summarize(resultados_2023[7:9,],
                                      N = n(),                # Número de observaciones
                                      Media = mean(Densidad), # Media
                                      Mediana = median(Densidad), # Mediana
                                      Moda = as.numeric(names(sort(table(Densidad), decreasing = TRUE)[1])), # Moda
                                      Desviacion_Estandar = sd(Densidad), # Desviación estándar
                                      Minimo = min(Densidad), # Valor mínimo
                                      Maximo = max(Densidad),  # Valor máximo
                                      IQR = IQR(Densidad),      # Rango intercuartílico
                                      Percentil_25 = quantile(Densidad, 0.25), # Percentil 25
                                      Percentil_75 = quantile(Densidad, 0.75)  # Percentil 75
)
print(resumen_estadistico_2023_E)
write.csv2(resumen_estadistico_2023_E,file = "resu_arenal_23.csv")


#herbazal
library(dplyr)
resultados_2023 <- read.csv2("Resultados_densidad_2023.csv") 
resumen_estadistico_2023_H <- summarize(resultados_2023[10:27,],
                                      N = n(),                # Número de observaciones
                                      Media = mean(Densidad), # Media
                                      Mediana = median(Densidad), # Mediana
                                      Moda = as.numeric(names(sort(table(Densidad), decreasing = TRUE)[1])), # Moda
                                      Desviacion_Estandar = sd(Densidad), # Desviación estándar
                                      Minimo = min(Densidad), # Valor mínimo
                                      Maximo = max(Densidad),  # Valor máximo
                                      IQR = IQR(Densidad),      # Rango intercuartílico
                                      Percentil_25 = quantile(Densidad, 0.25), # Percentil 25
                                      Percentil_75 = quantile(Densidad, 0.75)  # Percentil 75
)
print(resumen_estadistico_2023_H)
write.csv2(resumen_estadistico_2023_H,file = "resu_herbazal_23.csv")


#.......................................................................................#

            ########## KRUSKAL WALLIS PARA COMPARACIÓN DE HERBAZAL 2021-2022-2023 ########
setwd("D:/Tesis-MR/FEBRERO")        
D_2021<-read.csv2('Resultados_densidad_2021.csv', nrows = 13)
D_2021$año<-c(rep("2021",length(D_2021$Densidad)))

D_2022<-read.csv2("Resultados_densidad_2022.csv",nrows = 7)
D_2022$año<-c(rep("2022",length(D_2022$Densidad)))

D_2023<-read.csv2("Resultados_densidad_2023.csv")
D_2023<-D_2023[10:27,]
D_2023$año<-c(rep("2023",length(D_2023$Densidad)))


dataf<-rbind(D_2021,D_2022,D_2023)
kruskal_años_result <- kruskal.test( Densidad ~ año,data=dataf)
print(kruskal_años_result)
####Un valor de p (0.01435) < a 0.05 indica que se rechaza la hipótesis nula, 
#es decir que SI hay diferencias significativas entre las medianas de los grupos
#en este caso Kruskal-wallis afirma que las medianas de los grupos son diferentes

            # PRUEBA POST-HOC DE DUNN con ajuste de HOLM PARA VER ENTRE QUE GRUPOS HAY DIFERENCIAS #
library(dunn.test)
posthoc_dunn <- dunn.test(dataf$Densidad, g = dataf$año, method = "holm")
print(posthoc_dunn)
  
                      # BOX PLOT DE KRUSKAL WALLIS PARA HERBAZAL DE 2021-2022-2023 #
library(ggplot2)
ggplot(dataf,aes(x=año,y=Densidad,fill=año))+
  geom_boxplot()+
  scale_fill_manual(values = c('2021'="palevioletred",
                               '2022'="skyblue","2023"="yellowgreen"))+
  geom_dotplot(binaxis='y', stackdir='center',dotsize = 0.3,
               fill="black")+
  labs(x="Años", y="Densidad (Ind/km2)")+
  guides(fill = guide_legend(title = "Años")) +
  theme(legend.background = element_rect(color = "black", size = 0.5),
        legend.margin = margin(10, 10, 10, 10), 
        axis.title.x = element_text (size=12),
        axis.title.y = element_text(size=12))

                                # MEDIANAS Para cada habitat #
median(dataf[dataf$año =="2021", ]$Densidad)
median(dataf[dataf$año =="2022", ]$Densidad)
median(dataf[dataf$año =="2023", ]$Densidad)


                                            # RESUMEN ESTADISTICO #
# 2021 #
library(dplyr)
resultados_2021 <- read.csv2("Resultados_densidad_2021.csv") 
resumen_estadistico_2021 <- summarize(resultados_2021[1:13,],
                                      N = n(),                # Número de observaciones
                                      Media = mean(Densidad), # Media
                                      Mediana = median(Densidad), # Mediana
                                      Moda = as.numeric(names(sort(table(Densidad), decreasing = TRUE)[1])), # Moda
                                      Desviacion_Estandar = sd(Densidad), # Desviación estándar
                                      Minimo = min(Densidad), # Valor mínimo
                                      Maximo = max(Densidad),  # Valor máximo
                                      IQR = IQR(Densidad),      # Rango intercuartílico
                                      Percentil_25 = quantile(Densidad, 0.25), # Percentil 25
                                      Percentil_75 = quantile(Densidad, 0.75)  # Percentil 75
)
print(resumen_estadistico_2021)
write.csv2(resumen_estadistico_2021,file="resumen_2021.csv")

# 2022 #
library(dplyr)
resultados_2022 <- read.csv2("Resultados_densidad_2022.csv") 
resumen_estadistico_2022 <- summarize(resultados_2022[1:7,],
                                      N = n(),                # Número de observaciones
                                      Media = mean(Densidad), # Media
                                      Mediana = median(Densidad), # Mediana
                                      Moda = as.numeric(names(sort(table(Densidad), decreasing = TRUE)[1])), # Moda
                                      Desviacion_Estandar = sd(Densidad), # Desviación estándar
                                      Minimo = min(Densidad), # Valor mínimo
                                      Maximo = max(Densidad),  # Valor máximo
                                      IQR = IQR(Densidad),      # Rango intercuartílico
                                      Percentil_25 = quantile(Densidad, 0.25), # Percentil 25
                                      Percentil_75 = quantile(Densidad, 0.75)  # Percentil 75
)
print(resumen_estadistico_2022)
write.csv2(resumen_estadistico_2022,file="resumen_2022.csv")


# 2023#
library(dplyr)
resultados_2023 <- read.csv2("Resultados_densidad_2023.csv") 
resumen_estadistico_2023 <- summarize(resultados_2023[10:27,],
                                      N = n(),                # Número de observaciones
                                      Media = mean(Densidad), # Media
                                      Mediana = median(Densidad), # Mediana
                                      Moda = as.numeric(names(sort(table(Densidad), decreasing = TRUE)[1])), # Moda
                                      Desviacion_Estandar = sd(Densidad), # Desviación estándar
                                      Minimo = min(Densidad), # Valor mínimo
                                      Maximo = max(Densidad),  # Valor máximo
                                      IQR = IQR(Densidad),      # Rango intercuartílico
                                      Percentil_25 = quantile(Densidad, 0.25), # Percentil 25
                                      Percentil_75 = quantile(Densidad, 0.75)  # Percentil 75
)
print(resumen_estadistico_2023)
write.csv2(resumen_estadistico_2023,file="resumen_2023.csv")

