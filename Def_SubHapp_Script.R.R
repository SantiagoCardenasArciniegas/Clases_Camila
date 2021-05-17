#install.packages("easypackages")
library(easypackages)

library(boot)
library(ggm)
library(ggplot2)
library(polycor)
library(Hmisc)
library(dplyr)
library(readxl)
library(devtools)
library(tidyverse)
library(lubridate)
library(ggridges)
library(wesanderson)
library(RColorBrewer)
library(knitr)
library(kableExtra)
library(hrbrthemes)
library(statsr)
library(stargazer)
library(psych)
library(corrplot)
library(corrr)
library(GGally)
library(ggcorrplot)
library(PerformanceAnalytics)
library(pander)
library(broom)
library(purrr)
library(kableExtra)
library(egg)

#library(rstatix)
#library(ggpubr)
webshot::install_phantomjs()
#Hay que instalar webshot::install_phantomjs() para que kable se pueda exportar como png 
#(Solo instalar. No hace falta usar library luego)
#library(plotly) Oculta Select


#libraries("boot","ggm","ggplot2","polycor","Hmisc","dplyr",
          "readxl","devtools","tidyverse","lubridate",
          "ggridges","wesanderson","RColorBrewer","knitr",
          "kableExtra","hrbrthemes","statsr","stargazer","psych",
          "corrplot","corrr","GGally","ggcorrplot","PerformanceAnalytics",
          "pander","broom","purr","kableExtra","egg")


#install.packages(c("boot","ggm","ggplot2","polycor","Hmisc","dplyr",
"readxl","devtools","tidyverse","lubridate",
"ggridges","wesanderson","RColorBrewer","knitr",
"kableExtra","hrbrthemes","statsr","stargazer","psych",
"corrplot","corrr","GGally","ggcorrplot","PerformanceAnalytics",
"pander","broom","purr","kableExtra","egg"))


# Workspace ---------------------------------------------------------------


#Directorio

#setwd("C:/Users/Manuel Cardenas/Desktop/Prueba/Test6")

#Folders

dir.create("Correlation_Matrices_html")
dir.create("Summary_Tables")
dir.create("Correlation_Plots")
dir.create("Regression_Tables")
dir.create("Graficas_Definitivas")



#Cargando datos

data_final_well_being <- read_excel("data_final_well-being.xlsx")



# Exploración de datos ----------------------------------------------------


#Exploración inicial

head(data_final_well_being) #Primeras 10 filas
tail(data_final_well_being) #Últimas 6 filas
dim(data_final_well_being) #Número de filas y número de columnas
str(data_final_well_being) #Estructura de la bas (Tipos de variables)

glimpse(data_final_well_being) #Preview de los datos don Dplyr

summary(data_final_well_being) # Estadísticos descriptivos


##¿Hay NAS?

#Análisis de NAs

sum(is.na(data_final_well_being))
sum(is.na(data_final_well_being$space))

length(data_final_well_being$space)

A<-data_final_well_being %>% 
  na.omit()

sum(is.na(A))

length(A$space)

#Formato de las columnas

  #Parc es una variable con pocos niveles. Asumamos que la quiero
  #tratar, no como una variable numérica sino como una categórica.
  #Es decir, como un factor. Voy a crear una segunda base para hacer
  #La demostración.


Prueba <-data_final_well_being

View(Prueba)

unique(data_final_well_being$parc)

Prueba$parc <- as.factor(Prueba$green)
Prueba$sport <- as.integer(Prueba$sport)

str(Prueba)

#Cambiando el formato de varias variables al tiempo

Prueba[,c(2,3,5, 7)] <- lapply(Prueba[,c(2, 3, 5, 7)], as.factor)
str(Prueba)

Prueba[,c(1:8)] <- lapply(Prueba[,c(1:8)], as.numeric)
str(Prueba)


# Correlaciones -----------------------------------------------------------


#COR (Con Nas)

cor(data_final_well_being)

#La columna space tiene NAS, entonces tengo que introducir un argumento en la función Cor para que no los considere
#al momento de computar la correlación.

cor(data_final_well_being,use = "pairwise.complete.obs")


#Cor sin la columna de ID

data_final_well_being %>% 
  select(2:8) %>% 
  cor(use = "pairwise.complete.obs")


Correlation_Matrix <-data_final_well_being %>% 
  select(2:8) %>% 
  cor(use = "pairwise.complete.obs")


#Correlation Matrix con Stargazer

  #Para visualizar en R

  stargazer(Correlation_Matrix,
          title="Correlation Matrix",
          type = "text")

  #Exportar como objeto html para meter en word
  
  stargazer(Correlation_Matrix,
          title="Correlation Matrix",
          type = "html",
          out = "./Correlation_Matrices_html/Correlation_Matrix.html")
  

  #Summary Statistics con Stargazer. Stargazer solo funciona con 
  #Data Frames. Por ende es necesario utilizar as.data.frame. Con Tibbles
  #No funciona
  
  stargazer(as.data.frame(data_final_well_being),
            type = "text",
            title = "Well Being Summary")
  
  
  stargazer(as.data.frame(data_final_well_being),
            type = "html",
            title = "Well Being Summary",
            out="./Summary_Tables/Well_Being_Summary.html")
  
# Correlation plots -------------------------------------------------------

  #Nueva Base sin columna de ID
  
data_final_well_being_NoId <-data_final_well_being %>% 
  select(c(2:8)) 
  
head(data_final_well_being_NoId)


plot(data_final_well_being_NoId)

###Corplot1
plot(data_final_well_being_NoId)

#Export

    png("./Correlation_Plots/Corplot_1.png",
     width = 2000, height = 1800,type = "cairo",res=300)

    plot(data_final_well_being_NoId)

    dev.off()


    
###Corplot2    
    
  #Omitir ID primera columna sin crear una nueva base

  data_final_well_being %>% 
  select(c(2:8)) %>% 
  corPlot(cex = 1.2)
  
    #Export
  
      png("./Correlation_Plots/Corplot_2.png",
           width = 2000, height = 1800,type="cairo",res=250)
      
      data_final_well_being %>% 
        select(c(2:8)) %>% 
        corPlot(cex = 1.2)
      
      dev.off()


#Con El paquete Corplot 
      
###Corplot 2A
    corrplot(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),   
               method = "square", # Correlation plot method
               type = "full",    # Correlation plot style (also "upper" and "lower")
               diag = TRUE,      # If TRUE (default), adds the diagonal
               tl.col = "black", # Labels color
               bg = "white",     # Background color
               title = "Correlaciones Well Being", # Main title
               col = NULL)
      
  #Export
  png(height=1800, width=1800, file="./Correlation_Plots/Corplot_2A.png", 
      type = "cairo",res = 300)
      
  corrplot(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),   
               method = "square", # Correlation plot method
               type = "full",    # Correlation plot style (also "upper" and "lower")
               diag = TRUE,      # If TRUE (default), adds the diagonal
               tl.col = "black", # Labels color
               bg = "white",     # Background color
               title = "Correlaciones Well Being", # Main title
               col = NULL)

  dev.off()

###Corplot 3
  corrplot(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),        
         method = "shade", 
         type = "lower",    
         diag = TRUE,      
         tl.col = "black", 
         bg = "white",     
         title = "Correlaciones Well Being", # Main title
         col = NULL)

  #Export
  png(height=1800, width=1800, file="./Correlation_Plots/Corplot_3.png", type = "cairo",res = 300)

  corrplot(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),        
         method = "shade", 
         type = "lower",    
         diag = TRUE,      
         tl.col = "black", 
         bg = "white",     
         title = "Correlaciones Well Being", # Main title
         col = NULL)

  dev.off()

###Corplot 4
  corrplot.mixed(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),
                 lower = "circle", 
                 upper = "number",
                 tl.col = "black",
                 title="Correlaciones Well_Being")

  #Export
  png(height=1800, width=1800, file="./Correlation_Plots/Corplot_4.png", type = "cairo",res = 300)

  corrplot.mixed(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),
               lower = "circle", 
               upper = "number",
               tl.col = "black",
               title="Correlaciones Well_Being")

  dev.off()

###Corplot 5
  corrplot.mixed(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),
                 lower = "ellipse", 
                 upper = "number",
                 tl.col = "black",
                 title="Correlaciones Well_Being")
  #Export
  png(height=1800, width=1800, file="./Correlation_Plots/Corplot_5.png", type = "cairo",res = 300)


  corrplot.mixed(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),
               lower = "ellipse", 
               upper = "number",
               tl.col = "black",
               title="Correlaciones Well_Being")

dev.off()


###Corplot 6

  corrplot.mixed(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),
               lower = "pie", 
               upper = "number",
               tl.col = "black",
               title="Correlaciones Well_Being")
  
  #Export
  png(height=1800, width=1800, file="./Correlation_Plots/Corplot_6.png", type = "cairo",res = 300)

  corrplot.mixed(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),
               lower = "pie", 
               upper = "number",
               tl.col = "black",
               title="Correlaciones Well_Being")
  dev.off()

###Corplot 7

  chart.Correlation(data_final_well_being_NoId, histogram=TRUE, pch=19,
                    method = "pearson")

  #Export
  png("./Correlation_Plots/Corplot_7.png",
      width = 2000, height = 1800,type = "cairo",res=300)

  chart.Correlation(data_final_well_being_NoId, histogram=TRUE, pch=19,
                  method = "pearson")
  dev.off()


###Corplot 8 (ggplot)
  
ggpairs(data_final_well_being_NoId,
          title = "Correlaciones Well being")

  #Export
  Plot8<-ggpairs(data_final_well_being_NoId,
        title = "Correlaciones Well being")

  ggsave(plot = Plot8, filename = "./Correlation_Plots/Corplot_8.png", 
       width = 11, 
       height = 7,
       type = "cairo",
       dpi = "retina")

###Corplot 9 (ggplot)

ggcorrplot(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),
             hc.order=TRUE, type='lower',lab = TRUE,
             title = "Correlaciones Well Being")

  #Export

  Plot9<-ggcorrplot(cor(data_final_well_being_NoId,use = "pairwise.complete.obs"),
           hc.order=TRUE, type='lower',lab = TRUE,
           title = "Correlaciones Well Being")

  ggsave(plot = Plot9, filename = "./Correlation_Plots/Corplot_9.png", 
       width = 9, 
       height = 7,
       type = "cairo",
       dpi = "retina")

#Gráficas Básicas con Ggplot ------------------------------------------------------------------

#Histograma básico: Green
  
data_final_well_being %>% 
  ggplot(aes(x=green))+
  geom_histogram(fill="gray", color="black", bins=30)+
  ggtitle("Histograma de espacio verde disponible")+
  labs(x="Edad (años)", y="Frecuencia")


#Histopgrama básico: SHS
data_final_well_being %>% 
  ggplot(aes(x=SHS))+
  geom_histogram(fill="royalblue4")+
  ggtitle("Histograma de SHS")+
  labs(x="SHS", y="Frecuencia")


#Histograma anxdep con distribución normal

data_final_well_being %>% ggplot(aes(x=anxdep, y=..density..))+
  geom_histogram(fill="royalblue4", color="green")+
  geom_line(aes(x=anxdep, y=dnorm(anxdep, mean(anxdep), sd(anxdep))), color="red", size=1)+
  ggtitle("Histograma de edad con curva normal")+
  labs(x="Edad (años)", y="Densidad")+
  theme_minimal()

#Espacio Verde

data_final_well_being %>% 
  ggplot(aes(x=green))+
  geom_histogram(fill="seagreen4")+
  ggtitle("Histograma de espacio verde disponible")+
  labs(x="Espacio Verde", y="Frecuencia")


###Boxplots

  #Básico (Usando nomenclatura Dplyr)
  data_final_well_being %>%
    ggplot(aes(y=anxdep)) +
  geom_boxplot()

  #Básico (sin Dplyr)
  ggplot(data=data_final_well_being,aes(y=anxdep))+
    geom_boxplot()
  

  #Completo. Usando Labels, y filtrando por Sport. Dejando eje X en blanco
  
  data_final_well_being %>%
  filter(sport==6) %>% 
  na.omit() %>%  #No es necesario acá pero es bueno saber que se puede usar
  ggplot(aes(y=anxdep))+
  geom_boxplot(fill="springgreen4")+
  labs(title = "Nivel de ansiedad y depresión",
       subtitle = "Frecuencia deporte = 6",
       y="Nivel de ansiedad y depresión")+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


  
#Side by Side Boxplots (Anxdep_según Sport) Completo. Usando Labels y Brewer Palette
  
data_final_well_being %>%
    ggplot(aes(y=anxdep,x=factor(sport)))+
    geom_boxplot(aes(fill=as.factor(sport)))+
    labs(title = "Nivel de ansiedad y depresión",
         subtitle = "Según Frecuencia deporte",
         y="Nivel de ansiedad y depresión",
         x= "Frecuencia con la cual la persona practica deporte",
         fill="Frec deporte")+
    scale_fill_brewer(palette = "RdPu")+
  theme_classic()
  

  #Export
 
  SBS_Boxplot_Anxdep_Sport<-data_final_well_being %>%
   ggplot(aes(y=anxdep,x=factor(sport)))+
   geom_boxplot(aes(fill=as.factor(sport)))+
   labs(title = "Nivel de ansiedad y depresión",
        subtitle = "Según Frecuencia deporte",
        y="Nivel de ansiedad y depresión",
        x= "Frecuencia con la cual la persona practica deporte",
        fill="Frec deporte")+
   scale_fill_brewer(palette = "RdPu")+
    theme_classic()
 
  ggsave(plot = SBS_Boxplot_Anxdep_Sport, 
       filename = "./Graficas_Definitivas/SBS_Boxplot_Anxdep_Sport.png", 
       width = 11, height = 7,
       type= "cairo",
       dpi = "retina")


  
#Side by Sied Boxplots (Anxdep según Parc) 
  
  data_final_well_being %>%
    ggplot(aes(y=anxdep,x=factor(parc)))+
    geom_boxplot(aes(fill=as.factor(parc)))+
    labs(title = "Nivel de ansiedad y depresión",
         subtitle = "Según Frecuencia parque",
         y="Nivel de ansiedad y depresión",
         x= "Frecuencia con la cual la persona visita el parque",
         fill="Frec deporte")+
    scale_fill_brewer(palette = "RdPu")+
    theme_classic()
  
  #Export
  
  SBS_Boxplot_Anxdep_Parc<- data_final_well_being %>%
    ggplot(aes(y=anxdep,x=factor(parc)))+
    geom_boxplot(aes(fill=as.factor(parc)))+
    labs(title = "Nivel de ansiedad y depresión",
         subtitle = "Según Frecuencia parque",
         y="Nivel de ansiedad y depresión",
         x= "Frecuencia con la cual la persona visita el parque",
         fill="Frec deporte")+
    scale_fill_brewer(palette = "RdPu")+
    theme_classic()
  
  
  ggsave(plot = SBS_Boxplot_Anxdep_Parc, 
         filename = "./Graficas_Definitivas/SBS_Boxplot_Anxdep_Parc.png", 
         width = 11, height = 7,
         type= "cairo",
         dpi = "retina")
  
  
  ###SHS
  
  #Side by Sied Boxplots (SHS_según Sport) 
  
  SBS_Boxplot_1<-data_final_well_being %>%
    ggplot(aes(y=SHS,x=factor(sport)))+
    geom_boxplot(aes(fill=as.factor(sport)))+
    labs(title = "Nivel de ansiedad y depresión",
         subtitle = "Según Frecuencia deporte",
         y="Nivel de felicidad",
         x= "Frecuencia con la cual la persona practica deporte",
         fill="Frec deporte")+
    scale_fill_brewer(palette = "RdPu")+
    theme_classic()
  
  
  #Export
  
  SBS_Boxplot_SHS_Sport<-data_final_well_being %>%
    ggplot(aes(y=SHS,x=factor(sport)))+
    geom_boxplot(aes(fill=as.factor(sport)))+
    labs(title = "Nivel de ansiedad y depresión",
         subtitle = "Según Frecuencia deporte",
         y="Nivel de felicidad",
         x= "Frecuencia con la cual la persona practica deporte",
         fill="Frec deporte")+
    scale_fill_brewer(palette = "RdPu")+
    theme_classic()
  
  ggsave(plot = SBS_Boxplot_SHS_Sport, 
         filename = "./Graficas_Definitivas/SBS_Boxplot_SHS_Sport.png", 
         width = 11, height = 7,
         type= "cairo",
         dpi = "retina")
  
  
  
  #Side by Sied Boxplots (SHS según Parc) 
  
  data_final_well_being %>%
    ggplot(aes(y=SHS,x=factor(parc)))+
    geom_boxplot(aes(fill=as.factor(parc)))+
    labs(title = "Nivel de ansiedad y depresión",
         subtitle = "Según Frecuencia parque",
         y="Nivel de felicidad",
         x= "Frecuencia con la cual la persona visita el parque",
         fill="Frec deporte")+
    scale_fill_brewer(palette = "RdPu")+
    theme_classic()
  
  
  #Export
  
  SBS_Boxplot_SHS_Parc<- data_final_well_being %>%
    ggplot(aes(y=SHS,x=factor(parc)))+
    geom_boxplot(aes(fill=as.factor(parc)))+
    labs(title = "Nivel de ansiedad y depresión",
         subtitle = "Según Frecuencia parque",
         y="Nivel de felicidad",
         x= "Frecuencia con la cual la persona visita el parque",
         fill="Frec deporte")+
    scale_fill_brewer(palette = "RdPu")+
    theme_classic()
  
  
  ggsave(plot = SBS_Boxplot_SHS_Parc, 
         filename = "./Graficas_Definitivas/SBS_Boxplot_SHS_Parc.png", 
         width = 11, height = 7,
         type= "cairo",
         dpi = "retina")
  

  
## Scatterplots 

#Básico

data_final_well_being %>% 
  ggplot(aes(y=SHS,x=SES))+
  geom_point()+
  geom_smooth(method=lm, color="darkred", fill="blue")+
  labs(title="Nivel de felicidad vs Estatus socio-económico")


#Con facet_grid según Sport

data_final_well_being %>% 
  ggplot(aes(y=SHS,x=anxdep,color=factor(sport)))+
  geom_point()+
  geom_smooth(method=lm, color="darkred", fill="dodgerblue4")+
  labs(title="Nivel de felicidad vs Ansiedad y depresión", 
       subtitle="Según Frecuencia Deporte",color="Frecuencia deporte")+
  facet_grid( ~sport)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()

  #Export 

  SHS_Anxdep_Facetgrid_Sport<-data_final_well_being %>% 
  ggplot(aes(y=SHS,x=anxdep,color=factor(sport)))+
  geom_point()+
  geom_smooth(method=lm, color="darkred", fill="dodgerblue4")+
  labs(title="Nivel de felicidad vs Ansiedad y depresión", 
       subtitle="Según Frecuencia Deporte",color="Frecuencia deporte")+
  facet_grid( ~sport)+
  scale_color_brewer(palette="Dark2")+
    theme_minimal()


  ggsave(plot = SHS_Anxdep_Facetgrid_Sport, 
       filename = "./Graficas_Definitivas/SHS_Anxdep_Facetgrid_Sport.png", 
       width = 14, height = 7,
       type= "cairo",
       dpi = "retina")

#Con facet_grid según Parc

data_final_well_being %>% 
  ggplot(aes(y=SHS,x=anxdep,color=factor(parc)))+
  geom_point()+
  geom_smooth(method=lm, color="darkred", fill="dodgerblue4")+
  labs(title="Nivel de felicidad vs Ansiedad y depresión", 
       subtitle="Según frecuencia parque",color="Frecuencia parque")+
  facet_grid( ~parc)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()

  #Export

  SHS_Anxdep_Facetgrid_Parc<- data_final_well_being %>% 
  ggplot(aes(y=SHS,x=anxdep,color=factor(parc)))+
  geom_point()+
  geom_smooth(method=lm, color="darkred", fill="dodgerblue4")+
  labs(title="Nivel de felicidad vs Ansiedad y depresión", 
       subtitle="Según frecuencia parque",color="Frecuencia parque")+
  facet_grid( ~parc)+
  scale_color_brewer(palette="Dark2")+
    theme_minimal()
  
  
  ggsave(plot = SHS_Anxdep_Facetgrid_Parc, 
         filename = "./Graficas_Definitivas/SHS_Anxdep_Facetgrid_Parc.png", 
         width = 14, height = 7,
         type= "cairo",
         dpi = "retina")
  

###Side-By-Side Scatterplots con SHS. Usando Ggarrange (paquete egg) 

A<-data_final_well_being %>% 
  ggplot(aes(y=SHS,x=SES))+
  geom_point()+
  geom_smooth(method=lm, color="darkred", fill="blue")+
  labs(title="Nivel de felicidad vs Estatus socio-económico")+
  theme_classic()


B <- data_final_well_being %>% 
  ggplot(aes(y=SHS,x=green))+
  geom_point()+
  geom_smooth(method=lm, color="darkred", fill="blue")+
  labs(title="Nivel de felicidad vs Espacio Verde Disponible")+
  theme_classic()

C<- data_final_well_being %>% 
  ggplot(aes(y=SHS,x=sport))+
  geom_point()+
  geom_smooth(method=lm, color="darkred", fill="blue")+
  labs(title="Nivel de felicidad vs Frecuencia deporte")+
  theme_classic()

D<- data_final_well_being %>% 
  ggplot(aes(y=SHS,x=space))+
  geom_point()+
  geom_smooth(method=lm, color="darkred", fill="blue")+
  labs(title="Nivel de felicidad vs Space")+
  theme_classic()

E<- data_final_well_being %>% 
  ggplot(aes(y=SHS,x=parc))+
  geom_point()+
  geom_smooth(method=lm, color="darkred", fill="blue")+
  labs(title="Nivel de felicidad vs parque")+
  theme_classic()


ggarrange(A,B,C,D,E, ncol = 3, nrow = 2)

  #Export

  SBS_Scatterplot_SHS<-ggarrange(A,B,C,D,E, ncol = 3, nrow = 2)

  ggsave(plot = SBS_Scatterplot_SHS, 
       filename = "./Graficas_Definitivas/SBS_Scatterplot_SHS.png", 
       width = 16, height = 7,
       type= "cairo",
       dpi = "retina")

  
#########
  
  ##Anxdep

  ###Side-By-Side Scatterplots con Anxdep. Usando Ggarrange (paquete egg) 
  
  A.1<-data_final_well_being %>% 
    ggplot(aes(y=anxdep,x=SES))+
    geom_point()+
    geom_smooth(method=lm, color="darkred", fill="blue")+
    labs(title="Nivel de ansiedad y derpesión vs Estatus socio-económico")+
    theme_classic()
  
  
  B.1 <- data_final_well_being %>% 
    ggplot(aes(y=anxdep,x=green))+
    geom_point()+
    geom_smooth(method=lm, color="darkred", fill="blue")+
    labs(title="Nivel de ansiedad y depresión vs Espacio Verde Disponible")+
    theme_classic()
  
  C.1<- data_final_well_being %>% 
    ggplot(aes(y=anxdep,x=sport))+
    geom_point()+
    geom_smooth(method=lm, color="darkred", fill="blue")+
    labs(title="Nivel de ansiedad y depresión vs Frecuencia deporte")+
    theme_classic()
  
  D.1<- data_final_well_being %>% 
    ggplot(aes(y=anxdep,x=space))+
    geom_point()+
    geom_smooth(method=lm, color="darkred", fill="blue")+
    labs(title="Nivel de ansiedad y depresión vs Space")+
    theme_classic()
  
  E.1<- data_final_well_being %>% 
    ggplot(aes(y=anxdep,x=parc))+
    geom_point()+
    geom_smooth(method=lm, color="darkred", fill="blue")+
    labs(title="Nivel de ansiedad y depresión vs parque")+
    theme_classic()
  
  
  ggarrange(A.1,B.1,C.1,D.1,E.1, ncol = 3, nrow = 2)
  
  #Export
  
  SBS_Scatterplot_Anxdep<-  ggarrange(A.1,B.1,C.1,D.1,E.1, ncol = 3, nrow = 2)
  
  ggsave(plot = SBS_Scatterplot_Anxdep, 
         filename = "./Graficas_Definitivas/SBS_Scatterplot_Anxdep.png", 
         width = 16, height = 7,
         type= "cairo",
         dpi = "retina")
  
  
# Regresiones lineales -------------------------------------------------------------

#Modelo 1. Variable dependiente SHS

m1_SHS_Full<-lm(SHS ~ green +
         space +
          parc +
          SES+
          sport,
       data = data_final_well_being)

summary(m1_SHS_Full)



#M1. Regresiones Simples por cada variable dependiente

A<-lm(SHS ~ as.factor(sport)+ as.factor(parc),data = data_final_well_being)

summary(A)
 #Green
  m1_SHS_Full_Green<-lm(SHS ~ green,
                data = data_final_well_being)

    summary(m1_SHS_Full_Green)


  #Space
    m1_SHS_Full_Space<-lm(SHS ~ space,
                          data = data_final_well_being)
    
    summary(m1_SHS_Full_Space)

  
  #Parc
    m1_SHS_Full_Parc<-lm(SHS ~ parc,
                          data = data_final_well_being)
    
    summary(m1_SHS_Full_Parc)

    
  #SES
    m1_SHS_Full_SES<-lm(SHS ~ SES,
                         data = data_final_well_being)
    
    summary(m1_SHS_Full_SES)
    
    
  #Sport
    m1_SHS_Full_Sport<-lm(SHS ~ sport,
                        data = data_final_well_being)
    
    summary(m1_SHS_Full_Sport)

##Uniendo todos los modelos (m1) en una sola tabla
  
stargazer(m1_SHS_Full_Green,
              m1_SHS_Full_Parc,
              m1_SHS_Full_SES,
              m1_SHS_Full_Space,
              m1_SHS_Full_Sport,
              m1_SHS_Full,
              type = "text",
              title="Nivel de felicidad",
              align=TRUE)    
    
  #Export   

  stargazer(m1_SHS_Full_Green,
          m1_SHS_Full_Parc,
          m1_SHS_Full_SES,
          m1_SHS_Full_Space,
          m1_SHS_Full_Sport,
          m1_SHS_Full,
          type = "html",
          title="Nivel de felicidad",
          align=TRUE,
          out = "Regression_Tables/SBS_m1_SHS.html")



######M2_Anxdep

m2_Anxdep_Full<-lm(anxdep ~ green +
         space +
         parc +
         SES+
         sport,
       data = data_final_well_being)

summary(m2_Anxdep_Full)


#M2. Regresiones Simples

  #Green
    m2_Anxdep_Full_Green<-lm(anxdep ~ green,
                      data = data_final_well_being)

   summary(m2_Anxdep_Full_Green)


  #Space
    m2_Anxdep_Full_Space<-lm(anxdep ~ space,
                      data = data_final_well_being)

    summary(m2_Anxdep_Full_Space)


  #Parc
    m2_Anxdep_Full_Parc<-lm(anxdep ~ parc,
                     data = data_final_well_being)

    summary(m2_Anxdep_Full_Parc)


  #SES
    m2_Anxdep_Full_SES<-lm(anxdep ~ SES,
                    data = data_final_well_being)

    summary(m2_Anxdep_Full_SES)


  #Sport
    m2_Anxdep_Full_Sport<-lm(anxdep ~ sport,
                      data = data_final_well_being)

    summary(m2_Anxdep_Full_Sport)


##Uniendo todos los modelos (m2) en una sola tabla
    
stargazer(m2_Anxdep_Full_Green,
              m2_Anxdep_Full_Parc,
              m2_Anxdep_Full_SES,
              m2_Anxdep_Full_Space,
              m2_Anxdep_Full_Sport,
              m2_Anxdep_Full,
              type = "text",
              title="Nivel de ansiedad y depresión",
              align=TRUE)

  #Export

  stargazer(m2_Anxdep_Full_Green,
          m2_Anxdep_Full_Parc,
          m2_Anxdep_Full_SES,
          m2_Anxdep_Full_Space,
          m2_Anxdep_Full_Sport,
          m2_Anxdep_Full,
          type = "html",
          title="Nivel de ansiedad y depresión",
          align=TRUE,
          out = "Regression_Tables/SBS_m2_Anxdep.html")



# Inference ---------------------------------------------------------------



#Usando la función Inference
  
  #Hypothesis test
  inference(data=data_final_well_being, y = anxdep, statistic = "mean", type = "ht", 
              null = 0, alternative = "twosided", method = "theoretical")
  
  #Confidence Interval
  inference(data=data_final_well_being, y = anxdep, statistic = "mean", type = "ci", 
              null = 0, alternative = "twosided", method = "theoretical")
  
  t.test(x=data_final_well_being$anxdep)

  
