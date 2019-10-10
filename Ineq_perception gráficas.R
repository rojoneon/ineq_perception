########################################################
#Datos de perepciones de desigualdad a nivel mundial
########################################################
rm(list = ls())

### Instalar una paquetería que facilita apertura de paqueterías: pacman
#install.packages("pacman") 
library(pacman)

# Abrimos las paqueterías con un sólo comando:
p_load(ineq, haven, readr, readxl, ggplot2, shiny, tidyverse, expss, DescTools, lmtest)
library("wesanderson")
names(wes_palettes)



### Setup ----
options(scipen=999) # Prevenir notación científica
#-----------------

###Carpeta de trabajo y carga de datos
setwd("/Users/maximoernestojaramillomolina/Library/Mobile Documents/com~apple~CloudDocs/Escuela/Escuela/2017A/Congresos y mas/RC19/Cifras paper/")
datos <- read_dta("datos gráfica.dta")

datos$continente =factor(datos$continente,
                         levels = c(1,2,3,4,5),
                         labels = c("UE", "AL", "AE&P", "EUA", "Otros"))

x0 <- ggplot(data = datos , aes(x = datos$percep_ineq20082010 , y = datos$ineq2009)) +
  scale_color_manual(values=wes_palette(n=5, name="Cavalcanti1")) +
  theme_minimal() +
  theme (text = element_text(family = "Verdana"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "whitesmoke", linetype = "blank"),
         plot.background = element_rect(fill = "whitesmoke", linetype = "blank"),
         plot.title = element_text(face = "bold"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9))+
  scale_y_continuous(breaks = seq(0,70,10)) +
  scale_x_continuous(breaks = seq(0,90,10)) +
  labs ( x = "Coeficiente de Gini (circa 2009)",
         y = "Percepción de desigualdad (2009)",
         title = "Desigualdad subjetiva y objetiva",
         subtitle = "Por países, 2009",
         caption = "Fuente: Elaborado por @rojo_neon, con datos de ISSP y Banco Mundial",
         color = "Región") +  
  geom_abline(intercept = 0, slope = 1, linetype = "dashed" , color="Grey")
x0  
ggsave("Desigualdad subjetiva y objetiva por país__diagonal.png", width = 5.5)




x1 <- ggplot(data = datos , aes(x = datos$ineq2009 , y = datos$percep_ineq20082010)) +
  geom_point(mapping  = aes(color=datos$continente)) +
  geom_text(aes(label=datos$country, color=datos$continente), size=2, hjust = 0, nudge_x = 1) +
  scale_color_manual(values=wes_palette(n=5, name="Cavalcanti1")) +
  theme_minimal() +
  theme (text = element_text(family = "Verdana"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "whitesmoke", linetype = "blank"),
         plot.background = element_rect(fill = "whitesmoke", linetype = "blank"),
         plot.title = element_text(face = "bold"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9))+
  scale_y_continuous(breaks = seq(0,90,10)) +
  scale_x_continuous(breaks = seq(0,70,10)) +
  labs ( x = "Coeficiente de Gini (circa 2009)",
         y = "Percepción de desigualdad (2009)",
         title = "Desigualdad subjetiva y objetiva",
         subtitle = "Por países, 2009",
         caption = "Fuente: Elaborado por @rojo_neon, con datos de ISSP y Banco Mundial",
         color = "Región") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed" , color="Grey")
x1  
ggsave("Desigualdad subjetiva y objetiva por país_1.png", width = 5.5)


x2 <- ggplot(data = datos , aes(x = datos$ineq2009, y = datos$wage_gap)) +
  geom_point(mapping  = aes(color=datos$continente)) +
  geom_text(aes(label=datos$country, color=datos$continente), size=2, hjust = 0, nudge_x = 1) +
  scale_color_manual(values=wes_palette(n=5, name="Cavalcanti1")) +
  theme_minimal() +
  theme (text = element_text(family = "Verdana"),
         panel.grid = element_blank(),
         plot.margin = unit(c(10,30,10,20), units = "point"),
         panel.background = element_rect(fill = "whitesmoke", linetype = "blank"),
         plot.background = element_rect(fill = "whitesmoke", linetype = "blank"),
         plot.title = element_text(face = "bold"),
         plot.caption = element_text(hjust = 0.5, lineheight = 0.9))+
  scale_y_continuous(breaks = seq(0,90,10)) +
  scale_x_continuous(breaks = seq(0,70,10)) +
  labs ( x = "Coeficiente de Gini (circa 2009)",
         y = "Brecha salarial percibida (2009)",
         title = "Desigualdad subjetiva y objetiva",
         subtitle = "Por países, 2009",
         caption = "Fuente: Elaborado por @rojo_neon, con datos de ISSP y Banco Mundial",
         color = "Región") +
  geom_abline(intercept = -20, slope = 1, linetype = "dashed" , color="Grey")

x2
ggsave("Desigualdad subjetiva y objetiva por país_2.png", width = 5.5)
