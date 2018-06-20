datos=read.csv("Hogares3.csv", dec = ",")



#Crear indicador de comodidades basicas, relacionar con nivel educativo alcanzado.

library(tidyverse)


INSE=datos%>%
  select(a1,a2,a6, a91:a910, a912, a914, a916:a917, a919)


ICH=INSE%>%
  mutate(a1=case_when(
    a1=="1" ~ 0,
    a1=="2"~10),
    a2=case_when(
      a2=="1"~ 10,
      a2=="2" ~8,
      a2=="3" ~5,
      a2=="4" ~3,
      a2=="5" ~0
    ),
    a6=case_when(
      a6=="1"~ 0,
      a6=="2" ~2,
      a6=="3" ~3,
      a6=="4" ~4,
      a6=="5" ~5,
      a6=="6" ~5,
      a6=="7" ~5,
      a6=="8" ~5,
      a6=="9" ~5
    ),
    a91=case_when(
      a91=="1" ~ 7,
      a91=="2"~0),
    a92=case_when(
      a92=="1" ~ 7,
      a92=="2"~0),
    a93=case_when(
      a93=="1" ~ 5,
      a93=="2"~0),
    a94=case_when(
      a94=="1" ~ 2,
      a94=="2"~0),
    a95=case_when(
      a95=="1" ~ 7,
      a95=="2"~0),
    a96=case_when(
      a96=="1" ~ 6,
      a96=="2"~0),
    a97=case_when(
      a97=="1" ~ 3,
      a97=="2"~0),
    a98=case_when(
      a98=="1" ~ 5,
      a98=="2"~0),
    a99=case_when(
      a99=="1" ~ 2,
      a99=="2"~0),
    a910=case_when(
      a910=="1" ~ 2,
      a910=="2"~0),
    a912=case_when(
      a912=="1" ~ 4,
      a912=="2"~0),
    a914=case_when(
      a914=="1" ~ 6,
      a914=="2"~0),
    a916=case_when(
      a916=="1" ~ 6,
      a916=="2"~0),
    a917=case_when(
      a917=="1" ~ 8,
      a917=="2"~0),
    a919=case_when(
      a919=="1" ~ 5,
      a919=="2"~0)
  )

ICH[is.na(ICH)] <- 0


ICH=ICH%>%
  mutate(Puntaje=rowSums(ICH))

#Indicador Comfort del Hogar

ICH%>%
  ggplot(aes(x=Puntaje))+
  geom_histogram(fill="white", colour="black")


datos=cbind(datos, ICH[,19])

#Relacion ICH , repitio algun año

library(arules)
library(scales)

datos$`ICH[, 19]`=discretize(datos$`ICH[, 19]`, method = "interval", breaks = 10)
str(datos$`ICH[, 19]`)

datos=datos%>%
  mutate(b2=case_when(
    b2=="1" ~ "SI",
    b2=="2"~"NO"))

datos$b2=as.factor(datos$b2)

#EDUCACION 

#Repitio año (1)

datos%>%
  filter(!is.na(b2))%>%
  ggplot()+
  geom_bar(aes((`ICH[, 19]`), fill=b2), position= "fill")+
  labs(x="ICH en intervalos", y="Proporción", fill="Repitió año")


#Asistencia a i.educativo (2)

datos=datos%>%
  mutate(b4=case_when(
    b4=="1" ~ "SI",
    b4=="2"~"NO"))

datos$b4=as.factor(datos$b4)

datos%>%
  filter(!is.na(b4))%>%
  ggplot()+
  geom_bar(aes((`ICH[, 19]`), fill=b4), position= "fill")+
  labs(x="ICH en intervalos", y="Proporción", fill="Asiste IE")

#Tipo de i.educativo (3)

datos=datos%>%
  mutate(b6=case_when(
    b6=="1" ~ "PUBLICO",
    b6=="2"~ "PRIVADO"))

datos$b6=as.factor(datos$b6)

datos%>%
  filter(!is.na(b6))%>%
  ggplot()+
  geom_bar(aes((`ICH[, 19]`), fill=b6), position= "fill")+
  labs(x="ICH en intervalos", y="Proporción", fill="Publico/Privado")


#Nivel esperado (4)

datos=datos%>%
  mutate(b10=case_when(
    b10=="1" ~ 1,
    b10=="2"~2,
    b10=="3"~3,
    b10=="4"~4,
    b10=="5"~5))

datos$b10=as.factor(datos$b10)

datos%>%
  filter(!is.na(b10))%>%
  ggplot()+
  geom_bar(aes((`ICH[, 19]`), fill=b10), position= "fill")+
  labs(x="ICH en intervalos", y="Nivel esperado", fill="Cantidad de obs")


#PADRES

#Boxplot edad de concepción/ICH (1)

datos$c4=as.numeric(as.character(datos$c4))

datos%>%
  filter(!is.na(c4), !is.na(c5), !is.na(c7), c7!="99")%>%
  ggplot(aes(y=c4, x=reorder(`ICH[, 19]`, c4, FUN = median)))+
  geom_boxplot()


#Convivencia con los padres (2)
  
datos=datos%>%
  mutate(b33a=case_when(
    b33a=="1" ~ "No es cierto",
    b33a=="2"~"Un tanto cierto"))

datos$b33a=as.factor(datos$b33a)

datos%>%
  filter(!is.na(b33a))%>%
  ggplot()+
  geom_bar(aes((`ICH[, 19]`), fill=b33a), position= "fill")+
  labs(x="ICH en intervalos", y="Proporción", fill="Vive c/2 padres")




# Expectativas sobre el hijo (3)

datos=datos%>%
  mutate(b31a=case_when(
    b31a=="1" ~ "Reconocida y exitosa",
    b31a=="2"~"Recursos suficientes",
    b31a=="3" ~ "Estudie a gusto",
    b31a=="4" ~ "Supere $ padres",
    b31a=="5" ~ "Supere $ amigos"))

datos$b31a=as.factor(datos$b31a)

datos%>%
  filter(!is.na(b31a))%>%
  ggplot()+
  geom_bar(aes((`ICH[, 19]`), fill=b31a), position= "fill")+
  labs(x="ICH en intervalos", y="Proporción", fill="Expectativas")


  
#SALUD

# Percepcion e. salud (1)

datos=datos%>%
  mutate(b15=case_when(
    b10=="1" ~ 1,
    b10=="2"~2,
    b10=="3"~3,
    b10=="4"~4,
    b10=="5"~5))

datos$b15=as.factor(datos$b15)

datos%>%
  filter(!is.na(b15))%>%
  ggplot()+
  geom_count(aes(`ICH[, 19]`, b15))+
  labs(x="ICH en intervalos", y="Percepción estado de salud", size="Cantidad de obs")



#Dificultades / asistencia psiquiatrica (2)

datos%>%
  mutate(b1=case_when(
    b1=="1" ~"No" ,
    b1=="2"~"Si, moderada",
    b1=="3"~"Si, severa"
  ))

datos%>%
  mutate(b17=case_when(
    b17=="1" ~"Si" ,
    b17=="2"~"No"))


datos$b1=as.factor(datos$b1)
datos$b17=as.factor(datos$b17)


datos%>%
  filter(!is.na(b1), !is.na(b17), b17!="ns_nc", b17!="", b1!="ns_nc", b1!="")%>%
  ggplot()+
  geom_bar(aes(b1, fill=b17), position = "fill")+
  labs(x="Dificultades en el aprendizaje", y="Proporción", fill="Asistencia psicológica/psiquiátrica")


# Motivos hospitalizacion (3)

datos%>%
  mutate(b20=case_when(
    b20=="1" ~ "Enfermedad",
    b20=="2"~"Quemaduras",
    b20=="3"~"Agresión física",
    b20=="4"~"Otros accidentes",
    b20=="5"~"Operación",
    b20=="6"~"Análisis/estudios médicos",
    b20=="7"~"Otros motivos"))

datos$b20=as.factor(datos$b20)



datos%>%
  filter(b18=="1", !is.na(b20))%>%
  group_by(b20)%>%
  summarise(conteo=n())%>%
  ggplot(aes(fct_reorder(b20, conteo, .desc = F), y=conteo))+
  geom_point()+
  coord_flip()

#amigos / asistencia psiquiatrica (4)

datos$b21=as.factor(datos$b21)

datos%>%
  filter(!is.na(b21), !is.na(b17), b17!="ns_nc", b17!="", b21!="ns_nc", b21!="")%>%
  ggplot()+
  geom_bar(aes(b21, fill=b17), position = "fill")+
  labs(x="Cantidad de amigos", y="Proporción", fill="Asistencia psicológica/psiquiátrica")














