datos=read.csv("Hogares3.csv", dec = ",")


Crear indicador de comodidades basicas, relacionar con nivel educativo alcanzado.


INSE=datos%>%
  select(a1,a2,a6, a91:a910, a912:a919)


ASAP=INSE%>%
  mutate(a1=case_when(
    a1=="1" ~ 5,
    a1=="2"~0),
    a2=case_when(
      a2=="1"~ 5,
      a2=="2" ~0))

