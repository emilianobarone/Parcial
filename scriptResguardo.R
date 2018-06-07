datos=read.csv("Hogares3.csv", dec = ",")


#Crear indicador de comodidades basicas, relacionar con nivel educativo alcanzado.


INSE=datos%>%
  select(a1,a2,a6, a91:a910, a912, a914, a916:a917, a919)


ASAP=INSE%>%
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

ASAP%>%
  na.omit()
  mutate(Puntaje=sum(a1+a2+a6+a91+a92+a93+a94+a95+a96+a97+a98+a99+a910+a912+a914+a916+a917+a919))

  

