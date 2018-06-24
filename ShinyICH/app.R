library(plotly)
library(shiny)
library(arules)
library(tidyverse)

ui <- fluidPage(
  
  titlePanel(tags$h1("SHINY APP ICH")),
  
  tabsetPanel(
    
    tabPanel("ICH",  
             
             fluidRow(
               
               column(width = 4, wellPanel(sliderInput(inputId =  "sliderh" , label = "Numero de barras" ,min = 1, max = 60, value = 30)))
               , column(width = 8, plotOutput("ploth" )))),
    
    
    tabPanel("Educación",  
             
             fluidRow(
               column(width = 4, wellPanel(
                 radioButtons("Barplots", "Tipo de Barplot",
                              c("Repitió año", "Asiste I.Educativo", "Tipo de instituto", "Expectativas sobre el hijo")
                 )
               )),
               column(width = 8,
                      
                      
                      plotlyOutput("barplot1", height = 350
                                   
                      )
               )
             ),
             
             fluidRow( column(4),
                       column(8, verbatimTextOutput("texto1")))
    ),
    
    tabPanel("Padres",
             fluidRow(
               column(4, wellPanel(
                 radioButtons("padres", "Gráfica",
                              c("Boxplot concepción", "Convivencia con los padres", "Expectativas sobre los hijos")))),
               column(8,
                      plotlyOutput("plot1"))),
             
             fluidRow( column(4),
                       column(8, verbatimTextOutput("texto3")))
             
    ),
    
    
    tabPanel("Salud",
             
             fluidRow(
               
               column(width = 4, wellPanel(
                 radioButtons("Graficas", "Tipo de gráfica",
                              c("Percepción estado salud", "Dificultades aprendizaje", "Motivos hospitalización")
                 )
               )),
               column(width = 8,
                      plotlyOutput("barplot2", height = 350
                                   
                      )
               )),
             fluidRow( column(4),
                       
                       column(8, verbatimTextOutput("texto2"))))))




server <- function(input, output) {
  
  datos=read.csv("Hogares3.csv", dec = ",")
  
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
  
  
  datos=cbind(datos, ICH[,19])
  
  
  datos$`ICH[, 19]`=discretize(datos$`ICH[, 19]`, method = "interval", breaks = 10)
  
  
  output$barplot1 <- renderPlotly({
    if (input$Barplots == "Repitió año") {datos=read.csv("Hogares3.csv", dec = ",")
    
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
    
    
    datos=cbind(datos, ICH[,19])
    
    
    datos$`ICH[, 19]`=discretize(datos$`ICH[, 19]`, method = "interval", breaks = 10)
    
    
    datos=datos%>%
      mutate(b2=case_when(
        b2=="1" ~ "SI",
        b2=="2"~"NO"))
    
    datos$b2=as.factor(datos$b2)
    
    a=datos%>%
      filter(!is.na(b2))%>%
      ggplot()+
      geom_bar(aes((`ICH[, 19]`), fill=b2), position= "fill")+
      labs(x="ICH en intervalos", y="Proporción", fill="Repitió año")+
      ggtitle("Barplot desempeño educativo.")
    
    ggplotly(a)
    
    
    } else if (input$Barplots == "Asiste I.Educativo") {
      datos=read.csv("Hogares3.csv", dec = ",")
      
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
      
      datos=cbind(datos, ICH[,19])
      
      datos$`ICH[, 19]`=discretize(datos$`ICH[, 19]`, method = "interval", breaks = 10)
      str(datos$`ICH[, 19]`)
      
      datos=datos%>%
        mutate(b4=case_when(
          b4=="1" ~ "SI",
          b4=="2"~"NO"))
      
      datos$b4=as.factor(datos$b4)
      
      b=datos%>%
        filter(!is.na(b4))%>%
        ggplot()+
        geom_bar(aes((`ICH[, 19]`), fill=b4), position= "fill")+
        labs(x="ICH en intervalos", y="Proporción", fill="Asiste IE")+
        ggtitle("Barplot asistencia a centro educativo")
      
      ggplotly(b)
      
      
    }else if (input$Barplots == "Tipo de instituto"){
      datos=read.csv("Hogares3.csv", dec = ",")
      
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
      
      datos=cbind(datos, ICH[,19])
      
      datos$`ICH[, 19]`=discretize(datos$`ICH[, 19]`, method = "interval", breaks = 10)
      str(datos$`ICH[, 19]`)
      
      datos=datos%>%
        mutate(b6=case_when(
          b6=="1" ~ "PUBLICO",
          b6=="2"~ "PRIVADO"))
      
      datos$b6=as.factor(datos$b6)
      
      c=datos%>%
        filter(!is.na(b6))%>%
        ggplot()+
        geom_bar(aes((`ICH[, 19]`), fill=b6), position= "fill")+
        labs(x="ICH en intervalos", y="Proporción", fill="Publico/Privado")+
        ggtitle("Barplot tipo de instituto educativo")
      
      ggplotly(c)
      
    }else if (input$Barplots == "Expectativas sobre el hijo"){
      datos=read.csv("Hogares3.csv", dec = ",")
      
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
      
      datos=cbind(datos, ICH[,19])
      
      datos$`ICH[, 19]`=discretize(datos$`ICH[, 19]`, method = "interval", breaks = 10)
      str(datos$`ICH[, 19]`)
      
      datos=datos%>%
        mutate(b31a=case_when(
          b31a=="1" ~ "Reconocida y exitosa",
          b31a=="2"~"Recursos suficientes",
          b31a=="3" ~ "Estudie a gusto",
          b31a=="4" ~ "Supere $ padres",
          b31a=="5" ~ "Supere $ amigos"))
      
      datos$b31a=as.factor(datos$b31a)
      
      d=datos%>%
        filter(!is.na(b31a))%>%
        ggplot()+
        geom_bar(aes((`ICH[, 19]`), fill=b31a), position= "fill")+
        labs(x="ICH en intervalos", y="Proporción", fill="Expectativas")+
        ggtitle("Barplot expectativas de los hijos")
      ggplotly(d)
    }
  })
  
  
  output$texto1= renderText({
    if (input$Barplots == "Repitió año") {"El gráfico muestra en celeste la proporción de jóvenes que repitieron al menos un año educativo y en rojo los que no; según el ICH. Podemos observar que a medida que el ICH aumenta, disminuye la proporción de los individuos que repitieron"}
    else if(input$Barplots=="Asiste I.Educativo"){"Se puede apreciar como, a mayor ICH hay una proporción más alta de jóvenes que asisten a centros educativos; alcanzando casi el 100% los que tienen un ICH entre 90 y 100."}
    else if (input$Barplots=="Tipo de instituto"){"En este gráfico salta a la vista que la gran mayoría de los jóvenes entrevistados concurren a institutos públicos. Por otro lado, podemos ver que la mayor proporción de los que asisten a instituto privado se encuentra en el intervalo 90-100 del ICH."}
    else if(input$Barplots=="Expectativas sobre el hijo"){"En este gráfico representamos las expectativas que tienen los padres al desempeño educativo de su hijo, según el ICH. Podemos observar que en los hogares con ICH más alto, la proporción de los padres con expectativas más altas (facultad y bachillerato) aumenta.Algo que resulta curioso es que los padres pertenecientes a los hogares con ICH entre 10 y 20 descartan la posibilidad de que su hijo sea graduado universitario, mientras que las expectativas respecto a la finalización del bachillerato o UTU son altas. Este intervalo, además, es el que presenta un porcentaje mayor de expectativas puestas en que el hijo culmine el año que cursa actualmente."}})
  
  
  output$ploth=renderPlot({INSE=datos%>%
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
  
  
  
  ICH%>%
    ggplot(aes(x=Puntaje))+
    geom_histogram(fill="white", colour="black", bins =  input$sliderh)})
  
  
  
  
  output$plot1 <- renderPlotly({
    if (input$padres == "Boxplot concepción") {
      
      datos$c4=as.numeric(as.character(datos$c4))
      
      p=datos%>%
        filter(!is.na(c4), !is.na(c5), !is.na(c7), c7!="99")%>%
        ggplot(aes(y=c4, x=reorder(`ICH[, 19]`, c4, FUN = median)))+
        geom_boxplot()+
        labs(x="ICH en intervalos", y="Edad de concepción")+
        geom_hline(aes(yintercept=20, colour="red"))
      
      ggplotly(p)
      
    } else if (input$padres == "Convivencia con los padres"){
      
      datos=datos%>%
        mutate(b33a=case_when(
          b33a=="1" ~ "No es cierto",
          b33a=="2"~"Un tanto cierto"))
      
      datos$b33a=as.factor(datos$b33a)
      
      q=datos%>%
        filter(!is.na(b33a))%>%
        ggplot()+
        geom_bar(aes((`ICH[, 19]`), fill=b33a), position= "fill")+
        labs(x="ICH en intervalos", y="Proporción", fill="Vive c/2 padres")
      
      ggplotly(q)
    }
    
    else if (input$padres == "Expectativas sobre los hijos"){
      
      datos=read.csv("Hogares3.csv", dec = ",")
      
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
      
      ICH%>%
        ggplot(aes(x=Puntaje))+
        geom_histogram(fill="white", colour="black")
      
      
      datos=cbind(datos, ICH[,19])
      
      datos$`ICH[, 19]`=discretize(datos$`ICH[, 19]`, method = "interval", breaks = 10)
      str(datos$`ICH[, 19]`)
      
      datos=datos%>%
        mutate(b31a=case_when(
          b31a=="1" ~ "Reconocida y exitosa",
          b31a=="2"~"Recursos suficientes",
          b31a=="3" ~ "Estudie lo que le guste",
          b31a=="4" ~ "Supere $ padres",
          b31a=="5" ~ "Supere $ amigos"))
      
      datos$b31a=as.factor(datos$b31a)
      
      r=datos%>%
        filter(!is.na(b31a))%>%
        ggplot()+
        geom_bar(aes((`ICH[, 19]`), fill=b31a), position= "fill")+
        labs(x="ICH en intervalos", y="Proporción", fill="Expectativas")+
        ggtitle("Barplot expectativas de los hijos")
      
      ggplotly(r)
    }
  })  
  
  
  
  
  
  
  output$barplot2 <- renderPlotly({
    
    if (input$Graficas == "Percepción estado salud") {
      datos=read.csv("Hogares3.csv", dec = ",")
      
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
      
      datos=cbind(datos, ICH[,19])
      
      datos$`ICH[, 19]`=discretize(datos$`ICH[, 19]`, method = "interval", breaks = 10)
      str(datos$`ICH[, 19]`)
      
      datos=datos%>%
        mutate(b15=case_when(
          b10=="1" ~ 1,
          b10=="2"~2,
          b10=="3"~3,
          b10=="4"~4,
          b10=="5"~5))
      
      datos$b15=as.factor(datos$b15)
      
      f=datos%>%
        filter(!is.na(b15))%>%
        group_by(`ICH[, 19]`, b15)%>%
        summarise(count=n())%>%
        ggplot(aes(`ICH[, 19]`, b15 ))+
        geom_tile(aes(fill=count))+
        labs(x="ICH en intervalos", y="Percepción estado de salud", size="Cantidad de obs")
      
      ggplotly(f)  
      
    }
    
    
    
    else if (input$Graficas == "Dificultades aprendizaje") {
      
      datos=read.csv("Hogares3.csv", dec = ",")
      
      datos=datos%>%
        mutate(b1=case_when(
          b1=="1" ~"No" ,
          b1=="2"~"Si, moderada",
          b1=="3"~"Si, severa"
        ))
      datos$b1=as.factor(datos$b1)
      
      datos=datos%>%
        mutate(b17=case_when(
          b17=="1" ~"Si" ,
          b17=="2"~"No"))
      
      
      datos$b1=as.factor(datos$b1)
      datos$b17=as.factor(datos$b17)
      
      
      g=datos%>%
        filter(!is.na(b1), !is.na(b17), b17!="ns_nc", b17!="", b1!="ns_nc", b1!="")%>%
        ggplot()+
        geom_bar(aes(b1, fill=b17), position = "fill")+
        labs(x="Dificultades en el aprendizaje", y="Proporción", fill="Asistencia psicológica/psiquiátrica")
      
      ggplotly(g)  
    }
    
    
    
    else if (input$Graficas == "Motivos hospitalización") {
      
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
      
      
      
      h=datos%>%
        filter(b18=="1", !is.na(b20))%>%
        group_by(b20)%>%
        summarise(conteo=n())%>%
        ggplot(aes(fct_reorder(b20, conteo, .desc = F), y=conteo))+
        geom_point()+
        coord_flip()+      
        labs(x="Motivos de hospitalización", y="Cantidad de niños")
      
      ggplotly(h)} 
  })    
  
  
  output$texto2= renderText({
    if (input$Graficas == "Percepción estado salud") {"El gráfico muestra que para una percepción de salud excelente (5) del hijo, la mayor cantidad de observaciones es de padres pertenecientes a hogares de alto ICH (en especial el intervalo 80-90). Tambien podemos ver que el nivel de salud percibido que mas observaciones tiene es el 4 (buena)."}
    else if(input$Graficas=="Dificultades aprendizaje"){"Este gráfico contrasta las dificultades en el aprendisaje con la asistencia psicológica/psiquiátrica. Se puede apreciar que los niños que presentan dichas dificultades tienden a concurrir a un psicólogo/psiquiatra en mayor proporción que los que no la presentan."}
    else if (input$Graficas=="Motivos hospitalización"){"El siguiente gráfico de puntos representa los motivos por los cuales los jóvenes estuvieron hospitalizados en el último año. Se puede destacar que operación es la razón más frecuente."}
    
  })
  
  
  output$texto3= renderText({
    if (input$padres == "Boxplot concepción") {"El gráfico muestra en celeste la proporción de jóvenes que repitieron al menos un año educativo y en rojo los que no; según el ICH. Podemos observar que a medida que el ICH aumenta, disminuye la proporción de los individuos que repitieron"}
    else if(input$padres =="Convivencia con los padres"){"Se puede apreciar como, a mayor ICH hay una proporción más alta de jóvenes que asisten a centros educativos; alcanzando casi el 100% los que tienen un ICH entre 90 y 100."}
    else if (input$padres =="Expectativas sobre los hijos"){"En este gráfico salta a la vista que la gran mayoría de los jóvenes entrevistados concurren a institutos públicos. Por otro lado, podemos ver que la mayor proporción de los que asisten a instituto privado se encuentra en el intervalo 90-100 del ICH."}})
  
  
  
  
  
  
  
}




shinyApp(ui, server)
