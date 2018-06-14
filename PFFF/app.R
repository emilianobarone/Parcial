ui <- fluidPage(
  
  titlePanel(tags$h1("SHINY APP ICH")),
  
  tabsetPanel(
    tabPanel("Barplots", 
  
fluidRow(
    column(width = 4, wellPanel(
      radioButtons("Barplots", "Tipo de Barplot",
                   c("Repitió año", "Asiste I.Educativo", "Tipo de instituto", "Convivencia con los padres", "Expectativas sobre el hijo")
      )
    )),
    column(width = 8,
           
          
          plotOutput("barplot1", height = 350
                      
                      )
           )
    )
))
)




server <- function(input, output) {
  output$barplot1 <- renderPlot({
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
    
    datos%>%
      filter(!is.na(b2))%>%
      ggplot()+
      geom_bar(aes((`ICH[, 19]`), fill=b2), position= "fill")+
      labs(x="ICH en intervalos", y="Proporción", fill="Repitió año")+
      ggtitle("Barplot desempeño educativo.")

      
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
      
      datos%>%
        filter(!is.na(b4))%>%
        ggplot()+
        geom_bar(aes((`ICH[, 19]`), fill=b4), position= "fill")+
        labs(x="ICH en intervalos", y="Proporción", fill="Asiste IE")+
        ggtitle("Barplot asistencia a centro educativo")
      
      
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
    
    datos%>%
      filter(!is.na(b6))%>%
      ggplot()+
      geom_bar(aes((`ICH[, 19]`), fill=b6), position= "fill")+
      labs(x="ICH en intervalos", y="Proporción", fill="Publico/Privado")+
      ggtitle("Barplot tipo de instituto educativo")
      
    }else if(input$Barplots == "Convivencia con los padres") {
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
      mutate(b33a=case_when(
        b33a=="1" ~ "No es cierto",
        b33a=="2"~"Un tanto cierto"))
    
    datos$b33a=as.factor(datos$b33a)
    
    datos%>%
      filter(!is.na(b33a))%>%
      ggplot()+
      geom_bar(aes((`ICH[, 19]`), fill=b33a), position= "fill")+
      labs(x="ICH en intervalos", y="Proporción", fill="Vive c/2 padres")+
      ggtitle("Barplot convivencia con los padres")
      
      
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
      
      datos%>%
        filter(!is.na(b31a))%>%
        ggplot()+
        geom_bar(aes((`ICH[, 19]`), fill=b31a), position= "fill")+
        labs(x="ICH en intervalos", y="Proporción", fill="Expectativas")+
        ggtitle("Barplot expectativas de los hijos")
    }
  })
  
}



shinyApp(ui, server)
