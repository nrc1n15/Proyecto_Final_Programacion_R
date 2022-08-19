
# Descargamos librerías necesarias para ejecutar nuestro código
if(!require(shiny)) install.packages("shiny")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(DescTools)) install.packages("DescTools")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")

# Sección de UI (Front-end)
ui <- fluidPage(
  
  # Sección para el díseño de nuestro proyecto
  tags$head(
    tags$style(HTML("
      .myclass pre{color: #BD0537;font-weight: bolder;font-size:16px;margin-top:40px;width:900px;}     
      .nav-tabs>li>a{color:#143289;background-color:#EDF2F9;font-weight: bolder;}
	  tfoot {display: none;}
	  .table-striped>tbody>tr:nth-child(even)>td,.table-striped>tbody>tr:nth-child(odd)>th {background-color: rgb(189, 5, 55, 0.1)}	 
	  .table-striped>tbody>tr:nth-child(odd)>td,.table-striped>tbody>tr:nth-child(odd)>th {background-color: rgb(241, 191, 0, 0.1)}	  
	  tbody td, th{text-align: center;}"
    ))
  ),
  
  headerPanel(""),
  # Pestañas
  tabsetPanel(
    tabPanel("Goles", icon = icon("signal", lib = "glyphicon"),
             fluidRow(
               titlePanel(h4(strong("Goles anotados por equipo y por temporada de La Liga"), align = "center")),
               tags$br(),
               fluidRow(
                 column(width = 6, align="center", selectInput("x", "Seleccione el equipo:", 
                                                              choices = c("Equipo de casa", "Equipo visitante"))),
                 column(width = 6, align="center", selectInput("temp", "Seleccione la temporada:", 
                                                              choices = c("Todas las temporadas",
                                                                          "T. 2010-2011", "T. 2011-2012",
                                                                          "T. 2012-2013", "T. 2013-2014",
                                                                          "T. 2014-2015", "T. 2015-2016",
                                                                          "T. 2016-2017", "T. 2017-2018",
                                                                          "T. 2018-2019", "T. 2019-2020"
                                                              )))),
               tags$br(),
               plotOutput("plot1", height = 700, width = "100%")
             ),
             
    ),
    tabPanel("Probabilidad de goles", icon = icon("stats", lib = "glyphicon") ,
             headerPanel(h4(strong("Elige una opción"))),
             sidebarPanel(selectInput("goles_input", "Opciones:", 
									  choices = c("Goles equipo local", "Goles equipo visitante",
												"Goles en conjunto")),, width=3),
             titlePanel(h4(strong("Estadísticos de La Liga"), align = "center")),
             mainPanel(
               div(align = "center", 
                   imageOutput("goles_output", height = "300px")
               ),
               div(class = "myclass",
                   verbatimTextOutput("goles_resumen")
               ),
               tags$br(),
               div(class = "myclass2",
                   HTML(paste(tags$p(style="width:950px; color:#A50430 ; font-weight:bold; font-family:verdana", "Conclusiones"),sep = "\n"),),
                   htmlOutput(style="width:950px; font-family:verdana","goles_texto")
               ),
               tags$br(),
               tags$br()
             )
    ),
    tabPanel("Partidos", icon = icon("list-alt", lib = "glyphicon"),
             fluidRow(        
               titlePanel(h4(strong("Partidos de La Liga - temporadas 2010 a 2020"), align = "center")),
               tags$br(),
               dataTableOutput ("data_table")
             )
    ), 
    tabPanel("Momios", icon = icon("piggy-bank", lib = "glyphicon") ,
             headerPanel(h4(strong("Apuestas Over/Under 2.5"))),
             sidebarPanel(
               textInput("capital", "Capital inicial:", value="50000"),
               textInput("apuesta", "Monto de la apuesta:",   value="1000"),
               selectInput("tipo", "Apostar por:", choices = c("Momios máximos", "Momios promedios")),
               , width=3),
             mainPanel(
               column(width = 12, align="right", h4(strong("Apuestas de La Liga - Partidos del 05 de Marzo de 2012 al 19 de Julio de 2020"))),
               plotOutput("momiosPlot", width = 900),
               div(class = "myclass",
                   verbatimTextOutput("resumen")
               ),
               tags$br(),
               div(class = "myclass2",
                   HTML(paste(tags$p(style="width:950px; color:#A50430 ; font-weight:bold; font-family:verdana", "Conclusiones"),
                              tags$p(style="width:950px; font-family:verdana", "*Las apuestas por promedio no parecen ser una buena estrategia. El problema puede ser que uno o dos partidos con
                                altas anotaciones pueden arruinar el promedio, aunque se tenga una muestra grande."),
                              tags$p(style="width:950px; font-family:verdana", "
                                Por ejemplo, si en una muestra de 15 partidos se han anotado 45 goles, podríamos pensar que se anotaron 3 goles por partido. 
                                ¿Pero que pasaría si dos de esos partidos terminaron en 4-2 y 5-1? Estos valores inusuales influyen en el promedio. 
                                Sin esos dos partidos su promedio se convierte en 2.53, lo cual hace que la apuesta no se vea tan atractiva.."),
                              tags$p(style="width:950px; font-family:verdana", "*Las apuestas por máximos parece ser la mejor estrategia de apuesta."),
                              tags$p(style="width:950px; font-family:verdana", "*En las apuestas por máximos o promedios, cuando se acierta por el caso que tiene menor probabilidad de suceder, la ganancia es mayor."),
                              tags$p(style="width:950px; font-family:verdana", "*Cuando la predicción del partido es empate, la apuesta es nula."),
                              sep = "")
                   )
               ),
               tags$br(),
               img(src = "Momios_maxi.PNG", height = 410, width = 650),
               tags$br(), 
			   tags$br(),
               img( src = "Momios_prom.PNG", height = 410, width = 650),
             ),
    ),
    tabPanel("Información", icon = icon("info-sign", lib = "glyphicon"),
             titlePanel(h4(strong("Proyecto Final Programación en R"), align = "center")),
             mainPanel(h4(strong("Equipo 13")),
                       div(class = "myclass2",
                           HTML(paste(tags$p(style="width:950px; color:#A50430 ; font-weight:bold; font-family:verdana", "Integrantes del equipo"),
                                      tags$p(style="width:950px; font-family:verdana", "Jordan Natanael Arenas Lobato"),
                                      tags$p(style="width:950px; font-family:verdana", "Carolina Bolayna Álvarez"),
                                      tags$p(style="width:950px; font-family:verdana", "Noé Rodríguez Cabello"),
                                      tags$p(style="width:950px; font-family:verdana", "Jacinto Daniel Rosales Ríos"),								
                                      tags$p(style="width:950px; font-family:verdana", "Perla Citlallin Gutiérrez Villanueva"),
                                      sep = "")
                           ),
                           tags$br(),
                           HTML(paste(tags$p(style="width:950px; color:#A50430 ; font-weight:bold; font-family:verdana", "Link del Dashboard"),
                                      tags$html(tags$p(style="width:950px; font-family:verdana",a(href="https://m430lj-noe-rodriguez.shinyapps.io/Proyecto_Final_R", "https://m430lj-noe-rodriguez.shinyapps.io/Proyecto_Final_R"))),
                                      sep = "")
                           ),
                           tags$br(),
                           HTML(paste(tags$p(style="width:950px; color:#A50430 ; font-weight:bold; font-family:verdana", "Versión 1.0"),
                                      sep = "")
                           )
                       ),      
             )            
    )
  )
)

# Sección de Procesaminto de Datos (Back-end)
server <- function(input, output) {
    
  ### Inicio Goles
  output$plot1 <- renderPlot({
    data <-  read.csv("match.data.csv", header = T)
    data <- mutate(data, Temp = ifelse(date >= "2010-08-28" & date<= "2011-05-21", "T. 2010-2011", 
                                       ifelse(date >= "2011-08-27" & date<= "2012-05-13", "T. 2011-2012",
                                              ifelse(date >= "2012-08-18" & date<= "2013-06-01", "T. 2012-2013",
                                                     ifelse(date >= "2013-08-17" & date<= "2014-05-18", "T. 2013-2014",
                                                            ifelse(date >= "2014-08-23" & date<= "2015-05-23", "T. 2014-2015",
                                                                   ifelse(date >= "2015-08-21" & date<= "2016-05-15", "T. 2015-2016",
                                                                          ifelse(date >= "2016-08-19" & date<= "2017-05-21", "T. 2016-2017",
                                                                                 ifelse(date >= "2017-08-18" & date<= "2018-05-20", "T. 2017-2018",
                                                                                        ifelse(date >= "2018-08-17" & date<= "2019-05-19", "T. 2018-2019",
                                                                                               ifelse(date >= "2019-08-16" & date<= "2020-07-19", "T. 2019-2020","0"
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )))))
    limy <- 95
    
    if(input$temp != "Todas las temporadas"){
      data <- data %>% filter(Temp == input$temp)
      limy <- 15
    }
    
    team <- ifelse(input$x == "Equipo de casa", "home.team", "away.team")
    x <- data[,ifelse(input$x == "Equipo de casa", "home.score", "away.score")]
    col <- ifelse(input$x == "Equipo de casa", "home.score", "away.score")
    t <- unique(data[, team])
    equipos <- c()
    golesTotales <- c()
    
    for (i in t) {
      goles <- 0
      c <- which(data[, team] == i)
      for(ab in c){
        goles <- goles + data[ab, col]
      }
      equipos <- append(equipos, i)
      label <- paste(i, ": ", goles, " goles",  sep="")
      golesTotales <- append(golesTotales, label)
    }
    
    names(golesTotales) <- equipos
    
    goles_labeller <- function(variable,value){
      return(golesTotales[value])
    }
    
    data %>% ggplot(aes(x)) + 
      geom_bar(fill="#BD0537", color="navy", size=0.05, alpha=0.7) +
      geom_text(stat='count', aes(label=..count..), vjust=-1) + 
      facet_wrap(team,  labeller=goles_labeller) +
      labs(x = "Número de goles", y = "Frecuencia") + 
      ylim(0,limy) + 
      theme(strip.text.x = element_text(size = 13)) + 
      theme(strip.background =element_rect(fill="#E9ECF2", color="navy")) + 
      theme(strip.text = element_text(colour = 'navy'))
  })
  ### Fin Goles

  ### Inicio Probabilidad de Goles
  liga.espanola <- lapply(dir(pattern = "ESP*"), read.csv)
  liga.espanola.temporadas <- lapply(liga.espanola, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  for(i in 1:length(liga.espanola.temporadas)){
    liga.espanola.temporadas[[i]] <- mutate(liga.espanola.temporadas[[i]], Date = as.Date(Date, "%d/%m/%Y"))
  }
  liga.espanola.temporadas <- do.call(rbind,liga.espanola.temporadas)
  liga.espanola.temporadas$Date <- gsub("00","20", liga.espanola.temporadas$Date)
  liga.espanola <- liga.espanola.temporadas
  rm(liga.espanola.temporadas)
  
  output$goles_resumen <- renderText({
    medidas.tc <- "Sin asignar"
    texto_salida_goles <- if(input$goles_input == "Goles equipo local") { 
      medidas.tc <- liga.espanola$FTHG
    }
    else if(input$goles_input == "Goles equipo visitante") {
      medidas.tc <- liga.espanola$FTAG
    }
    else if(input$goles_input == "Goles en conjunto") { 
    }
    
    if(input$goles_input != "Goles en conjunto") {
      promedio <- paste("Promedio: ", round(mean(medidas.tc)*(length(medidas.tc))/(length(medidas.tc)-1),3), "gol(es)") 
      mediana <- paste("Mediana: ", median(medidas.tc), "gol(es)") 
      moda <- paste("Moda: ", Mode(medidas.tc)[1], "gol(es), frecuencia", length(which(medidas.tc == Mode(medidas.tc)))) 
      minimo <- paste("Valor Minimo: ", min(medidas.tc), "gol(es)")
      maximo <- paste("Valor Máximo: ", max(medidas.tc), "gol(es)")
      desv_est <- paste("Desviación Estándar: ", round(sd(medidas.tc)*(length(medidas.tc))/(length(medidas.tc)-1),3), "gol(es)") 
      paste(promedio,mediana, moda, minimo, maximo, desv_est, sep="\n")
    }
    
  })
  
  output$goles_texto <- renderUI({
    
    texto_salida <- if(input$goles_input == "Goles equipo local") { 
      texto1 <- paste("En este caso nuestra población son todas las temporadas de la liga española (La Liga), de los cuales
  							tomamos los datos correspondientes a las temporadas 2017-18, 2018-19 y 2019-20 para analizarlos.")
      texto2 <- paste("* Encontramos que lo más probable es que cuando un equipo juega como local anote un gol, lo cual 
							                   se presentó en 373 partidos (apróximadamente la tercera parte de los juegos), mientras que es 
											   muy poco probable que anote más de 5 goles como observamos en la gráfica de barras.")
      texto3 <- paste("* En esta muestra también encontramos que la cantidad mínima de goles fue de 0 mientras que la mayor 
							                  cantidad de goles que un equipo local anotó en estas temporadas fue de 8 goles.")
    }
    else if(input$goles_input == "Goles equipo visitante") {
      texto1 <- paste("En este caso nuestra población son todas las temporadas de la liga española (La Liga), de los cuales
  							tomamos los datos correspondientes a las temporadas 2017-18, 2018-19 y 2019-20 para analizarlos.")
      texto2 <- paste("* Encontramos que lo más probable es que cuando un equipo juega como visitante no anote goles, lo cual
                                             se presentó en 401 partidos (un poco más de la tercera parte de los juegos), aunque también existe 
											 una alta probabilidad que anote un gol, estos dos valores representan más de dos terceras partes de 
											 los resultados posibles. De igual manera, es muy poco probable que un equipo que juega como visitante 
											 anote más de 5 goles como apreciamos en el gráfico de barras.")
      texto3 <- paste("* En esta muestra también encontramos que la cantidad mínima de goles fue de 0 mientras que la mayor 
							                 cantidad de goles que un equipo visitante anotó en estas temporadas fue de 6 goles.")
    }
    else if(input$goles_input == "Goles en conjunto") {  
      texto1 <- paste("En este caso nuestra población son todas las temporadas de la liga española (La Liga), de los cuales
  							tomamos los datos correspondientes a las temporadas 2017-18, 2018-19 y 2019-20 para analizarlos.")
      texto2 <- paste("* En este gráfico observamos la probabilidad de los goles anotados en los partidos
                                             de manera conjunta. Podemos ver que en el 75% de los partidos se obtuvieron resultados en los que 
											 ambos equipos tuvieron un marcador de 0 a 2 goles, mientras que no se presentaron partidos en los 
											 que ambos equipos anotaran de 6 a 8 goles.")
      texto3 <- paste("")
    }
    
    HTML(paste(texto1,texto2,texto3, sep="<br/><br/>"))
    
  })
  
  output$goles_output <- renderImage({
    if(input$goles_input == "Goles equipo local") { list(src = './www/Goles_equipo_local.png')}
    else if(input$goles_input == "Goles equipo visitante") { list(src = './www/Goles_equipo_visitante.png')}
    else if(input$goles_input == "Goles en conjunto") { list(src = './www/Goles_conjunto.png')}
  }, deleteFile = FALSE)
  ### Fin Probabilidad de Goles 

  ### Inicio Partidos
  partidos_resultados <- read.csv("match.data.csv")
  
  colnames(partidos_resultados) = c('Fecha', 'Equipo local', 'Goles local', 'Equipo visitante',  'Goles visitante')
  
  output$data_table <- renderDataTable({ partidos_resultados}, 
                                       options = list(lengthMenu = c(10, 50, 100), 
                                                      pageLength = 10)
  )
  ### Fin Partidos
  
  ### Inicio Momios
  getCapital <- reactive(
    input$capital
  )
  
  getApuesta <- reactive(
    input$apuesta
  )
  
  predicciones <- read.csv("predicciones.csv")
  phs <- predicciones$phs
  pas <- predicciones$pas
  momio <- read.csv("momio.csv")
  hs <- momio$home.score
  as <- momio$away.score
  
  output$momiosPlot <- renderPlot({
    
    cap <- as.numeric(getCapital()); g <- NULL; capInicial <- cap;
    apuesta <- as.numeric(getApuesta())
    
    if(input$tipo ==  "Momios máximos"){
      
      for(j in 1:length(phs)){
        if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){
          if((hs[j] + as[j]) > 2.5) cap <- cap + apuesta*(momio$Max.2.5.O[j]-1)
          else cap <- cap - apuesta
          g <- c(g, cap)
        }
        
        if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){
          if((hs[j] + as[j]) < 2.5) cap <- cap + apuesta*(momio$Max.2.5.U[j]-1)
          else cap <- cap - apuesta
          g <- c(g, cap)
        } 
      }
    }
    
    else if(input$tipo ==  "Momios promedios"){
      
      for(j in 1:length(phs)){
        if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
          if((hs[j] + as[j]) > 2.5) cap <- cap + apuesta*(momio$Avg.2.5.O[j]-1)
          else cap <- cap - apuesta
          g <- c(g, cap)
        }
        
        if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){
          if((hs[j] + as[j]) < 2.5) cap <- cap + apuesta*(momio$Avg.2.5.U[j]-1)
          else cap <- cap - apuesta
          g <- c(g, cap)
        }
      }
    } 
    
    g <- data.frame(Num_Ap = 1:length(g), Capital = g)
    title <- paste(input$tipo, "     Capital: $", format(capInicial, big.mark=","), "     Apuesta: $", format(as.numeric(input$apuesta), big.mark=","), sep="")
    
	ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
      labs(x = "Número de apuesta", 
           y = "Capital",
           title = title) +
      theme(plot.title = element_text(size=14, hjust = 0.5, color="#BD0537", face = "bold"))  +
      theme(axis.text.x = element_text(face = "bold", color="#BD0537" , size = 12, angle = 25, hjust = 1),
            axis.text.y = element_text(face = "bold", color="#BD0537" , size = 12, angle = 25, hjust = 1)) +
      theme(text = element_text(size=13))
    
  })
  
  output$resumen <- renderText({
    
	cap <- as.numeric(getCapital()); g <- NULL; capInicial <- cap;
    apuesta <- as.numeric(getApuesta())
    nulas <- 0 
    jugadas <- 0
    
    if(input$tipo ==  "Momios máximos"){
      
      for(j in 1:length(phs)){
        if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){
          jugadas <- jugadas + 1
          if((hs[j] + as[j]) > 2.5) cap <- cap + apuesta*(momio$Max.2.5.O[j]-1)
          else cap <- cap - apuesta
        }
        
        if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){
          jugadas <- jugadas + 1
          if((hs[j] + as[j]) < 2.5) cap <- cap + apuesta*(momio$Max.2.5.U[j]-1)
          else cap <- cap - apuesta
        }
        
        if(((phs[j] + pas[j]) <= 3 & (phs[j] + pas[j]) >= 2.1)
           | ((phs[j] + pas[j]) > 3) & !(0.64/(momio$Max.2.5.O[j]^-1) > 1)
           | ((phs[j] + pas[j]) < 2.1) & !(0.58/(momio$Max.2.5.U[j]^-1) > 1)
        )
          nulas <- nulas + 1    
      }
    }
    
    else if(input$tipo ==  "Momios promedios"){
      
      for(j in 1:length(phs)){
        if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
          jugadas <- jugadas + 1
          if((hs[j] + as[j]) > 2.5) cap <- cap + apuesta*(momio$Avg.2.5.O[j]-1)
          else cap <- cap - apuesta
        }
        
        if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){
          jugadas <- jugadas + 1
          if((hs[j] + as[j]) < 2.5) cap <- cap + apuesta*(momio$Avg.2.5.U[j]-1)
          else cap <- cap - apuesta
        }
        
        if(((phs[j] + pas[j]) <= 3 & (phs[j] + pas[j]) >= 2.1)
           | ((phs[j] + pas[j]) > 3) & !(0.64/(momio$Avg.2.5.O[j]^-1) > 1)
           | ((phs[j] + pas[j]) < 2.1) & !(0.58/(momio$Avg.2.5.U[j]^-1) > 1)
        )
          nulas <- nulas + 1     
      }
    }  
    
    capActual <- paste( "Capital final: $" , format(round(cap,2),  big.mark=","), sep="")

    if (cap > capInicial) {
      text <- "Ganancia: $"
    } else {
      text <- "Pérdida: $"
    }
    
    resultado <- paste(text, format(round(cap - capInicial,2),  big.mark=","), sep="")
    partidos <- paste("Número de partidos: ", dim(momio)[1], sep="") 
    jugados <- paste("Apuestas jugadas: ", jugadas, sep="")
    nulos <- paste("Apuestas nulas: ", nulas, sep="")
    paste(resultado, capActual, partidos, jugados, nulos,  sep="\n")
  })
  ### Fin Momios
}

# Paso de parámetros para ejecución de la aplicación 
shinyApp(ui = ui, server = server)
