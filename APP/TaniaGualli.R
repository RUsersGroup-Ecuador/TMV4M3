#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(DT)

#data_estudiantes <- read.csv("E:/Tania/Cursos/R - RUGE/Proyecto Final/00 Data/student_mat.csv", stringsAsFactors= FALSE)
data_estudiantes <- read.csv("www/student_mat.csv", stringsAsFactors= FALSE)
data_estudiantes=data_estudiantes %>% filter( G3!=0 ) #eilimino notas 0

#data_estudiantes <- tbl_df(data_estudiantes)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  navbarPage(
    title = HTML('<center><b>ANÁLISIS DE RENDIMIENTO ACADÉMICO DE LA MATERIA DE MATEMÁTICAS</b></center>'),
    tabPanel('Inicio', column(4,
                              HTML('<center><img src="http://rusersgroup.com/img/final5x5.png"></center>')),
                        column(6,
                               HTML('<br><br><br>'),
                               HTML('<P ALIGN="justify"> El presente trabajo consiste en el análisis descriptivo y predictivo de una base de datos de un grupo de estudiantes en su clase de Matemática.'),
                               HTML('<br>'),
                               HTML('<P ALIGN="justify"> Se requiere predecir la calificación final, que está representada por la variable “G3”.'),
                               HTML('<br>'),
                               HTML('<P ALIGN="justify"> Se cuenta con varios atributos económicos y sociales que se usarán como predictores.')
                               )),
    tabPanel('Datos Descriptivos',
               column(4,
                      tabsetPanel(
                        tabPanel("Gráfico - Resumen Nota Final Matemáticas", plotOutput("distPlot5")))),
                      column(6,
                      HTML('<P ALIGN="justify"> INFORME'),
                      HTML('<br>'),
                      HTML('<P ALIGN="justify"> Este análisis se realiza sobre una base que recoge la tasa de rendimiento de un examen rendido por 395 estudiantes.  El examen fue evaluado sobre un total de 20 puntos.'),
                      HTML('<br>'),
                      HTML('<P ALIGN="justify">Se detectaron 38 estudiantes con calificación de 0, que correspondió a aquellos que no se presentaron al examen, razón por la que se les asignó esa calificación. Se eliminó este grupo,luego de lo cual se realizó el análisis correspondiente y se obtuvieron los resultados que se indican a continuación:'),
                      HTML('<br>'),
                      HTML('<P ALIGN="justify">La tasa de rendimiento promedio en la materia de Matemática es de 11.52; y en promedio, la tasa de rendimiento de los estudiantes difieren frente a la media en 3.23 puntos, lo cual nos da un indicio de que existe una considerable variabilidad entre cada estudiante.
                           El estudiante con la tasa más alta de rendimiento es de 20,0, mientras que el estudiante con la tasa más baja es de 4.0. La diferencia entre la tasa más baja y más alta es 16 puntos, lo cual confirma que existe una alta heterogeneidad entre los estudiantes y que las tasas varían mucho de un estudiante a otro. 
                           El 25% de los estudiantes tienen una tasa de hasta 9 puntos (Q1), el 50% de los estudiantes, tienen una tasa de rendimiento de hasta 11 puntos (mediana); mientras que el 75% de los estudiantes tienen una tasa de rendimiento de hasta 14 puntos. '),
                      HTML('<br>'),
                      HTML('<P ALIGN="justify">Apenas un 25% del total de estudiantes aprueba el examen.'),
                      HTML('<br>'),
                      HTML('<P ALIGN="justify">En el siguiente estudio se identifican los factores socioeconómicos que afectan al rendimiento académicos de estos estudiantes.')
                           )
                      ),
    tabPanel('Dashboard',
             fluidRow(
               column(5,
                      HTML('<br><br>'),
                      HTML('<b>Gráfico - Nota Final Matemáticas por Sexo</b>'),
                      HTML('<br><br><br>'),
                      selectInput("sexo", "Seleccione sexo", choices = unique(data_estudiantes$sex)),
                      plotOutput("distPlot")
               ),
               column(5,
                        HTML('<br><br>'),
                        HTML('<b>Tabla - Detalle por Colegio</b>'),
                        HTML('<br><br><br>'),
                        selectInput("colegio", "Seleccione colegio", choices = unique(data_estudiantes$school)),
                        DTOutput("distPlot2"))
               )
             ),
    tabPanel('Modelo Predictivo', plotOutput("distPlot6"))
  ),
  

  HTML('<br><br><br>'),
  HTML('<br><br><br>'),
  HTML('<br><br><br>'),
  HTML('<br><br><br>'),
  HTML('<br><br><br>'),
  HTML('<br><br><br>'),
  HTML('<footer> Desarrollado por Tania Gualli </footer>')
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # filtra cada vez que selecciono de la lista desplegable
  #  data_filtrada <- reactive({
  #    data_estudiantes %>% filter(school==input$colegio)})
  
  #para que filtre cuando hace clic en el botón
  #output$txt1 <- "Texto simple"
  data_filtrada <- eventReactive(input$boton,{
    data_estudiantes %>% filter(school==input$colegio)})
  
  
  data2 <- reactive({
    dim(data_filtrada())
  })
  
  output$txt <- renderText(data2())
  
  output$distPlot <- renderPlot({
    x <- data_estudiantes %>% filter(sex==input$sexo)
    hist(x$G3)
    
  })
  output$distPlot2 <- renderDT({
    data_estudiantes %>% filter(school==input$colegio)
  })
  
  #puedo cambiar esto por una función reactiva
  #  output$distPlot3 <- renderDT({
  #    data_estudiantes %>% filter(school==input$colegio)
  #    group_by(sex) %>%
  #      sumarize (N=n(), max_nota_final=max(G3))
  #  })
  
  output$distPlot3 <- renderDT({
    data_filtrada() %>% 
      group_by(sex) %>%
      summarize(N=n(), max_nota_final=max(G3)) #n, funciona con el group by, cuenta por la categoría
  })
  
  #
  output$distPlot3 <- renderDT({
    tmp <- data_filtrada() %>% 
      group_by(sex) %>%
      summarize(N=n(), max_nota_final=max(G3)) #n, funciona con el group by, cuenta por la categoría
    
    datatable(
      tmp, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  })

  output$distPlot5 <- renderPlot({
    ggplot(data_estudiantes,aes(sex,G3)) + 
      geom_boxplot(aes(fill=sex))
  })
  

#MODELO PREDICTIVO
  set.seed(101)
  sample <- sample.int(n = nrow(data_estudiantes), size = floor(.75*nrow(data_estudiantes)), replace = F)
  train <- data_estudiantes[sample, ]
  test  <- data_estudiantes[-sample, ]
  
  str(train)
  str(test)
  
  #l.	Utilizando la base de datos train, se estima un modelo de regresión múltiple para 
  #explicar G3 (Nota Final). Se verifica que el modelo propuesto tenga coeficientes estadísticamente 
  #significativos de forma individual (prueba t) y global (prueba F).
  
  lm.fit=lm(G3~., data=train)
  summary(lm.fit)
  
  #las variables 
  lm.fit=lm(G3~failures+goout+schoolsup, data=train)
  summary(lm.fit)
  
  #n.	Utilizando el modelo de regresión lineal múltiple se realiza 
  #una predicción utilizando la base de datos test.
  
  output$distPlot6 <- renderPlot({
    hist(predict(lm.fit , data.frame(test)))
  })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
