library(shiny)
library(xlsx)
library(dplyr)
library(DT)
data <- read.xlsx("C:/Users/crbdl/Desktop/banco.xlsx", sheetIndex = 1)
#data <- read.xlsx("www/banco.xlsx", sheetIndex = 1)
ui <- fluidPage(
  titlePanel("Tabsets"),
  fluidRow(column(3,
                  HTML('<center><img src="http://rusersgroup.com/img/final5x5.png" width="185"></center>'),
                  numericInput("bins","Number of bins:",min = 1,max = 50,value = 30),
                  selectInput("cat", "Seleccione la categoria", 
                              choices = unique(data$Tipo)),
                  actionButton("boton", "Filtrar"),
                  h2("h2"),h3("h3"),br(),hr(),
                  helpText("Texto ayuda"),
                  textOutput("txt"),
                  img(src = "fff.jpg", height = 450, width = 185)
  ),
  column(9,
         navbarPage("Web",
           tabPanel("Plot",plotOutput("distPlot")), 
           tabPanel("Table", DTOutput("distPlot2")),
           tabPanel("Resumen", DTOutput("distPlot3"))
         )
  )
  )
)
server <- function(input, output) {
   data_filtrada <- eventReactive(input$boton,{
    data %>% filter(Tipo==input$cat)
  })
  
  data2 <- reactive({
    paste("El numero de registros de la tabla es",dim(data_filtrada())[1])
  })
  
  output$txt <- renderText(data2())
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$distPlot2 <- renderDT({
      data_filtrada()
  })
  
  output$distPlot3 <- renderDT({
    data_filtrada() %>%
      group_by(Oficina) %>%
      summarize(N=n(), Monto=sum(as.numeric(as.character(Monto))))
  })
  
  
}
shinyApp(ui = ui, server = server)

