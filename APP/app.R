#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#data<-read.xlsx("C:/users....")
#crear una carpeta www junto a nuestro archivo app.r ... Cuando abrimos el archivo desde el explorador
#va a setear por defecto el directorio:
#data<-read.xlsx("www/banco.xlsx",sheetIndex=1)
library(dslabs)
library(shiny)
library(dplyr)
library(DT)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  theme=shinytheme("slate"),
  #themeSelector(),  #este sirve para tener el cuadro flotante de themes
  titlePanel("Tabsets"),
  fluidRow(column(3,
                  HTML('<center><img src="http://rusersgroup.com/img/final5x5.png" height="185"></center>'),
                  numericInput("bins","Numero de Estados", min = 1,max = 51,value = 30),
                  selectInput("cat","Seleccione la Region", 
                              choices = c("Todos",as.character(unique(murders$region)))),
                  actionButton("boton","Filtrar"),
                  conditionalPanel(condition = "input.cat=='South'",
                                   h2("h2"),h3("h3"),br(),hr(),
                                   helpText("Texto ayuda"),
                                   textOutput("txt")
                  ),
                  conditionalPanel(condition="input.pestana_seleccionada=='Resumen",
                                   img(scr="http://rusersgroup.com/img/final5x5.png", height=450, width=185))
                  
                  #img(scr="unnamed.jpg", height=45, width=185)
                  #conditionalPanel(
                  #condition="input.conditionedPanels_app=='Resumen'",
                  #downloadButton('downloadData','Descargar'),br()
  ),
  column(9,
         tabsetPanel(
           tabPanel("Plot",plotOutput("distPlot")),
           tabPanel("Table",tableOutput("distPlot2")),
           tabPanel("Resumen",DTOutput("distPlot3")) ,id="pestana_seleccionada" #cuando quiero ligarle a una pestaña
         ))
  )
))           


# Define server logic required to draw a histogram
server <- shinyServer (function(input, output) {
  
  data_filtrada<-eventReactive(input$boton,{
    if(input$cat=="Todos"){murders}else{
      murders%>%filter(region==input$cat)}
  })
  
  data_filtrada2<-reactive({
    paste("El numero de registros de la tabla es", dim(data_filtrada())[1])
  })
  
  output$txt<-renderText(data_filtrada2())
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- if(input$cat=="Todos"){murders}else{
      murders%>%filter(region==input$cat)}
    x<-x[, 5] 
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$distPlot2<-renderTable({
    data_filtrada()
    #if(input$cat=="Todos"){murders}else{      #Desactivo esto para que no se filtre automaticamente y sirva el boton
    #murders%>%filter(region==input$cat)}
  })
  
  output$distPlot3<-renderDT({
    tmp<-data_filtrada() %>%     #Guardo en una temporal
      group_by(abb)%>%
      summarize(N=n(), total=sum(as.numeric(as.character(total))))
    
    datatable(
      tmp, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print', I('colvis'))
      )
    )
  })
})

# Run the application 
shinyApp(ui = ui, server = server)


