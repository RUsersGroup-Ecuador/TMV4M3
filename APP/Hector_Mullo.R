#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(highcharter)
library(ggplot2)
library(dplyr)
library(xts)
library(TeachingSampling)
data(Lucy)
names(Lucy)
str(Lucy)

ui <- fluidPage(
  titlePanel("Análisis de ingresos, número de empleados e impuestos
             pagados por empresas Colombianas en 2008"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.pestaña!='Tabla'",
                       sliderInput("bins",
                                   "Number of bins:",min = 1,max = 50,
                                   value = 30)
      ),
      textInput("txt", "Ingrese texto"),
      textOutput("texto"),
      hr(), br(),
      
      conditionalPanel(condition = "input.pestaña=='Data A'",
                       selectInput("servi", "Seleccione el servicio",
                                   choices = unique(data$Servicio)),
                       textOutput("nreg"),
                       dateRangeInput('date',
                                      label = 'Seleccione el período a visualizar:',
                                      #min = "2016-08-01", max = "2017-05-23",
                                      start = "2016-08-01", end = "2017-05-23",
                                      language = "es"),
                       actionButton("boton", "Actualizar")
      ),
      numericInput("n", "Ingrese un número", value = 5, min = 1,
                   max = 10),
      conditionalPanel(condition = "input.txt!='no mostrar'",
                       h5("h55555"),h4("h4444"),h3("h333")),
      helpText("Aplicacion desarrollada  para ...........")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla", 
                 div(DTOutput("tabla"),style = "font-size:60%")
        ),
        tabPanel("Highcharter",highchartOutput("graf")),
        navbarMenu("Data",
                   tabPanel("Data A", highchartOutput("diarios"),
                            tableOutput("tabla_diarios")),
                   tabPanel("Sub-Component B")),
        tabPanel("Plot",plotOutput("distPlot")),id="pestaña"
      )
    )
  )
)
server <- function(input, output) {
  
  tmp <- reactive({
    data <- data %>% filter(Servicio==input$servi)
    
    data <- data %>% filter(`Fecha de Ingreso`>=input$date[1] & `Fecha de Ingreso`<=input$date[2])
    resumen <- group_by(data, `Fecha de Ingreso`) %>% 
      summarize(Registros=n(), Cooperativa=length(unique(Cooperativa)))
    resumen
  })
  
  tmp1 <- reactive({
    dim(tmp())[1]
  })
  
  output$nreg <- renderText(tmp1())
  
  output$diarios <- renderHighchart({
    resumen <- tmp()
    xts <- xts(resumen[, -1], order.by=as.POSIXct(resumen$"Fecha de Ingreso"))
    highchart(type = "stock") %>%
      hc_title(text = "Registros diarios") %>%
      hc_subtitle(text = "Numero de registros y cooperativas") %>%
      hc_add_series_xts(xts$Registros, name = "Registros") %>%
      hc_add_series_xts(xts$Cooperativa, name = "Cooperativas")
  })
  
  output$tabla_diarios <- renderTable({
    tmp()
  })
  
  
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  output$texto <- renderText({
    paste("El texto ingresado es",input$txt)
  })
  output$tabla <- renderDT({
    datatable(mtcars[c(1:input$n),], 
              fillContainer=TRUE,
              extensions = c('Buttons', 'Scroller'), options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
              )
    )
    
    
    # div(renderDataTable(data1(),
    #                     fillContainer=TRUE,
    #                     options = list(lengthChange=TRUE, 
    #                                    scrollY = 450,
    #                                    scroller = TRUE,
    #                                    pageLength = 50),
    #                     rownames = FALSE),style = "font-size:80%")
  })
  output$graf <- renderHighchart({
    mpg <- mpg[c(1:input$bins),]
    hchart(mpg, "scatter", hcaes(x = displ, y = hwy, group = class)) %>% 
      hc_exporting(
        enabled = TRUE
      )
    
  })
  
  
  
  
}
shinyApp(ui = ui, server = server)
