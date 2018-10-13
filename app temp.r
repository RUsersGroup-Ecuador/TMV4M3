library(shiny)
library(DT)
library(highcharter)
library(ggplot2)
library(dplyr)



ui <- fluidPage(
  titlePanel("Aplicación R"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.pestaña!='Tabla'",
                       sliderInput("bins",
                                   "Number of bins:",min = 1,max = 50,value = 30)
                       ),
      textInput("txt", "Ingrese texto"),
      textOutput("texto"),
      hr(), br(),
      numericInput("n", "Ingrese un número", value = 5, min = 1,
                   max = 10),
      conditionalPanel(condition = "input.txt!='no mostrar'",
                       h5("h55555"),h4("h4444"),h3("h333")),
      helpText("Aplicacion desarrollada  para ...........")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla", DTOutput("tabla")),
        tabPanel("Highcharter",highchartOutput("graf")),
        navbarMenu("More",
                   tabPanel("Sub-Component A"),
                   tabPanel("Sub-Component B")),
        tabPanel("Plot",plotOutput("distPlot")),id="pestaña"
      )
    )
  )
)
server <- function(input, output) {
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
              extensions = c('Buttons', 'Scroller'), options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
              )
    )
    
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
