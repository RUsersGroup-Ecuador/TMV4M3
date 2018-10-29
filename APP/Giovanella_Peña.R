library(shiny)
library(DT)
library(highcharter)
library(tidyverse)
library("RODBC")
library(readxl)

rsl<- read_excel("propad_qry_mst_rsl_lcl.xlsx")
colnames(rsl)<-c("Region","Provincias","genero","animales","fecha_nac","dts","peso","dgPrs")
prv_MstAlm <- select(rsl,Provincias) %>% group_by(Provincias) %>% count(Provincias) %>% arrange(Provincias)
Tabl<- select(rsl,dgPrs,Provincias)%>% group_by(dgPrs,Provincias) %>% count(dgPrs) 
Tabla_prc<-left_join(prv_MstAlm,Tabl,"Provincias")
Tabla<- Tabla_prc %>% mutate(por = round(((n.y/n.x)*100),2))
colnames(Tabla)<-c("Provincias","ttlAlmprv","dgPrs","mstAlmprv","Porcentaje")
#save(rsl,file ="DATOS.rdata" )
#load("www/DATOS.rdata")
ui <- fluidPage(
  titlePanel("Informe del muestreo de alumnos a nivel nacional"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.icono!= 'Tabla'",
                       sliderInput("range", "Range:",
                                   min = 1, max = 8882,
                                   value = c(1000,3000))),
      conditionalPanel(condition = "input.icono == 'Plot'",
                       sliderInput("num","Número de alumnos:",min = 10,max = 45,value = 20)),
      
      conditionalPanel(condition = "input.icono== 'Tabla'",
                       numericInput("n","Elija la cantidad de registros:", value = 10, min = 1, max = 28)),
      br(),
      conditionalPanel(condition = "input.icono== 'Tabla'",
                       checkboxGroupInput("variable", "Seleccione las variables:",
                                          c("Total de alumnos en cada provincia" = "ttlAlmprv",
                                            "Diagnostico de parasitosis"="dgPrs",
                                            "Total de alumnos muestreados en cada provincia" = "mstAlmprv"))
      ),
      
      hr(),
      conditionalPanel(condition = "input.icono== 'Tabla'",  
                       selectInput("prs", "Selecione el diagnostico:",
                                   choices =  rsl$dgPrs,
                                   selected = 1))
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla",div(DTOutput("data"), style="font-size:80%")
                 
        ),
        #tabPanel ("Highcharter",highchartOutput("graf")),
        navbarMenu("Highcharter",
                   tabPanel("Diagnostico de parasitosis por region ", highchartOutput("graf")),
                   #         tableOutput("tabla_diarios")),
                   tabPanel("Diagnostico de parasitosis por provincias", highchartOutput("gn"))),
        tabPanel("Plot",plotOutput("distPlot")),
        
        id="icono"
      )
    )
  )
)
server <- function(input, output) {
  output$data <- renderDT({
    datatable(Tabla[1:input$n,c("Provincias","Porcentaje", input$variable)],
              fillContainer = TRUE,
              extensions =c('Buttons', 'Scroller'),
              options = list(dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             deferRender = TRUE, scrollY = 200, scroller = TRUE,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
              ))
  })
  # output$data <-renderTable({Tabla,input$prs })
  output$graf <- renderHighchart({
    b <- rsl[c(1:input$range),]
    hchart(b, "scatter", hcaes(x = Region, y = dts, group = dgPrs)) %>%
      hc_exporting(
        enabled = TRUE
      )
    
  })
  output$gn <- renderHighchart({
    c <- rsl[c(1:input$range),]
    hchart(c, "scatter", hcaes(x = Provincias, y = peso, group = dgPrs)) %>%
      hc_exporting(
        enabled = TRUE
      )
    
    
  })
  output$distPlot <- renderPlot({
    x <- rsl$dts
    num<- seq(min(x), max(x), length.out = input$num+ 1)
    hist(x,main="Histograma de alumnos muestreados", xlab = "Peso",breaks = num, col = 'darkgray', border = 'white')
  })
  
  
}

shinyApp(ui, server)
