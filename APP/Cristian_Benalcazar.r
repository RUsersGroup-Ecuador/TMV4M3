easypackages::libraries(c("shiny","shinythemes","rjson", "rCharts", "highcharter", "DT", "rpivotTable", "dplyr", "xlsx", "scales", "xts", 
                          "reshape2", "httr", "devtools", "ggplot2", "quantmod", "RPostgreSQL", "DBI", "ineq", "anytime", "gsubfn"))

################ VARIABLES ##################
{
  var <- c("EMPRESAS","PROCEDIMIENTOS","MONTO ADJUDICADO (en millones)")
  var_procesos <- c("PROCESOS","MONTO")
  var_procesos_tipo <- c("EXTREMO", "ALTO", "MEDIO", "BAJO", "INSIGNIFICANTE")
  riesgo <- c("TODOS", "EXTREMO", "ALTO", "MEDIO", "BAJO","INSIGNIFICANTE")
  GRUPO <- c("ENTIDADES","PROVEEDORES PUBLICOS","PROVEEDORES PRIVADOS")
  GRUPO_PROCESOS <- c("RESUMEN","RANKING", "ENTIDADES", "TABLA DINÁMICA")
  consolidado <- c("Publicados","Ofertas")
  ZONAS <- c("GLOBAL",
             "Zona 1: Esmeraldas, Imbabura, Carchi, Sucumbios.",
             "Zona 2: Pichincha (excepto Quito), Napo, Orellana.",
             "Zona 3: Cotopaxi, Tungurahua, Chimborazo, Pastaza.",
             "Zona 4: Manabi, Santo Domingo de los Tsachilas.",
             "Zona 5: Santa Elena, Guayas (sin Zona 8), Bolivar, Los Rios y Galapagos.",
             'Zona 6: Canar, Azuay, Morona Santiago.',
             "Zona 7: El Oro, Loja, Zamora Chinchipe.",
             "Zona 8: Guayaquil, Samborondon y Duran.",
             "Zona 9: Distrito Metropolitano de Quito.")
  
  SELEC_VAR <- "Seleccione la variable a visualizar"
  SELEC_RIES <- "Seleccione el nivel de riesgo"
  m <- format(seq(as.Date("2016-01-01"),as.Date("2018-01-01"),by="months"), "%Y-%B")
}

############# AGREGAR MES ADICIONAL #########################
MESES <- c(paste(c("ENERO","FEBRERO","MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO","SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"), "2016"),
           paste(c("ENERO", "FEBRERO", "MARZO"), "2017"))



###########  CARGAR MES ADICIONAL ##################
dir <- ifelse(Sys.info()[[1]]=="Windows","C:/CRISBEN/APPS/MATRIZ/DATA-DASH/", "/mnt/hgfs/CRISBEN/APPS/MATRIZ/DATA-DASH/")
RDatas <- list.files(paste0(dir, "RDATA/"))
for ( i in seq(1:length(RDatas))){
        load(paste0(dir,"RDATA/", RDatas[i]))
}
rm(i, RDatas)
#infos <- apropos("INFO_")
INFO <- list(INFO_ENERO_2016, INFO_FEBRERO_2016, INFO_MARZO_2016, INFO_ABRIL_2016, INFO_MAYO_2016, INFO_JUNIO_2016, INFO_JULIO_2016, INFO_AGOSTO_2016,
             INFO_SEPTIEMBRE_2016, INFO_OCTUBRE_2016, INFO_NOVIEMBRE_2016, INFO_DICIEMBRE_2016, INFO_ENERO_2017, INFO_FEBRERO_2017, INFO_MARZO_2017)
rm(INFO_ENERO_2016, INFO_FEBRERO_2016, INFO_MARZO_2016, INFO_ABRIL_2016, INFO_MAYO_2016, INFO_JUNIO_2016, INFO_JULIO_2016, INFO_AGOSTO_2016,
   INFO_SEPTIEMBRE_2016, INFO_OCTUBRE_2016, INFO_NOVIEMBRE_2016, INFO_DICIEMBRE_2016, INFO_ENERO_2017, INFO_FEBRERO_2017, INFO_MARZO_2017)

#CARGA CATALOGO CPC RIESGOSOS
cpcs_riesgo <- read.xlsx(paste0(dir, "ANEXO_CODIGOS_CPC.xlsx"),header=T,sheetIndex = 1)


#cpcs_riesgo <- read.xlsx("/riesgos/DATA-DASH/ANEXO_CODIGOS_CPC.xlsx")
cpcs_riesgo$cpc_n9 <- as.numeric(as.character(cpcs_riesgo$cpc_n9))

######### ui.R ##################
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  #shinythemes::themeSelector(),
  tabsetPanel(
    tabPanel("Matriz de riesgos",
             fluidRow(column(2,
                             wellPanel(
                               #img(src = "r.png", height = 45, width = 185),
                               HTML('<center><img src="r.png" width="185"></center>'),
                               h4("R USERS GROUP - ECUADOR", align = "center"),
                               h5("APP SHINY - DEMO", align = "center"),
                               h6("Condicionales y automatización", align = "center"),
                               h6("Comparativo Mensual", align = "center"),
                               br(),
                               selectInput("mes", "Seleccione el mes", choices = MESES, selected = "MARZO 2017"),
                               radioButtons("grupo", "Seleccione el grupo", choices = GRUPO),
                               
                               conditionalPanel(condition = "input.conditionedPanels_matriz == 'Evolución Global' | input.conditionedPanels_matriz == 'Evolución (nuevos mes a mes)'",
                                                br(),selectInput("var_evo", SELEC_VAR, choices = var))
                               
                             )),
                      column(10,
                             navbarPage("",collapsible=T,
                                        tabPanel("Resumen",
                                                 selectInput("var", SELEC_VAR, choices = var, selected = "MONTO ADJUDICADO (en millones)"),
                                                 chartOutput("graf_resumen", "highcharts"),
                                                 div(shiny::dataTableOutput("resumen"), style = "font-size:80%")
                                        ),
                                        tabPanel("Componentes principales",
                                                 helpText("Análisis de componentes principales"),
                                                 plotOutput("graf_comp_prin"),
                                                 textOutput("ncomp"),
                                                 div(shiny::dataTableOutput("comp_prin"), style = "font-size:80%")
                                        )
                                        ,
                                        tabPanel("Ranking",
                                                 fluidRow(
                                                   column(4, selectInput("riesgo", SELEC_RIES, choices = riesgo)), br(),
                                                   column(4,downloadButton('downloadData', 'Descargar'))
                                                 ),
                                                 div(dataTableOutput("ranking"), style = "font-size:65%")
                                        )
                                        ,
                                        tabPanel("Evolución Global",
                                                 
                                                 highchartOutput("graf_evo_global"),
                                                 div(dataTableOutput("evo"), style = "font-size:80%")
                                        ),
                                        tabPanel("Evolución (nuevos mes a mes)",
                                                 #selectInput("var_NUE_evo", SELEC_VAR, choices = var),
                                                 chartOutput("NUE_evo_graf", "highcharts"),
                                                 div(dataTableOutput("NUE_evo"), style = "font-size:80%")
                                        )
                                        ,
                                        navbarMenu("Riesgo medio",
                                                   tabPanel("Evolución",
                                                            chartOutput("riesgo_medio_graf", "highcharts"),
                                                            div(dataTableOutput("riesgo_medio"), style = "font-size:80%")
                                                   ),
                                                   tabPanel("Variación riesgo medio",
                                                            chartOutput("variacion_medio_graf", "highcharts"),
                                                            div(dataTableOutput("variacion_medio"), style = "font-size:80%")
                                                   )
                                        )
                                        
                                        # ,
                                        # tabPanel("Metodología",
                                        #          htmlOutput('pdfviewer_met_riesgo'))
                                        ,
                                        id="conditionedPanels_matriz"
                             )
                      )
             )
    ),
    
    tabPanel("Riesgo por procesos",
             
             fluidRow(column(2,wellPanel(
                     #img(src = "1.png", height = 45, width = 155),
                     HTML('<center><img src="r.png" width="185"></center>'),
                     h4("R USERS GROUP - ECUADOR", align = "center"),
                     h5("APP SHINY - DEMO", align = "center"),
                     h6("Condicionales y automatización", align = "center"),
                     h6("Comparativo Diario, 2016", align = "center"),br(),
                                         helpText("A partir del 27-12-2016 se incluye al análisis de texto de preguntas como componente en el nivel de riesgo por procedimiento"),br(),
                                         
                                         conditionalPanel(condition = "input.conditionedPanels != 'Perfil'",
                                                          dateInput('date',
                                                                    label = 'Seleccione una fecha',
                                                                    value = Sys.Date()-1,
                                                                    min = "2016-09-26", max = Sys.Date(),language = "es"
                                                          )),
                                         conditionalPanel(condition = "input.conditionedPanels == 'Publicados' | input.conditionedPanels == 'Con ofertas'",
                                                          selectInput("zona_procesos", "Seleccionar Zona", choices = ZONAS),
                                                          conditionalPanel(condition = "input.zona_procesos != 'GLOBAL'",
                                                                           helpText("El nivel de riesgos de procedimientos por zonas muestra los procedimientos rescalados por su correspondiente zona.")
                                                          ),
                                                          radioButtons("grupo_procesos", "", choices = GRUPO_PROCESOS)),
                                         conditionalPanel(condition = "input.conditionedPanels == 'Perfil'",
                                                          radioButtons("tipo_proceso", "", choices =c("CON OFERTAS","PUBLICADOS"))
                                         ),
                                         conditionalPanel(condition = "input.conditionedPanels == 'Preguntas publicados' | input.conditionedPanels == 'Preguntas ofertas'",
                                                          radioButtons("grupo_preguntas", "", choices = c("PREGUNTAS", "RESULTADOS"))
                                         )
                                         
             )
             ),
             column(10,
                    navbarPage("",collapsible=T,
                               tabPanel("Publicados",
                                        fluidPage(
                                          fluidRow(column(4,
                                                          conditionalPanel(
                                                            condition = "input.grupo_procesos=='RESUMEN'",
                                                            selectInput('agregado', 'Seleccione un resumen a visualizar', choices = c("NIVEL DE RIESGO"#, "NIVEL DE RIESGO POR TIPO"
                                                            ), selected = "NIVEL DE RIESGO")
                                                          )
                                          ),
                                          column(4,
                                                 conditionalPanel(
                                                   condition = "input.grupo_procesos=='RESUMEN' & input.agregado=='NIVEL DE RIESGO'",
                                                   selectInput("var_procesos", "Seleccione un variable a visualizar", choices = var_procesos, selected = "PROCESOS")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.grupo_procesos=='RESUMEN' & input.agregado=='NIVEL DE RIESGO POR TIPO'",
                                                   selectInput('var_procesos_tipo', 'Seleccione un nivel de riesgo a visualizar', choices = var_procesos_tipo, selected = "EXTREMO")
                                                 )
                                          )
                                          ),
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='RESUMEN' & input.agregado=='NIVEL DE RIESGO'",
                                            chartOutput("graf_resumen_procesos", "highcharts")
                                          ),
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='RESUMEN' & input.agregado=='NIVEL DE RIESGO POR TIPO'",
                                            chartOutput("graf_resumen_procesos_tipos", "highcharts")
                                          ),
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='RESUMEN'",
                                            div(dataTableOutput("riesgo_proceso_resumen"), style = "font-size:80%")
                                          ),
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='RANKING'",
                                            downloadButton('downloadData_proceso', 'Descargar'), br(), br(),
                                            div(dataTableOutput("riesgo_proceso"), style = "font-size:80%")
                                          ),
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='ENTIDADES'",
                                            downloadButton('downloadData_proceso_entidades', 'Descargar'), br(), br(),
                                            div(dataTableOutput("entidades_procesos"), style = "font-size:80%")
                                          )
                                          ,
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='TABLA DINÁMICA'",br(),
                                            div(rpivotTableOutput("pivot_publicados"))
                                          )
                                          # ,
                                          # conditionalPanel(
                                          #   condition = "input.grupo_procesos=='EVOLUCION'",
                                          #   highchartOutput("pub_diarias")
                                          # )
                                        )
                               ),
                               tabPanel("Con ofertas",
                                        fluidPage(
                                          
                                          #fluidRow(
                                          column(4,
                                                 conditionalPanel(
                                                   condition = "input.grupo_procesos=='RESUMEN'",
                                                   selectInput('agregado1', 'Seleccione un resumen a visualizar', choices = c("NIVEL DE RIESGO"#, "NIVEL DE RIESGO POR TIPO"
                                                   ))
                                                 )
                                          ),
                                          column(4,
                                                 conditionalPanel(
                                                   condition = "input.grupo_procesos=='RESUMEN' & input.agregado1=='NIVEL DE RIESGO'",
                                                   selectInput("var_procesos_ofertas", "Seleccione un variable a visualizar", choices = var_procesos)
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.grupo_procesos=='RESUMEN' & input.agregado1=='NIVEL DE RIESGO POR TIPO'",
                                                   selectInput('var_procesos_ofertas_tipo', 'Seleccione un nivel de riesgo a visualizar', choices = var_procesos_tipo)
                                                 )
                                                 #)
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='RESUMEN' & input.agregado1=='NIVEL DE RIESGO'",
                                            chartOutput("graf_resumen_procesos_ofertas", "highcharts")
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='RESUMEN' & input.agregado1=='NIVEL DE RIESGO POR TIPO'",
                                            chartOutput("graf_resumen_procesos_ofertas_tipos", "highcharts")
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='RESUMEN'",
                                            div(dataTableOutput("riesgo_proceso_ofertas_resumen"), style = "font-size:80%")
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='RANKING'",
                                            downloadButton('downloadData_proceso_ofertas', 'Descargar'), br(), br(),
                                            div(dataTableOutput("riesgo_proceso_ofertas"), style = "font-size:80%")
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='ENTIDADES'",
                                            downloadButton('downloadData_proceso_entidades_ofertas', 'Descargar'), br(), br(),
                                            div(dataTableOutput("entidades_procesos_ofertas"), style = "font-size:80%")
                                          )
                                          ,
                                          conditionalPanel(
                                            condition = "input.grupo_procesos=='TABLA DINÁMICA'",br(),
                                            div(rpivotTableOutput("pivot_ofertas"))
                                          )
                                        )
                               )
                               ,
                               tabPanel("Consolidado",
                                        fluidPage(
                                                downloadButton('downloadData_riesgo_proceso_consolidado', 'Descargar'), br(), br(),
                                                div(dataTableOutput("riesgo_proceso_consolidado"), style = "font-size:90%")
                                                )
                                        )
                               ,
                               
                               tabPanel("Preguntas publicados",
                                        fluidPage(
                                          fluidRow(column(12,
                                                          conditionalPanel(condition = "input.grupo_preguntas=='PREGUNTAS' & input.date>=('2016-09-19')",
                                                                           plotOutput("graf_pregun_cate"),br(),br(),
                                                                           downloadButton('downloadData_preguntas', 'Descargar'),
                                                                           div(dataTableOutput("pregunta_categoria"), style = "font-size:80%")),
                                                          conditionalPanel(condition = "input.grupo_preguntas=='RESULTADOS'  & input.date>=('2016-09-19')",
                                                                           downloadButton('downloadData_preguntas_id_soli', 'Descargar'),br(),br(),
                                                                           div(dataTableOutput("categoria_id_soli"),style = "font-size:80%"))
                                          )
                                          )
                                          
                                        )
                                        
                               )
                               ,
                               tabPanel("Preguntas ofertas",
                                        fluidPage(
                                          fluidRow(column(12,
                                                          conditionalPanel(condition = "input.grupo_preguntas=='PREGUNTAS' & input.date>=('2016-09-19')",
                                                                           plotOutput("graf_pregun_cate_oferta"),br(),br(),
                                                                           downloadButton('downloadData_preguntas_ofertas', 'Descargar'),
                                                                           div(dataTableOutput("pregunta_categoria_oferta"), style = "font-size:80%")),
                                                          conditionalPanel(condition = "input.grupo_preguntas=='RESULTADOS' & input.date>=('2016-09-19')",
                                                                           downloadButton('downloadData_preguntas_id_soli_oferta', 'Descargar'),br(),br(),
                                                                           div(dataTableOutput("categoria_id_soli_ofertas"),
                                                                               style = "font-size:80%"))
                                          )
                                          )
                                          
                                        )
                               ),
                               tabPanel("Publicados - CPC",
                                        fluidPage(
                                          column(4,
                                                 conditionalPanel(
                                                   condition = "input.date>=as.Date('2016-09-19')",
                                                   downloadButton('downloadData_proceso_cpc_publicados', 'Descargar'), br(), br(),
                                                   div(dataTableOutput("cpc_publicados"), style = "font-size:80%")
                                                 )
                                          )
                                        )
                               ),
                               tabPanel("Ofertas - CPC",
                                        fluidPage(
                                          column(4,
                                                 conditionalPanel(
                                                   condition = "input.date>=as.Date('2016-09-19')",
                                                   downloadButton('downloadData_proceso_cpc_ofertas', 'Descargar'), br(), br(),
                                                   div(dataTableOutput("cpc_ofertas"), style = "font-size:80%")
                                                 )
                                          )
                                        )
                               ),id = "conditionedPanels" 
                               ,
                               tabPanel("Metodología",
                                        htmlOutput('pdfviewer_doc')
                               )
                               
                    )
             ))
             
    )
  )
)

########### server. R #######################
myoptions <- list(searching = F, searchable=F, paging = F, autoWidth=T)
myoptions1 <- list(searching = T, searchable=T, paging = T, autoWidth=T)
color1 <- "lightblue"
color2 <- "steelblue"

server <- function(input, output,session) {

        output$pdfviewer_doc <- renderText({
                return(paste('<iframe style="height:600px; width:100%" src="', 
                             "Manual.pdf", '"></iframe>', sep = ""))
        })

        
  ####### REACTIVOS GLOBALES ########
    ### AÑADAIR CODIGO PARA MES ADICIONAL
    mesInput <- reactive({
      switch(input$mes,
             "ENERO 2016" = 1,
             "FEBRERO 2016" = 2,
             "MARZO 2016" = 3,
             "ABRIL 2016" = 4,
             "MAYO 2016" = 5,
             "JUNIO 2016" = 6,
             "JULIO 2016" = 7, 
             "AGOSTO 2016" = 8,
             "SEPTIEMBRE 2016"=9,
             "OCTUBRE 2016"=10,
             "NOVIEMBRE 2016"=11,
             "DICIEMBRE 2016"=12,
             "ENERO 2017"=13,
             "FEBRERO 2017"=14,
             "MARZO 2017"=15,
             "ABRIL 2017"=16,
             "MAYO 2017"=17)
    })
    
    grupoInput <- reactive({
      switch(input$grupo,
             "ENTIDADES" = 1,
             "PROVEEDORES PUBLICOS" = 3,
             "PROVEEDORES PRIVADOS" = 2)
    })
    
    zonaInput <- reactive({
      ifelse(mesInput()<50,10,
             switch(input$zona,
                    "GLOBAL"=10,
                    "Zona 1: Esmeraldas, Imbabura, Carchi, Sucumbios."=1,
                    "Zona 2: Pichincha (excepto Quito), Napo, Orellana."=2,
                    "Zona 3: Cotopaxi, Tungurahua, Chimborazo, Pastaza."=3,
                    "Zona 4: Manabi, Santo Domingo de los Tsachilas."=4,
                    "Zona 5: Santa Elena, Guayas (sin Zona 8), Bolivar, Los Rios y Galapagos."=5,
                    'Zona 6: Canar, Azuay, Morona Santiago.'=6,
                    "Zona 7: El Oro, Loja, Zamora Chinchipe."=7,
                    "Zona 8: Guayaquil, Samborondon y Duran."=8,
                    "Zona 9: Distrito Metropolitano de Quito."=9)
      )
    })
    
    zona_procesos_Input <- reactive({
      switch(input$zona_procesos,
             "GLOBAL"=10,
             "Zona 1: Esmeraldas, Imbabura, Carchi, Sucumbios."=1,
             "Zona 2: Pichincha (excepto Quito), Napo, Orellana."=2,
             "Zona 3: Cotopaxi, Tungurahua, Chimborazo, Pastaza."=3,
             "Zona 4: Manabi, Santo Domingo de los Tsachilas."=4,
             "Zona 5: Santa Elena, Guayas (sin Zona 8), Bolivar, Los Rios y Galapagos."=5,
             'Zona 6: Canar, Azuay, Morona Santiago.'=6,
             "Zona 7: El Oro, Loja, Zamora Chinchipe."=7,
             "Zona 8: Guayaquil, Samborondon y Duran."=8,
             "Zona 9: Distrito Metropolitano de Quito."=9)
    })
    
    #VARIABLES PARA RESUMEN
    
    varInput <- reactive({
      switch(input$var,
             "MONTO ADJUDICADO (en millones)" = "Monto (en Millones)",
             "EMPRESAS" = "Numero de empresas",
             "PROCEDIMIENTOS" = "PROCEDIMIENTOS")
    })
    
    var_procesosInput <- reactive({
      switch(input$var_procesos,
             "PROCESOS" = "Procesos",
             "MONTO" = "Monto")
    })
    
    var_procesos_ofertasInput <- reactive({
      switch(input$var_procesos_ofertas,
             "PROCESOS" = "Procesos",
             "MONTO" = "Monto")
    })
    
    var_procesos_tipoInput <- reactive({
      switch(input$var_procesos_tipo,
             "EXTREMO" = "Extremo",
             "ALTO" = "Alto",
             "MEDIO" = "Medio",
             "BAJO" = "Bajo",
             "INSIGNIFICANTE" = "Insignificante")
    })
    
    var_procesos_ofertas_tipoInput <- reactive({
      switch(input$var_procesos_ofertas_tipo,
             "EXTREMO" = "Extremo",
             "ALTO" = "Alto",
             "MEDIO" = "Medio",
             "BAJO" = "Bajo",
             "INSIGNIFICANTE" = "Insignificante")
    })
    
    # VARIABLES PARA EVOLUCION 
    varInput_evo <- reactive({
      switch(input$var_evo,
             "MONTO ADJUDICADO (en millones)" = "Monto (en Millones)",
             "EMPRESAS" = "Numero de empresas",
             "PROCEDIMIENTOS" = "PROCEDIMIENTOS")
    })
    
    # VARIABLES PARA NIVEL DE RIESGO
    riesgoInput <- reactive({
      switch(input$riesgo,
             "TODOS" = "TODOS",
             "EXTREMO" = "1. Riesgo Extremo",
             "ALTO" = "2. Riesgo Alto",
             "MEDIO" = "3. Riesgo Medio",
             "BAJO" = "4. Riesgo Bajo",
             "INSIGNIFICANTE" = "5. Riesgo Insignificante")
    })
    
    # GRUPOS PROCESOS
    grupo_procesosInput <- reactive({
      switch (input$grupo_procesos,
              "RESUMEN" = 1,
              "RANKING" = 2,
              "EVOLUCION" = 3)
    })
    consolidadoInput <- reactive({
      switch (input$consolidado,
              "Publicados" = 1,
              "Ofertas" = 2)
    })
    
    # FUNCIONES GLOBALES
    graf_evo <- function (datos, tipo, titulo, variable, evo=F) {
      a <- Highcharts$new()
      a$chart(type = "spline", backgroundColor = NULL)
      
      #a$chart(type = "column", backgroundColor = NULL)
      if(tipo==3){
        a$series(name= "Riesgo Extremo", data=as.numeric(datos[1, c(2:dim(datos)[2])]), dashStyle = "shortdot")
        a$series(name= "Riesgo Alto", data=as.numeric(datos[1, c(2:dim(datos)[2])]), dashStyle = "shortdot")
        a$series(name= "Riesgo Medio", data=as.numeric(datos[2, c(2:dim(datos)[2])]), dashStyle = "shortdot")
        a$series(name= "Riesgo Bajo", data=as.numeric(datos[3, c(2:dim(datos)[2])]), dashStyle = "shortdot")
        a$series(name= "Riesgo Insignficante", data=as.numeric(datos[4, c(2:dim(datos)[2])]), dashStyle = "shortdot")
        a$series(name= "TOTAL", data=as.numeric(datos[5, c(2:dim(datos)[2])]), dashStyle = "Solid")
      } else {
        a$series(name= "Riesgo Extremo", data=as.numeric(datos[1, c(2:dim(datos)[2])]), dashStyle = "shortdot")
        a$series(name= "Riesgo Alto", data=as.numeric(datos[2, c(2:dim(datos)[2])]), dashStyle = "shortdot")
        a$series(name= "Riesgo Medio", data=as.numeric(datos[3, c(2:dim(datos)[2])]), dashStyle = "shortdot")
        a$series(name= "Riesgo Bajo", data=as.numeric(datos[4, c(2:dim(datos)[2])]), dashStyle = "shortdot")
        a$series(name= "Riesgo Insignficante", data=as.numeric(datos[5, c(2:dim(datos)[2])]), dashStyle = "shortdot")
        a$series(name= "TOTAL", data=as.numeric(datos[6, c(2:dim(datos)[2])]), dashStyle = "Solid")
      }
      a$xAxis(categories = colnames(datos[1,c(2:dim(datos)[2])]))
      a$tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                shared = TRUE, borderWidth = 5)
      a$exporting(enabled = TRUE) # enable exporting option
      
      a$legend(symbolWidth = 80)
      a$set(height = 400, width=1050)
      if(evo==F) {
        tx <- '- Nuevos - mes a mes'
      } 
      else if (evo=="VAR") {
        tx <- 'Variación de riesgo medio'
        variable <- ""
      } else {
        tx <- 'Nivel de riesgo medio'
        variable <- ""
      }
      
      if(titulo==1) a$title(text = paste('Entidades -', variable, tx))
      if(titulo==3) a$title(text = paste('Proveedores públicos -', variable, tx))
      if(titulo==2) a$title(text = paste('Proveedores privados -', variable, tx))
      a
    }
    
    descarga_ex <- function(datos, file, tipo){        
      wb <- createWorkbook(type="xlsx")
      
      # Define some cell styles
      # Title and sub title styles
      TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, isBold=TRUE)
      
      SUB_TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=12,
                                              isItalic=TRUE, isBold=FALSE)
      
      # Styles for the data table row/column names
      TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
      
      TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
        Alignment(vertical="VERTICAL_CENTER",wrapText=TRUE, horizontal="ALIGN_CENTER") +
        Border(color="black", position=c("TOP", "BOTTOM"), 
               pen=c("BORDER_THICK", "BORDER_THICK"))+Fill(foregroundColor = "lightblue", pattern = "SOLID_FOREGROUND")
      
      sheet <- createSheet(wb, sheetName = "Ranking")
      
      # Helper function to add titles
      xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
        rows <- createRow(sheet, rowIndex=rowIndex)
        sheetTitle <- createCell(rows, colIndex=1)
        setCellValue(sheetTitle[[1,1]], title)
        setCellStyle(sheetTitle[[1,1]], titleStyle)
      }
      
      # Add title and sub title into a worksheet
      xlsx.addTitle(sheet, rowIndex=4, 
                    title=paste("Fecha:", format(Sys.Date(), format="%Y/%m/%d")),
                    titleStyle = SUB_TITLE_STYLE)
      
      xlsx.addTitle(sheet, rowIndex=5, 
                    title="Elaborado por: R Users Group - Ecuador",
                    titleStyle = SUB_TITLE_STYLE)
      
      # Add title
      if(tipo==1){
        xlsx.addTitle(sheet, rowIndex=7, 
                      paste("MATRIZ DE PERFIL DE RIESGOS - ENTIDADES CONTRATANTES -", input$mes),
                      titleStyle = TITLE_STYLE)
        
        # Add a table into a worksheet
        addDataFrame(datos[,-c(which(colnames(datos)=="CATEGORÍA"))],
                     sheet, startRow=9, startColumn=1, 
                     colnamesStyle = TABLE_COLNAMES_STYLE,
                     rownamesStyle = TABLE_ROWNAMES_STYLE,
                     row.names = FALSE)
      } else {
        
        if(tipo==3){
          xlsx.addTitle(sheet, rowIndex=7, 
                        paste("MATRIZ DE PERFIL DE RIESGOS - PROVEEDORES PÚBLICOS -", input$mes),
                        titleStyle = TITLE_STYLE)
          
        } else {
          
          if(tipo==4){
            xlsx.addTitle(sheet, rowIndex=7, 
                          paste("RIESGO POR PROCESOS - ", input$date),
                          titleStyle = TITLE_STYLE)
            
          } else {
            
            if(tipo==5){
              xlsx.addTitle(sheet, rowIndex=7, 
                            paste("RIESGO POR PROCESOS CON OFERTAS - ", input$date),
                            titleStyle = TITLE_STYLE)
            } else {
              if(tipo==6){
                xlsx.addTitle(sheet, rowIndex=7, 
                              paste("RIESGO POR PROCESO - CONSOLIDADO"),
                              titleStyle = TITLE_STYLE)
              } else {
                if(tipo==7){
                  xlsx.addTitle(sheet, rowIndex=7, 
                                paste("RIESGO POR PROCESO MUNICIPIO DE QUITO", input$fecha_MQ),
                                titleStyle = TITLE_STYLE)
                } else {
                  xlsx.addTitle(sheet, rowIndex=7,
                                paste("MATRIZ DE PERFIL DE RIESGOS - PROVEEDORES PRIVADOS -", input$mes),
                                titleStyle = TITLE_STYLE)
                }
              }
            }
          }
        }
        # Add a table into a worksheet
        addDataFrame(datos,
                     sheet, startRow=9, startColumn=1,
                     colnamesStyle = TABLE_COLNAMES_STYLE,
                     rownamesStyle = TABLE_ROWNAMES_STYLE,
                     row.names = FALSE)
      }
      
      # Change column width
      setColumnWidth(sheet, colIndex=c(1:ncol(datos)), colWidth=20)
      
      # image
      addPicture(paste0(dir, "r.png"), sheet, scale=0.28, startRow = 1, startColumn = 1)
      
      # Save the workbook to a file...
      saveWorkbook(wb, file)
    }
    
    resumen <- function(datos){
      colnames(datos) <-  c("Nivel de Riesgo", m[1:(dim(datos)[2]-1)])
      a <- t(data.frame("TOTAL"=colSums(datos[,-1], na.rm = T)))
      datos <- rbind(datos, datos[1,])
      datos[dim(datos)[1],1] <- "TOTAL"
      datos[dim(datos)[1],c(2:dim(datos)[2])] <- a
      colnames(datos) <-  c("Nivel de Riesgo", m[1:(dim(datos)[2]-1)])
      datos}
    
    resumen_nuevos <- function(datos){
      datos <- datos[-dim(datos)[1],]
      b <- list(0)
      for( i in c(dim(datos)[2]:2)){
        ifelse(i==2,
               b[[i-1]] <- datos[,i]-datos[,i],
               b[[i-1]] <- datos[,i]-datos[, c(i-1)])
      }
      bb <- do.call(cbind, b)
      datos[,-1] <- bb
      a <- t(data.frame("TOTAL"=colSums(datos[,-1])))
      datos <- rbind(datos, datos[1,])
      datos[dim(datos)[1],1] <- "TOTAL"
      datos[dim(datos)[1],c(2:dim(datos)[2])] <- a
      colnames(datos) <-  c("Nivel de Riesgo", m[1:(dim(datos)[2]-1)])
      datos}

    pie <- function (datos, v, y, titulo) {
      n1 <- Highcharts$new()
      n1 <- hPlot(v, y, data = datos, type = "pie", options3d = list(enabled = TRUE, alpha = 70, beta = 0))
      
      n1$addParams(height = 400, width = 1050)
      n1$exporting(sourceWidth = 1000, sourceHeight = 400)
      if(titulo==1) n1$title(text = paste("Resumen Entidades - ", y," - ", input$mes))
      if(titulo==3) n1$title(text = paste("Resumen Proveedores Públicos - ", y," - ", input$mes))
      if(titulo==2) n1$title(text = paste("Resumen Proveedores Privados - ", y," - ", input$mes))
      if(titulo==4) n1$title(text = "")
      n1
    }
    
    variacion_relativa <- function(datos){
      variacion <- list(0)
      for( i in c(dim(datos)[2]:2)){
        ifelse(i==2,
               variacion[[i-1]] <- datos[,i]-datos[,i],
               variacion[[i-1]] <- (datos[,i]-datos[, c(i-1)])/datos[, c(i-1)])
        variacion[[i-1]][is.infinite(variacion[[i-1]])]<-0
        variacion[[i-1]][is.nan(variacion[[i-1]])]<-0
      }
      variacion[[i-1]][(variacion[[i-1]])] <- 0
      bb <- do.call(cbind, variacion)
      datos[,-1] <- bb
      datos[,-2]
    }
    
    graf_evo_global <- function(datos, variable, titulo) {
      a <- unlist(c(datos[1]))
      datos <- as.data.frame(t(datos[,c(2:dim(datos)[2])]))
      colnames(datos) <- a
      Mes <- rownames(datos)
      datos <- cbind(Mes, datos)
      
        if(titulo==1) y <-  "- Entidades -"
        if(titulo==2) y <-  "- Proveedores privados -"
        if(titulo==3) y <-  "- Proveedores públicos -"
        
        hc <- highchart() %>%
          hc_add_theme(hc_theme_smpl()) %>%
          hc_title(text = paste("Evolución Global", y, variable)) %>%
          hc_xAxis(categories = datos[,1]) %>%
          hc_yAxis(title = list(text = variable),align = "left")  %>%
          hc_add_series(name = "Extremo", data = datos$'1. Riesgo Extremo') %>%
          hc_add_series(name = "Alto", data = datos$'2. Riesgo Alto') %>%
          hc_add_series(name = "Medio", data = datos$'3. Riesgo Medio') %>%
          hc_add_series(name = "Bajo", data = datos$'4. Riesgo Bajo') %>%
          hc_add_series(name = "Insignificante", data = datos$'5. Riesgo Insignificante') %>%
          hc_chart(type = "column",
                   options3d = list(enabled = F, beta = 15, alpha = 15)) %>%
          hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                     shared = TRUE, borderWidth = 5) %>%
          hc_exporting(enabled = TRUE) # enable exporting option
     hc}
    
  
  ### fecha para calendario de riesgo por proceso ###
  fechaCons <- reactive(as.character(gsub("-", "",input$dateCons)))
  fecha <- reactive(as.character(gsub("-", "",input$date)))
  fecha_esp <- reactive(as.character(gsub("-", "",input$date_esp)))
  
  formato_resumen <- function(data){
    nom_colum <- colnames(data)
    nom_fil <- data[,1]
    data <- sapply(data[,c(2:7)],as.numeric)
    rownames(data) <- nom_fil
    data <- cbind(sapply(data[,c(1)],comma_format()),
                  sapply(data[,c(2)], percent_format()),
                  sapply(data[,c(3)],comma_format()),
                  sapply(data[,c(4)], percent_format()),
                  sapply(data[,c(5)],dollar_format()),
                  sapply(data[,c(6)], percent_format()))
    colnames(data) <- nom_colum[2:7]
    as.data.frame(data)
  }
  
  
  #################### OUTPUTS ################
  {
    
    point <- format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)
    
    # RESUMEN
    output$resumen <- shiny::renderDataTable({
      a <- mesInput()
      x <- grupoInput()
      xx <- zonaInput()
      #formato_resumen(INFO[[a]][[x]][[xx]]$resumen)}, options = myoptions)
      INFO[[a]][[x]][[xx]]$resumen$`PROCEDIMIENTOS`<- point(as.numeric(INFO[[a]][[x]][[xx]]$resumen$`PROCEDIMIENTOS`))
      
      INFO[[a]][[x]][[xx]]$resumen$`Monto (en Millones)`<- sapply(as.numeric(INFO[[a]][[x]][[xx]]$resumen$`Monto (en Millones)`),dollar_format())
      INFO[[a]][[x]][[xx]]$resumen[[3]]<- sapply(as.numeric(INFO[[a]][[x]][[xx]]$resumen[[3]])/100,percent_format())
      INFO[[a]][[x]][[xx]]$resumen[[5]]<- sapply(as.numeric(INFO[[a]][[x]][[xx]]$resumen[[5]])/100,percent_format())
      INFO[[a]][[x]][[xx]]$resumen[[7]]<- sapply(as.numeric(INFO[[a]][[x]][[xx]]$resumen[[7]])/100,percent_format())
      
      INFO[[a]][[x]][[xx]]$resumen}, options = myoptions)
    
    output$graf_resumen <- renderChart2({
      a <- mesInput()
      x <- grupoInput()
      xx <- zonaInput()
      datos <- INFO[[a]][[x]][[xx]]$tabla1
      pie(datos=datos,v="Nivel de Riesgo",y=varInput(), titulo=x)
    })
    
    # COMPONENTES PRINCIPALES
    output$comp_prin <- shiny::renderDataTable({
      a <- mesInput()
      x <- grupoInput()
      xx <- zonaInput()
      INFO[[a]][[x]][[xx]]$importance}, options = myoptions)
    
    output$graf_comp_prin <- renderPlot({
      a <- mesInput()
      x <- grupoInput()
      xx <- zonaInput()
      par(mfrow=c(1,2))
      screeplot(INFO[[a]][[x]][[xx]]$acp1,main = paste("Varianza capturada por las componentes", input$grupo, input$mes), col=color1)
      abline(1,0)
      screeplot(INFO[[a]][[x]][[xx]]$acp1,type="lines",main = paste("Gráfico de Sedimentación", input$grupo, input$mes), col=color2)
    })
    
    output$ncomp <- renderText({
      a <- mesInput()
      x <- grupoInput()
      xx <- zonaInput()
      paste("- Se debe retener ", sum((INFO[[a]][[x]][[xx]]$acp1$sdev)^2 >1), "componentes, los cuales representan un (",
            round(as.numeric(summary(INFO[[a]][[x]][[xx]]$acp1)$importance[3, sum((INFO[[a]][[x]][[xx]]$acp1$sdev)^2 >1)])*100,2),
            "%) de la informacion.")
    })
    
    # RANKING
    output$ranking <- renderDataTable({
      a <- mesInput()
      x <- grupoInput()
      xx <- zonaInput()
      #INFO[[a]][[x]][[xx]]$ranking$`MONTO ADJUDICADO` <- sapply(INFO[[a]][[x]][[xx]]$ranking$`MONTO ADJUDICADO`,dollar_format())
      datos <- INFO[[a]][[x]][[xx]]$ranking
      n_col <- dim(datos)[2]
      var_cam <- riesgoInput()
      if(x==3 & var_cam=="1. Riesgo Extremo") datos_f <- matrix("NO HAY EXTREMOS",1,1)
      if (x==1) {
        ifelse(var_cam=="TODOS",
               # datos_f <- datos[, c(1:4,6,7,n_col-2, n_col-1, n_col)],
               # datos_f <- datos[, c(1:4,6,7,n_col-2, n_col-1, n_col)] %>% filter (datos$'NIVEL DE RIESGO'==var_cam)
               datos_f <- datos[, -c(5)],
               datos_f <- datos[, -c(5)] %>% filter (datos$'NIVEL DE RIESGO'==var_cam)
        )
      }
      else {
        ifelse(var_cam=="TODOS",
               # datos_f <- datos[, c(1:4,9,10,n_col-2, n_col-1, n_col)],
               # datos_f <- datos[, c(1:4,9,10,n_col-2, n_col-1, n_col)] %>% filter (datos$'NIVEL DE RIESGO'==var_cam)
               datos_f <- datos[, -c(5)],
               datos_f <- datos[, -c(5)] %>% filter (datos$'NIVEL DE RIESGO'==var_cam)
        )
      }
      datatable(datos_f, filter = 'top', options = list(
        pageLength = 10, autoWidth = TRUE)) %>% formatCurrency(c('MONTO ADJUDICADO'))
    }, options = myoptions
    )
    
  
  
    global<-reactive({
      archivos_ofertas<-dir(paste0(dir, "PROCESOS/OFERTAS/"))
      n<-grep("20170125",archivos_ofertas)
      
      
      lista_ofertas<-list()
      
      for( i in 1:n){
        
        lista_ofertas[[i]]<-read.csv(paste(dir, "PROCESOS/OFERTAS/",archivos_ofertas[i],sep=""),row.names=NULL,stringsAsFactors = FALSE,
                                     colClasses = c(entidadruc="character",proveedorruc="character" ))
        
        
      }
      
      for( i in (n+1):(length(archivos_ofertas)-1)){
        if(dim(read.csv(paste(dir,"PROCESOS/OFERTAS/",archivos_ofertas[i],sep=""),row.names=NULL,sep="|",quote = "",stringsAsFactors = FALSE))[2] == 25){
          
          lista_ofertas[[i]]<-read.csv(paste(dir, "PROCESOS/OFERTAS/",archivos_ofertas[i],sep=""),row.names=NULL,sep="|",quote = "",stringsAsFactors = FALSE,
                                       colClasses = c(entidadruc="character",proveedorruc="character" ))
        }
        if(dim(read.csv(paste(dir, "PROCESOS/OFERTAS/",archivos_ofertas[i],sep=""),row.names=NULL,sep="|",quote = "",stringsAsFactors = FALSE))[2] != 25){
          lista_ofertas[[i]]<-read.csv(paste(dir, "PROCESOS/OFERTAS/",archivos_ofertas[i],sep=""),row.names=NULL,sep="|",stringsAsFactors = FALSE,
                                       colClasses = c(entidadruc="character",proveedorruc="character" ))
        }
      }
      
      
      for (i in 1:length(lista_ofertas)){
        lista_ofertas[[i]]$codigo_mes<-i
      }
      
      ante<-do.call(rbind,lista_ofertas)
      
      ante
    })
    
    
    proc<-reactive(as.character(input$proceso))
    #proc<-function() {"RE-PU-HCAM-2017-033"}     proc()
    
    ranking_perfil_proceso_ofertas <- reactive({
      entidades<-0
      lista_proc<-dir(paste(dir, "automaticos/riesgo_proceso_automatico/"))
      
      for(i in length(lista_proc):1){
        load(paste(dir, "automaticos/riesgo_proceso_automatico/",lista_proc[i],sep=""))
        entidades<-lista[[2]][[1]][[2]]
        if(length(which(entidades$"Código del proceso" == proc()))>0){
          break 
          return(entidades)
        }
        
      }
      
      # proc<-function(){"SIE-EMGIRS-1-2017"}
      
      n<-nrow(entidades)
      
      entidades<-arrange(entidades,desc(entidades$"Presupuesto"))
      entidades$ORDEN_PRESUPUESTO<-c(1:n)
      
      entidades<-arrange(entidades,desc(entidades$"Puntaje de riesgo"))
      entidades$ORDEN_PUNTAJE_RIESGO<-c(1:n)
      
      entidades<-arrange(entidades,desc(entidades$"RANKING"))
      m<-dim(entidades)[2]
      # which(colnames(entidades) %in% "Peso")
      columnas<-grep("Peso",colnames(entidades))
      for (i in 1:length(columnas)){
        entidades<-arrange(entidades,desc(entidades[,columnas[i]]))
        entidades[,m+i]<-c(1:n)
        colnames(entidades)[m+i]<-paste("ORDEN_",colnames(entidades)[columnas[i]],sep="")
        entidades<-arrange(entidades,desc(entidades$"RANKING"))
      }    
      
      entidades
      #ranking_perfil_proceso_ofertas <- function() {entidades}
    })
    


    
    ############### GLOBAL PUBLICADOS ##############################
    
    global_publicados<-reactive({
      archivos_publicados<-dir(paste0(dir, "PROCESOS/PUBLICADOS/"))
      n<-grep("20170125",archivos_publicados)
      
      lista_publicados<-list()
      
      for( i in 1:n){
        
        lista_publicados[[i]]<-read.csv(paste(dir, "PROCESOS/PUBLICADOS/",archivos_publicados[i],sep=""),row.names=NULL,stringsAsFactors = FALSE,
                                        colClasses = c(entidadruc="character"))
        
        
      }
      
      for( i in (n+1):(length(archivos_publicados)-1)){
        if(dim(read.csv(paste(dir, "PROCESOS/PUBLICADOS/",archivos_publicados[i],sep=""),row.names=NULL,sep="|",quote = "",stringsAsFactors = FALSE))[2] == 25){
          
          
          lista_publicados[[i]]<-read.csv(paste(dir, "PROCESOS/PUBLICADOS/",archivos_publicados[i],sep=""),row.names=NULL,sep="|",quote = "",stringsAsFactors = FALSE,
                                          colClasses = c(entidadruc="character"))
        }
        if(dim(read.csv(paste(dir, "PROCESOS/PUBLICADOS/",archivos_publicados[i],sep=""),row.names=NULL,sep="|",quote = "",stringsAsFactors = FALSE))[2] != 25){
          lista_publicados[[i]]<-read.csv(paste(dir,"PROCESOS/PUBLICADOS/",archivos_publicados[i],sep=""),row.names=NULL,sep="|",stringsAsFactors = FALSE,
                                          colClasses = c(entidadruc="character"))
        }
      }
      
      
      for (i in 1:length(lista_publicados)){
        lista_publicados[[i]]$codigo_mes<-i
      }
      
      ante<-do.call(rbind,lista_publicados)

      ante
    })
    
    ranking_perfil_proceso_publicados <- reactive({
      
      entidades<-0
      lista_proc<-dir(paste(dir, "automaticos/riesgo_proceso_automatico/"))
      
      for(i in length(lista_proc):1){
        load(paste(dir, "automaticos/riesgo_proceso_automatico/",lista_proc[i],sep=""))
        entidades<-lista[[1]][[1]][[2]]
        if(length(which(entidades[,3] == proc_pub()))>0){
          break 
          return(entidades)
        }
        
      }

      n<-nrow(entidades)
      
      entidades<-arrange(entidades,entidades$"Presupuesto")
      entidades$ORDEN_PRESUPUESTO<-c(1:n)
      
      entidades<-arrange(entidades,entidades$"RANKING")
      entidades<-arrange(entidades,entidades$"Puntaje de riesgo")
      entidades$ORDEN_PUNTAJE_RIESGO<-c(1:n)
      
      entidades<-arrange(entidades,desc(entidades$"RANKING"))
      m<-dim(entidades)[2]
      # which(colnames(entidades) %in% "Peso")
      columnas<-grep("Peso",colnames(entidades))
      for (i in 1:length(columnas)){
        entidades<-arrange(entidades,desc(entidades[,columnas[i]]))
        entidades[,m+i]<-c(1:n)
        colnames(entidades)[m+i]<-paste("ORDEN_",colnames(entidades)[columnas[i]],sep="")
        entidades<-arrange(entidades,desc(entidades$"RANKING"))
      }    
      
      entidades
    })
    
    proc_pub<-reactive(as.character(input$proceso_pub))
   
    output$deutptalle_proc_publicados <- shiny::renderTable({
      datos<-ranking_perfil_proceso_publicados()
      detalle<-datos %>% filter (datos[,3]==proc_pub())
      detalle$Presupuesto <- dollar(detalle$Presupuesto)
      
      #### ESTADO ACTUAL SOCE #####
      db <- dbConnect(drv=dbDriver("PostgreSQL"), dbname = "mic", host = "192.168.1.50",
                      port = 5432, user = "servidor_riesgos",
                      password = "sintiempo")
      sql <- paste0("SELECT id_soli_compra, codigo,   c.razon_social razon_social, c.cedula ruc, d.detalle_id tipo_proceso, desc_compra, e.detalle_id estado, cast(b.fech_hora as varchar) fecha_publicacion, presupuesto from tcom_solicitud_compra a INNER JOIN tcom_fecha b using (id_soli_compra) INNER JOIN tgen_persona c on c.persona_id = a.persona_id inner join tgen_det_cat d on d.seq_det_cat = a.seq_tipo_solicitud inner join tgen_det_cat e on e.seq_det_cat = a.esta_solicitud  where b.seq_det_cat = 388 and codigo =?id1;")
      query <- sqlInterpolate(db, sql, id1=detalle$"Código del proceso")
      datos <- dbGetQuery(db, query)
      dbDisconnect(db)
      
      detalle$"Fecha de publicación" <- datos$fecha_publicacion
      detalle$"Estado actual SOCE" <- datos$estado
      detalle <- t(detalle[, c(3:6,9:10,dim(detalle)[2]-1,dim(detalle)[2])])
      ####
      
      #detalle <- t(detalle[, c(3:6,9:10)])
    }, rownames = T, colnames=F)
    
    
    
    global_pub<-reactive({
      global<-data.frame(global_publicados())
      # global<-ante
      
      entidad<-global[which(global$codigo==proc_pub()),c(7,6)]
      # entidad<-global[which(global$id_soli_compra=="1303350"),c(7,16)]
      entidad
    })
    
    
    output$perfil_proceso_publicados_2<-renderDataTable({
      data<-global_pub()
      
    })
    
    
    
    ranking_perfil_entidad_pub<- reactive({
      entidad<-global_pub()
      info_buscado<-0
      necesario<-list()
      for ( i in length(INFO)){
        info_buscado<-INFO[[i]][[1]][[10]]$ranking[which(INFO[[i]][[1]][[10]]$ranking$RUC %in% entidad[,2]),]
        necesario[[1]]<-info_buscado
        necesario[[2]]<-i
        if(nrow(info_buscado)==1){
          break
          return(necesario)
        }
      }
      datos <- INFO[[necesario[[2]]]][[1]][[10]]$ranking
      columnas_orden_entidades(datos)
    })
    
  
    
   
    #perfil procedimiento entidad
    
    global_pub_ofertas<-reactive({
      global<-data.frame(global())
      # global<-ante
      
      entidad<-global[which(global$codigo==proc()),c(7,6)]
      # entidad<-global[which(global$id_soli_compra=="1303350"),c(7,16)]
      entidad
    })
    
    
    archivo <- reactive(
      ifelse(grupoInput()==2,'.txt','.xlsx')
    )
    

    output$downloadData <- downloadHandler(
      #filename = function() { paste("Perfiles de Riesgo", '.txt') },
      filename = function() { paste("Perfiles de Riesgo", archivo()) },
      content = function(file) {
        a <- mesInput()
        x <- grupoInput()
        xx <- zonaInput()
        datos <- INFO[[a]][[x]][[xx]]$ranking
        var_cam <- riesgoInput()
        
        ifelse(var_cam=="TODOS",
               datos_f <- datos,
               datos_f <- datos %>% filter (datos$'NIVEL DE RIESGO'==var_cam)
        )
        if (x==2) {
          write.csv2(datos_f, file)
        }
        else {
          descarga_ex(datos_f, file, tipo=x)}
      }
    )
    
    
    datos_agru <- function(data,val){
      ifelse(dim(data)[1]==4,
             {
               data1 <- tbl_df(0)
               data1[1] <- "1. Riesgo Extremo"
               data1[2] <- val
               colnames(data1) <- names(data)
               data1[2:5,] <- data
             }, {data1 = data})
      data1
    }
    
    
    evolucion_global <- reactive({ 
            x <- grupoInput() #x<- 1
            xx <- zonaInput() #xx<-10
            variable <- varInput_evo() #variable <- "PROCEDIMIENTOS"
            datos1 <- list(0)
            datos <- list(0)
            mn <- which(MESES==input$mes) #mn<-4
            
            if(mn==1) {
                    for (i in c(1:2)) {
                            datos1[[i]] <- INFO[[i]][[x]][[xx]]$tabla1[, c("Nivel de Riesgo", variable)]
                            datos[[i]] <- merge(datos1[[1]], datos1[[i]], by="Nivel de Riesgo", all.x=T)
                            
                    }
                    f <- datos[[2]]
            }
            if(mn>1) {
                    for (i in c(1:mn)) {
                            datos1[[i]] <- INFO[[i]][[x]][[xx]]$tabla1[, c("Nivel de Riesgo", variable)]
                            datos[[1]] <- datos1[[1]]
                            if(i>1) {
                                    datos[[i]] <- merge(datos[[i-1]], datos1[[i]], by="Nivel de Riesgo", all.x=T)
                                    }
                    }
                    f <- datos[[mn]]
            }
            resumen(f)
            
    })
    
    
    riesgo_medio <- reactive({
            x <- grupoInput() #x<- 1
            xx <- zonaInput() #xx<-10
            variable <- varInput_evo() #variable <- "PROCEDIMIENTOS"
            mn <- which(MESES==input$mes)
            datos <- list(0)
            datos1 <- list(0)
            if(mn==1) {
                    for (i in c(1:2)) {
                            datos1[[i]] <- group_by(INFO[[i]][[x]][[xx]]$ranking[, c("PUNTAJE (0-1)","NIVEL DE RIESGO")], `NIVEL DE RIESGO`) %>% 
                                    summarize(Promedio=round(mean(`PUNTAJE (0-1)`),3))
                            datos[[i]] <- merge(datos1[[1]], datos1[[i]], by="NIVEL DE RIESGO", all.x=T)
                    }
                    f <- datos[[2]]
                    }
            if(mn>1) {
                    for (i in c(1:mn)) {
                            datos1[[i]] <- group_by(INFO[[i]][[x]][[xx]]$ranking[, c("PUNTAJE (0-1)","NIVEL DE RIESGO")], `NIVEL DE RIESGO`) %>% 
                                    summarize(Promedio=round(mean(`PUNTAJE (0-1)`),3))
                            datos[[1]] <- datos1[[1]]
                            if(i>1) {
                                    datos[[i]] <- merge(datos[[i-1]], datos1[[i]], by="NIVEL DE RIESGO", all.x=T)
                            }
                    }
                    f <- datos[[mn]]
            }
            resumen(f)
            })
    
    
    
    ##### EVOLUCION ######
    
    output$graf_evo_global <- renderHighchart({
            x <- grupoInput()
      graf_evo_global(evolucion_global(), varInput_evo(), titulo=x)
    })
    
    output$evo <- renderDataTable({
      evolucion_global()
    })
    
    output$NUE_evo <- renderDataTable({
      resumen_nuevos(evolucion_global())}
      )
    
    output$NUE_evo_graf <- renderChart2({
            x <- grupoInput()
      graf_evo(resumen_nuevos(evolucion_global()), tipo=x, titulo=x,varInput_evo())
    })
    
    ######## INDICADOR EVOLUCION ###############
    output$riesgo_medio <- renderDataTable({
            riesgo_medio()
    })
    
    output$riesgo_medio_graf <- renderChart2({
            x <- grupoInput()
      graf_evo(riesgo_medio(), tipo=x, titulo=x,varInput_evo(), evo=T)
    })
    

    ####### VARIACION INDICADOR EVOLUCION ####################
    output$variacion_medio <- renderDataTable({
      x <- grupoInput()
      variacion_relativa(riesgo_medio())
    })
    
    output$variacion_medio_graf <- renderChart2({
      x <- grupoInput()
      graf_evo(variacion_relativa(riesgo_medio()), tipo=x, titulo=x,variable, evo="VAR")
    })
    
    ######### RIESGO POR PROCESO ##########
    
    
    output$fecha_procesos  <- renderText({
      as.character(input$date)
    })
    
    dir_RData <- paste0(dir, "automaticos/riesgo_proceso_automatico/")
    
    data_riesgo_proceso <- reactive({
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        load(paste0(dir_RData,input$date,".RData"))
        lista[[1]][[1]]
      }else{
        data <- data_frame(0)
      }
    })
    
    
    funcion_riesgo_proceso_rescala <- function(TIPO="publicados"){
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        load(paste0(dir_RData,input$date,".RData"))
        
        if (TIPO=="publicados") {
          x <- 1
          if(as.Date(input$date)<= as.Date("2016-12-26")){
            data <- unique(lista[[x]][[6]][, c(1:4,6:15,18:23)])
          } else {
            data <- unique(lista[[x]][[6]][, c(1:4,6:17,20:25)])
          }
        } else {
          x <- 2
          if(as.Date(input$date)<= as.Date("2016-12-26")){
            data <- unique(lista[[x]][[6]][, c(1:4,6:18,21:26)])
          } else {
            data <- unique(lista[[x]][[6]][, c(1:4,6:19,22:27)])
          }
        }
        
        data$"Nivel de riesgo" <- data[, which(substring(names(data),1,5)=="Nivel")]
        
        data$zona <- ifelse(substring(data$cod_geografica, 1,2) %in% c('08','10','04','21'), '1',NA)
        data$zona <- ifelse(substring(data$cod_geografica, 1,2) %in% c('17','15','22'), '2',data$zona)
        data$zona <- ifelse(substring(data$cod_geografica, 1,2) %in% c('05','18','06','16'), '3',data$zona)
        data$zona <- ifelse(substring(data$cod_geografica, 1,2) %in% c('13','25'), '4',data$zona)
        data$zona <- ifelse(substring(data$cod_geografica, 1,2) %in% c('24','09','02','12','20'), '5',data$zona)
        data$zona <- ifelse(substring(data$cod_geografica, 1,2) %in% c('03','01','14'), '6',data$zona)
        data$zona <- ifelse(substring(data$cod_geografica, 1,2) %in% c('07','11','19'), '7',data$zona)
        data$zona <- ifelse(substring(data$cod_geografica, 1,5) %in% c('09-01','09-16','09-07'), '8',data$zona)
        data$zona <- ifelse(substring(data$cod_geografica, 1,5) %in% c('17-01'), '9',data$zona)
        
        if(zona_procesos_Input()=="10") {
          data
        } else {
          #data <- select(data,-contains("Nivel"))
                data <- data[, -grep("Nivel", colnames(data))]
          
          #puntaje_maximo <- max(data$"Puntaje de riesgo")
          #data <- data %>% filter(zona==2)
          data <- data %>% filter(zona==zona_procesos_Input())
          puntaje_maximo_zona <- max(data$"Puntaje de riesgo")
          
          for (i in c(1:dim(data)[1])) {
            data$"Puntaje de riesgo"[i] <- round(rescale(data$"Puntaje de riesgo"[i], to=c(0,1), from=c(0,puntaje_maximo_zona)),4)
          }
          
          data$"Nivel de riesgo" <- 0
          data$"Nivel de riesgo"[data$"Puntaje de riesgo" <= 0.20 ]                               <- "5. Riesgo Insignificante"
          data$"Nivel de riesgo"[data$"Puntaje de riesgo" >  0.20 & data$"Puntaje de riesgo" <= 0.40] <- "4. Riesgo Bajo"
          data$"Nivel de riesgo"[data$"Puntaje de riesgo" >  0.40 & data$"Puntaje de riesgo" <= 0.60] <- "3. Riesgo Medio"
          data$"Nivel de riesgo"[data$"Puntaje de riesgo" >  0.60 & data$"Puntaje de riesgo" <= 0.80] <- "2. Riesgo Alto"
          data$"Nivel de riesgo"[data$"Puntaje de riesgo" >  0.80 ]<-                                 "1. Riesgo Extremo"
          data <- arrange(data, desc(data$"Puntaje de riesgo"))
          data$RANKING <- 0
          for (i in c(1:dim(data)[1])) {
            data[i, "RANKING"] <- i
          }
          data
        }
      }else{
        data <- data_frame(0)
        data
      }
    }
    
    
    riesgo_proceso  <- reactive({
      funcion_riesgo_proceso_rescala("publicados")
    })
    
    output$riesgo_proceso  <-renderDataTable({
      data<-riesgo_proceso()
      dt <- datatable(data.frame(data),rownames =F ) %>%  formatCurrency(columns='Presupuesto', mark = ",")
      dt
    }, options = myoptions)
    
    entidades_procesos <- reactive({
      
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        
        data <- riesgo_proceso()
        
        data <- data[,c("RANKING","id_soli_compra","RUC de la entidad",	"Razón social de la entidad","Presupuesto")]
        
        agrup <- group_by(data, `RUC de la entidad`)
        data <- summarize(agrup, 
                          entidad=first(`Razón social de la entidad`),
                          procesos=length(id_soli_compra),
                          Presupuesto=sum(Presupuesto)
        )
        data <- arrange(data, desc(procesos))
        colnames(data) <- c("RUC de la entidad", "Razón social de la entidad", "Número de procesos", "Presupuesto")
        data
      }else{
        data <- data_frame(0)
        data
      }
    })
    
    
    output$entidades_procesos <- renderDataTable({
      datos<-entidades_procesos()
      dt <- datatable(datos,rownames =F ) %>%  formatCurrency(columns='Presupuesto')
      dt
    })
    
    riesgo_proceso_resumen  <- reactive({
      
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        
        data <- riesgo_proceso()
        
        if (input$agregado=='NIVEL DE RIESGO') {
          
          resumen <- group_by(data,`Nivel de riesgo`)
          
          resumen <-summarize(resumen,
                              procesos=length(id_soli_compra),
                              presupuesto=round(sum(Presupuesto), 2))
          
          colnames(resumen) <- c("Nivel de Riesgo", "Procesos", "Monto")
          
        } else #if (input$agregado=='NIVEL DE RIESGO POR TIPO') 
        {
          resumen <- group_by(data, tipoproceso)
          
          resumen <-summarize(resumen,
                              EXTREMO = length(which(niveles_riesgo=="1. Riesgo Extremo")),
                              ALTO = length(which(niveles_riesgo=="2. Riesgo Alto")),
                              MEDIO = length(which(niveles_riesgo=="3. Riesgo Medio")),
                              BAJO = length(which(niveles_riesgo=="4. Riesgo Bajo")),
                              INSIGNIFICANTE = length(which(niveles_riesgo=="5. Riesgo Insignificante"))
          )
          
          colnames(resumen) <- c("Tipo de Proceso", "Extremo", "Alto", "Medio", "Bajo", "Insignificante")
          
        }
        resumen
        
      }else{
        data <- data_frame(0)
        data
      }
      
    })
    
    
    output$riesgo_proceso_resumen <- renderDataTable({
      if (file.exists(paste(dir, "PROCESOS/PUBLICADOS/", gsub("-", "",input$date), ".csv", sep = ""))==TRUE) {
        resumen <- riesgo_proceso_resumen()
        #resumen <- rbind(resumen, c("TOTAL", sum(resumen$Procesos), sum(resumen$Monto)))
        
      }
      else {
        resumen <- data.frame()
      }
      dt <- datatable(resumen,rownames =F ) #%>%  formatCurrency(columns='Monto')
      dt
    },options = list(pageLength = dim(resumen)[1]))
    
    output$graf_resumen_procesos <- renderChart2({
      if (file.exists(paste(dir,"PROCESOS/PUBLICADOS/", gsub("-", "",input$date), ".csv", sep = ""))==TRUE) {
        datos <- riesgo_proceso_resumen()
        pie(datos=datos, v="Nivel de Riesgo", y=var_procesosInput(), titulo=4)
      }
      else {
        n1 <- Highcharts$new()
        n1
      }
    })
    
    output$graf_resumen_procesos_tipos <- renderChart2({
      if (file.exists(paste(dir, "PROCESOS/PUBLICADOS/", gsub("-", "",input$date), ".csv", sep = ""))==TRUE) {
        datos <- riesgo_proceso_resumen()
        pie(datos=datos, v="Tipo de Proceso", y=var_procesos_tipoInput(), titulo=4)
      }
      else {
        n1 <- Highcharts$new()
        n1
      }
    })
    
    
    output$downloadData_proceso <- downloadHandler(
      filename = function() { paste("Riesgo por proceso (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(riesgo_proceso(), file, tipo=4)
      })
    
    output$downloadData_proceso_entidades <- downloadHandler(
      filename = function() { paste("Entidades - Riesgo por proceso (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(entidades_procesos(), file, tipo=4)
      })
    
    
    output$pivot_publicados <- renderRpivotTable({
      rpivotTable(riesgo_proceso(),
                  rows=c("Tipo de proceso", "Estado"),
                  col="Nivel de riesgo", 
                  aggregatorName="Sum as Fraction of Total", 
                  vals="Presupuesto", 
                  rendererName="Heatmap")
    })
    
    
    ######### RIESGO POR PROCESO CON OFERTAS ##########
    
    output$fecha_procesos_ofertas  <- renderText({
      as.character(input$date)
    })
    
    
    riesgo_proceso_ofertas <- reactive({
      funcion_riesgo_proceso_rescala("ofertas")
    })
    
    
    output$riesgo_proceso_ofertas  <-renderDataTable({
      data<-riesgo_proceso_ofertas()
      dt <- datatable(data.frame(data),rownames =F ) %>%  formatCurrency(columns='Presupuesto', mark = ",")
      dt
    }, options = myoptions1
    )
    
    
    entidades_procesos_ofertas  <- reactive({
      
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        data <- riesgo_proceso_ofertas()
        
        data <- data[,c("RANKING","id_soli_compra","RUC de la entidad",	"Entidad","Presupuesto")]
        
        agrup <- group_by(data, `RUC de la entidad`)
        data <- summarize(agrup, 
                          entidad=first(Entidad),
                          procesos=length(id_soli_compra),
                          Presupuesto=sum(Presupuesto)
        )
        data <- arrange(data, desc(procesos))
        colnames(data) <- c("RUC de la entidad", "Razón social de la entidad", "Número de procesos", "Presupuesto")
        data
      }else{
        data <- data_frame(0)
        data
      }
    })
    
    output$entidades_procesos_ofertas  <- renderDataTable({
      data <- entidades_procesos_ofertas()
      dt <- datatable(data.frame(data),rownames =F ) %>%  formatCurrency(columns='Presupuesto')
      dt
    }, options = myoptions1)
    
    
    
    
    
    
    
    riesgo_proceso_ofertas_resumen  <- reactive({
      if (file.exists(paste(dir, "PROCESOS/OFERTAS/", gsub("-", "",input$date), ".csv", sep = ""))==TRUE) {
        procesos_con_invitados <- riesgo_proceso_ofertas()
        
        if (input$agregado1=='NIVEL DE RIESGO') {
          resumen <- group_by(procesos_con_invitados, `Nivel de riesgo`)
          resumen <-summarize(resumen,
                              procesos=length(id_soli_compra),
                              presupuesto=round(sum(Presupuesto), 2))
          
          colnames(resumen) <- c("Nivel de Riesgo", "Procesos", "Monto")
          
        }
        if (input$agregado1=='NIVEL DE RIESGO POR TIPO') {
          
          resumen <- group_by(procesos_con_invitados, tipoprocedimiento)
          
          resumen <-summarize(resumen,
                              EXTREMO = length(which(niveles_riesgo=="1. Riesgo Extremo")),
                              ALTO = length(which(niveles_riesgo=="2. Riesgo Alto")),
                              MEDIO = length(which(niveles_riesgo=="3. Riesgo Medio")),
                              BAJO = length(which(niveles_riesgo=="4. Riesgo Bajo")),
                              INSIGNIFICANTE = length(which(niveles_riesgo=="5. Riesgo Insignificante"))
          )
          
          colnames(resumen) <- c("Tipo de Proceso", "Extremo", "Alto", "Medio", "Bajo", "Insignificante")
        }
      }
      else {
        resumen <- data.frame()
      }
      resumen
    })
    
    
    output$riesgo_proceso_ofertas_resumen  <- renderDataTable({
      if (file.exists(paste(dir, "PROCESOS/OFERTAS/", gsub("-", "",input$date), ".csv", sep = ""))==TRUE) {
        resumen <- riesgo_proceso_ofertas_resumen()
        
      }
      else {
        resumen <- data.frame()
      }
      dt <- datatable(data.frame(resumen),rownames =F ) %>%  formatCurrency(columns='Monto')
      dt
    }#, options = myoptions
    )
    
    
    output$graf_resumen_procesos_ofertas <- renderChart2({
      if (file.exists(paste(dir, "PROCESOS/OFERTAS/", gsub("-", "",input$date), ".csv", sep = ""))==TRUE) {
        datos <- riesgo_proceso_ofertas_resumen()
        pie(datos=datos, v="Nivel de Riesgo", y=var_procesos_ofertasInput(), titulo=4)
      }
      else {
        n1 <- Highcharts$new()
        n1
      }
    })
    
    
    output$graf_resumen_procesos_ofertas_tipos <- renderChart2({
      if (file.exists(paste(dir, "PROCESOS/OFERTAS/", gsub("-", "",input$date), ".csv", sep = ""))==TRUE) {
        datos <- riesgo_proceso_ofertas_resumen()
        pie(datos=datos, v="Tipo de Proceso", y=var_procesos_ofertas_tipoInput(), titulo=4)
      }
      else {
        n1 <- Highcharts$new()
        n1
      }
    })
    
    output$downloadData_proceso_ofertas <- downloadHandler(
      filename = function() { paste("Riesgo por proceso con ofertas (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(riesgo_proceso_ofertas(), file, tipo=5)
      })      
    
    
    output$downloadData_proceso_entidades_ofertas <- downloadHandler(
      filename = function() { paste("Entidades - Riesgo por proceso con ofertas (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(entidades_procesos_ofertas(), file, tipo=4)
      })
    
    output$downloadData_preguntas <- downloadHandler(
      filename = function() { paste("Preguntas - Procesos Publicados - Categorias (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(pregunta_categoria_pub(), file, tipo=4)
      })
    
    output$downloadData_preguntas_id_soli <- downloadHandler(
      filename = function() { paste("Preguntas - Procesos Publicados - Puntaje de Preguntas (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(pregunta_categoria_id_pub(), file, tipo=4)
      })
    
    output$downloadData_preguntas_ofertas<- downloadHandler(
      filename = function() { paste("Preguntas - Procesos Publicados con ofertas- Categorias (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(pregunta_categoria_ofe(), file, tipo=4)
      })
    
    output$downloadData_preguntas_id_soli_oferta <- downloadHandler(
      filename = function() { paste("Preguntas - Procesos Publicados con ofertas - Puntaje de Preguntas (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(pregunta_categoria_id_ofe(), file, tipo=4)
      })
    
    
    output$pivot_ofertas <- renderRpivotTable({
      rpivotTable(riesgo_proceso_ofertas(),
                  rows=c("Tipo de proceso", "Estado"),
                  col="Nivel de riesgo", 
                  aggregatorName="Sum as Fraction of Total", 
                  vals="Presupuesto", 
                  rendererName="Heatmap")
    })
    
    
    riesgo_proceso_consolidado <-{
            listado_rdatas <- list.files(paste0(dir, "automaticos/riesgo_proceso_automatico"))
            publicados_rdata <- list(0)
            ofertas_rdata <- list(0)
            for (i in c(1:length(listado_rdatas))){
                    load(paste(dir, "automaticos/riesgo_proceso_automatico/",listado_rdatas[i], sep=""))
                    publicados_rdata[[i]] <- lista[[1]][[1]][[1]][, c("id_soli_compra", "codigo","entidadruc","entidad","niveles_riesgo", "publicacion")]
                    colnames(publicados_rdata[[i]]) <- c("id_soli_compra", "codigo","entidadruc","entidad","niveles_riesgo", "fechalimite")
                    publicados_rdata[[i]]$tipo <- "publicados"
                    ofertas_rdata[[i]] <- lista[[2]][[1]][[1]][, c("id_soli_compra", "codigo","entidadruc","entidad","niveles_riesgo","fechalimite")]
                    ofertas_rdata[[i]]$tipo <- "ofertas"
            }
            consolidado_rdata <- rbind(do.call(rbind, ofertas_rdata),do.call(rbind, publicados_rdata))
            colnames(consolidado_rdata) <- c("id_soli_compra", "Código del procedimiento","Ruc entidad","Entidad","Nivel de riesgo", "Fecha limite", "Tipo")
            consolidado_rdata
            #consolidado_rdata_sin_dup <- consolidado_rdata[c(which(duplicated(consolidado_rdata$id_soli_compra)==F)),]
    }
    
    output$riesgo_proceso_consolidado  <- renderDataTable({
            riesgo_proceso_consolidado
    }, options = myoptions1)
    
    output$downloadData_riesgo_proceso_consolidado <- downloadHandler(
            filename = function() { paste("Riesgo por proceso - Consolidado (", input$date, ').csv', sep = "") },
            content = function(file) {
                    #descarga_ex(riesgo_proceso_consolidado, file, tipo=6)
                    write.csv2(riesgo_proceso_consolidado, file)
            })
    
    # CPC PUBLICADOS
    cpc_publicados <- reactive({
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        load(paste0(dir_RData,input$date,".RData"))
        lista[[1]][[6]]
      }else{
        data <- data_frame(0)
      }
    })
    
    output$cpc_publicados  <- renderDataTable({
      data<-cpc_publicados()
      dt <- datatable(data.frame(data),rownames =F ) %>%  formatCurrency(columns='Presupuesto')
    }, options = myoptions1)
    
    output$downloadData_proceso_cpc_publicados <- downloadHandler(
      filename = function() { paste("Riesgo por proceso - publicados - CPC (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(cpc_publicados(), file, tipo=4)
      })
    ########################################################
    
    output$downloadData_consolidado_publicados <- downloadHandler(
      
      filename = function() { paste("Riesgo por proceso - consolidado publicados (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(consolidado_publicados(), file, tipo=4)
      })
    
    output$downloadData_consolidado_ofertas <- downloadHandler(
      filename = function() { paste("Riesgo por proceso - consolidado ofertas (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(consolidado_ofertas(), file, tipo=4)
      })
    ####################################################################
    # cpc ofertas
    cpc_ofertas <- reactive({
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        load(paste0(dir_RData,input$date,".RData"))
        lista[[2]][[6]]
      }else{
        data <- data_frame(0)
      }
    })
    
    output$cpc_ofertas  <- renderDataTable({
      data<-cpc_ofertas()
      dt <- datatable(data.frame(data),rownames =F ) %>%  formatCurrency(columns='Presupuesto')
    }, options = myoptions1)
    
    output$downloadData_proceso_cpc_ofertas <- downloadHandler(
      filename = function() { paste("Riesgo por proceso con ofertas - CPC (", input$date, ').xlsx', sep = "") },
      content = function(file) {
        descarga_ex(cpc_ofertas(), file, tipo=4)
      })
    
    funcion_consolidado_publicado_ofertas <- function(fec, pub_ofe){
      
      files <- list.files(paste(dir, "PROCESOS/",pub_ofe,"/",sep=""))
      files <- files[1:(length(files)-1)]
      files <- gsub(".csv","",files)
      files <- files[match(files[1],files) : match(fec,files)]
      rango_fechas <- files
      
      
      consolidado_pub_ofer <- data.frame()
      
      for (i in 1:length(rango_fechas)){
        ifelse(pub_ofe=="PUBLICADOS",
               {datos1 <- funcion_riesgo_proceso(fec=rango_fechas[i],TIPO = "PROCESOS")[[1]]
               datos1 <- datos1[,c("RANKING","id_soli_compra","codigo","tipoproceso","presupuesto","desc_compra","publicacion",
                                   "estadosolicitud","entidad","peso_presupuesto","peso_riesgo_entidad","puntaje_definitivo","niveles_riesgo")]
               },
               {datos1 <- funcion_riesgo_proceso_ofertas(fec=rango_fechas[i],TIPO = "PROCESOS")[[1]]
               datos1 <- datos1[,c("RANKING","id_soli_compra","codigo","tipoprocedimiento","presupuesto","desc_compra","fechalimite",
                                   "estadosolicitud","entidadruc","peso_presupuesto","peso_riesgo","puntaje_definitivo","niveles_riesgo")]
               })
        colnames(datos1) <- c("RANKING","id_soli_compra","codigo","tipoproceso","presupuesto","desc_compra","publicacion",
                              "estadosolicitud","entidad","peso_presupuesto","peso_riesgo_entidad","puntaje_definitivo","niveles_riesgo")
        
        
        
        consolidado_pub_ofer <- rbind(consolidado_pub_ofer,datos1)
      }
      return(consolidado_pub_ofer)
    }
    
    ##############################################    
    consolidado_publicados <- eventReactive(input$goButtonConsolidadoPublicados, {
      
      datos_publicados_publicados <- funcion_consolidado_publicado_ofertas(fec=fechaCons(), pub_ofe = "PUBLICADOS")
      datos_publicados_publicados
    })
    
    output$consolidado_publicados <- renderDataTable({
      consolidado_publicados()
      #write.xlsx2(datos_publicados_publicados,"datos_publicados_publicados.xlsx")
    })
    
    consolidado_ofertas <-eventReactive(input$goButtonConsolidadoOfertas, {
      datos_publicados_publicados <- funcion_consolidado_publicado_ofertas(fec=fechaCons(), pub_ofe = "OFERTAS")
      datos_publicados_publicados
    })
    
    
    
    
    output$consolidado_ofertas <- renderDataTable({
      consolidado_ofertas()
    })
    ##################################			
    
    
    ###########  PREGUNTA CATEGORIA PUBLICADOS ###########
    pregunta_categoria_pub <- reactive({
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        load(paste0(dir_RData,input$date,".RData"))
        lista[[1]][[4]]
      }else{
        data <- data_frame(0)
      }
    })
    output$pregunta_categoria<-renderDataTable({pregunta_categoria_pub()},option=myoptions1)
    
    ###### CLASIFICACION POR id_soli_compra   
    pregunta_categoria_id_pub <- reactive({
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        load(paste0(dir_RData,input$date,".RData"))
        lista[[1]][[5]]
      }else{
        data <- data_frame(0)
      }
    })
    
    output$categoria_id_soli <-renderDataTable({pregunta_categoria_id_pub()},option=myoptions1)
    
    preguntas_graf_pub <- reactive({
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        load(paste0(dir_RData,input$date,".RData"))
        lista[[1]][[7]]
      }else{
        data <- data_frame(0)
      }
    })
    
    output$graf_pregun_cate <- renderPlot({
      pregunta_categoria <- preguntas_graf_pub()
      grafico <- barplot(pregunta_categoria,main = "Número de Preguntas por categoría", col=color1, border="white")
      text(grafico,pregunta_categoria/2,pregunta_categoria)
    })
    
    
    ######################  PREGUNTA CATEGORIA OFERTAS ###########
    pregunta_categoria_ofe <- reactive({
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        load(paste0(dir_RData,input$date,".RData"))
        lista[[2]][[4]]
      }else{
        data <- data_frame(0)
      }
    })
    
    output$pregunta_categoria_oferta<-renderDataTable({pregunta_categoria_ofe()},option=myoptions1)
    
    
    ######## CLASIFICACION POR id_soli_compra 
    pregunta_categoria_id_ofe <- reactive({
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        load(paste0(dir_RData,input$date,".RData"))
        lista[[2]][[5]]
      }else{
        data <- data_frame(0)
      }
    })
    
    output$categoria_id_soli_ofertas <-renderDataTable({pregunta_categoria_id_ofe()},option=myoptions1)
    
    preguntas_graf_ofer <- reactive({
      if(file.exists(paste0(dir_RData,input$date,".RData"))==TRUE){
        load(paste0(dir_RData,input$date,".RData"))
        lista[[2]][[7]]
      }else{
        data <- data_frame(0)
      }
    })
    output$graf_pregun_cate_oferta <- renderPlot({
      pregunta_categoria <- preguntas_graf_ofer()
      grafico <- barplot(pregunta_categoria,main = "Número de Preguntas por categoría", col=color1, border="white")
      text(grafico,pregunta_categoria/2,pregunta_categoria)
    })
    
    
    }
  }

shinyApp(ui = ui, server = server)
