library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(shinyBS)
library(DT)
library(shinyWidgets)


source("funciones.R")

# Data to be used
setwd("C:/Users/usaurio/Desktop/IgnacioCarpeta/Shiny R")#se debe colocar la ruta de la carpeta donde esté ubicado el archivo prueba_shiny_3.RData

load("influenza1923.RData")

dataTotal <- influenza_2019_2023
dataTotal$edad <- as.integer(dataTotal$edad)
dataTotal <- rename(dataTotal, Campaña = campana, ComunaOcurrencia = comuna_ocurr, ComunaResidencia = comuna_residencia, Fecha = fecha_inmunizacion)

comunas_seleccionadas <- c("Rancagua", "Machalí", "Chépica", "Chimbarongo","Codegua","Coinco","Pichilemu","Nancagua","Navidad","Graneros","Coltauco","Las Cabras","La Estrella","Pumanque","Palmilla","Quinta de Tilcoco","Paredones","San Vicente","Santa Cruz","San Fernando","Rengo")

user_base <- tibble::tibble(
  user = c("user1", "user2","nacho", "admin"),
  password = c("pass1", "pass2","sunclay21", 12345),
  permissions = c("admin", "standard","admin","admin"),
  name = c("User One", "User Two", "User Three", "User Four")
)

# UI
ui <-fluidPage(
  shinyauthr::loginUI(id = "login"),
  div(
    id = "mostrar-pagina",
  
  dashboardPage(
    dashboardHeader(
    title = tags$img(src = "logo_ues_blanco.png", height = "100px"),
    titleWidth = 220  
    ),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tabla de datos", icon = icon("table"), tabName = "resume11n"),
      menuItem("Visualizaciones", icon = icon("table"), tabName = "visualizaciones", selected = TRUE)
      # menuItem("Tabla de datos", icon = icon("dashboard"), tabName = "resumen")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName = "resumen",
        fluidRow(
          box(width = 12,tags$p(
            style = "font-size: 30px; font-weight: bold;",  # Ajustar el tamaño de letra deseado
            "Descargar"
          ),
          # DTOutput("table")  
          ),
        )
      ),
      tabItem(
        tabName = "visualizaciones",
        fluidRow(
          column(
            width = 2,
            selectInput(
              inputId = "visualizacion",
              tags$label("Selecciona la visualización gráfica:", style = "font-size: 26px; color: black;"),
              choices = c("Gráfico de líneas", "Gráfico de barras", "Gráfico de barras apilado"),
              selected = "Gráfico de líneas"
            )
          ),
          fluidRow(
            column(width = 9,
                   valueBoxOutput("valuebox"), 
                   valueBoxOutput("valuebox_ocu"),
                   valueBoxOutput("valuebox_resi"),
            )),
          conditionalPanel(
            condition = 'input.visualizacion == "Gráfico de líneas"',
            tags$label(HTML(" Comuna Ocurrencia&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Comuna Residencia"), style = "font-size: 50px; color: black;"),
            
            fluidRow(
              column(width =1,
                     sliderInput(
                       inputId = "filtro_edad_ocu1",
                       label = "Rango de edad:",
                       min = min(dataTotal$edad),
                       max = max(dataTotal$edad),
                       value = c(0, 119)
                     )
              ),
              column(
                width = 1,
                pickerInput(
                  inputId = "sexo_line_ocu",
                  label = "Sexo",
                  choices = unique(dataTotal$sexo),
                  selected = unique(dataTotal$sexo),
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE, 
                    size = 10, 
                    liveSearch = TRUE  # Esto habilita la búsqueda de texto
                  )
                )
              ),
              
              column(width = 1,
                     pickerInput(
                       inputId = "campaña_line_ocu",
                       label = "Campaña",
                       choices = unique(dataTotal$Campaña),
                       selected = unique(dataTotal$Campaña),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
                     
              ),
              
              column(width = 1,
                     pickerInput(
                       inputId = "dosis_line_ocu",
                       label = "Dosis",
                       choices = unique(dataTotal$dosis),
                       selected = unique(dataTotal$dosis),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
              ),
              
              column(
                width = 1,
                pickerInput(
                  inputId = "comuna_input_ocu1", 
                  label = "Comuna:",
                  choices = unique(dataTotal$ComunaOcurrencia),
                  selected = comunas_seleccionadas,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                    size = 10, # Número de elementos visibles
                    liveSearch = TRUE # Activa la búsqueda en vivo
                  )
                )
              ),
              
              column(width = 1,),
              column(width = 1,
                     sliderInput(
                       inputId = "filtro_edad_resi1",
                       label = "Rango de edad:",
                       min = min(dataTotal$edad),
                       max = max(dataTotal$edad),
                       value = c(0, 119)
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "sexo_line_resi",
                       label = "Sexo",
                       choices = unique(dataTotal$sexo),
                       selected = unique(dataTotal$sexo),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE
                         
                       )
                     )
                     
              ),
              
              
              
              column(width = 1,
                     pickerInput(
                       inputId = "campaña_line_resi",
                       label = "Campaña",
                       choices = unique(dataTotal$Campaña),
                       selected = unique(dataTotal$Campaña),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
                     
              ),
              
              column(width = 1,
                     pickerInput(
                       inputId = "dosis_line_resi",
                       label = "Dosis",
                       choices = unique(dataTotal$dosis),
                       selected = unique(dataTotal$dosis),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
              ),
              
              column(width = 1,
                     pickerInput(
                       inputId = "comuna_input_resi1", 
                       label = "Comuna:",
                       choices = unique(dataTotal$ComunaResidencia),
                       selected = comunas_seleccionadas,
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
              ),
              
              
            ),
            fluidRow(
              column(
                width = 12,
                splitLayout(cellWidths = c("50%", "50%"),
                            plotlyOutput("grafico_linea_ocurr"),
                            plotlyOutput("grafico_linea_resi"),
                            
                )
              )),
            fluidRow(
              box(width = 12, tags$p(
                style = "font-size: 30px; font-weight: bold;",
                "// Descargar"
              ),
              DTOutput("table1")
              )
            ),
          ),
          
###########################################################################################################################################################################################################              
          
          
          conditionalPanel(
            condition = 'input.visualizacion == "Gráfico de barras"',
            
            fluidRow(),
            tags$label(HTML(" Comuna Ocurrencia&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Comuna Residencia"), style = "font-size: 50px; color: black;"),
            fluidRow(
              column(width = 1,
                     sliderInput(
                       inputId = "filtro_edad_ocu",
                       label = "Rango de edad:",
                       min = min(dataTotal$edad),
                       max = max(dataTotal$edad),
                       value = c(0, 119)
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "sexo_barra_ocu",
                       label = "Sexo",
                       choices = unique(dataTotal$sexo),
                       selected = unique(dataTotal$sexo),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE,
                         size = 10,
                         liveSearch = TRUE
                       )
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "campaña_barra_ocu",
                       label = "Campaña",
                       choices = unique(dataTotal$Campaña),
                       selected = unique(dataTotal$Campaña),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE,
                         size = 10,
                         liveSearch = TRUE
                       )
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "dosis_barra_ocu",
                       label = "Dosis",
                       choices = unique(dataTotal$dosis),
                       selected = unique(dataTotal$dosis),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE,
                         size = 10,
                         liveSearch = TRUE
                       )
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "comuna_input_ocu",
                       label = "Comuna",
                       choices = unique(dataTotal$ComunaOcurrencia),
                       selected = comunas_seleccionadas,
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE,
                         size = 10,
                         liveSearch = TRUE
                       )
                     )
              ),
              
              column(width = 1,),
              
              column(width = 1,
                     sliderInput(
                       inputId = "filtro_edad_resi",
                       label = "Rango de edad:",
                       min = min(dataTotal$edad),
                       max = max(dataTotal$edad),
                       value = c(0, 119)
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "sexo_barra_resi",
                       label = "Sexo",
                       choices = unique(dataTotal$sexo),
                       selected = unique(dataTotal$sexo),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE,
                         size = 10,
                         liveSearch = TRUE
                       )
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "campaña_barra_resi",
                       label = "Campaña",
                       choices = unique(dataTotal$Campaña),
                       selected = unique(dataTotal$Campaña),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE,
                         size = 10,
                         liveSearch = TRUE
                       )
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "dosis_barra_resi",
                       label = "Dosis",
                       choices = unique(dataTotal$dosis),
                       selected = unique(dataTotal$dosis),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE,
                         size = 10,
                         liveSearch = TRUE
                       )
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "comuna_input_resi",
                       label = "Comuna",
                       choices = unique(dataTotal$ComunaResidencia),
                       selected = comunas_seleccionadas,
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE,
                         size = 10,
                         liveSearch = TRUE
                       )
                     )
              )
              
            ),
            
            
            fluidRow(
              column(
                width = 12,
                splitLayout(cellWidths = c("50%", "50%"),
                            plotlyOutput("grafico_barra_ocu"),
                            plotlyOutput("grafico_barra_resi")
                )
              )
            ),
            
            fluidRow(
              box(width = 12, tags$p(
                style = "font-size: 30px; font-weight: bold;",
                "// Descargar"
              ),
              DTOutput("table")
              )
            )
          ),
###########################################################################################################################################################################################################                        ####################################################################################################################################################################                           
          conditionalPanel(
            condition = 'input.visualizacion == "Gráfico de barras apilado"',
            fluidRow(),
            tags$label(HTML(" Comuna Ocurrencia&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Comuna Residencia"), style = "font-size: 50px; color: black;"),
            
            fluidRow(
              column(width = 1,
                     sliderInput(
                       inputId = "filtro_edad_barocu",
                       label = "Rango de edad:",
                       min = min(dataTotal$edad),
                       max = max(dataTotal$edad),
                       value = c(0, 119)
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "sexo_barrapi_ocu",
                       label = "Sexo",
                       choices = unique(dataTotal$sexo),
                       selected = unique(dataTotal$sexo),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE
                         
                       )
                     )
                     
              ),
              
              column(width = 1,
                     pickerInput(
                       inputId = "campaña_barrapi_ocu",
                       label = "Campaña",
                       choices = unique(dataTotal$Campaña),
                       selected = unique(dataTotal$Campaña),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
                     
              ),
              
              column(width = 1,
                     pickerInput(
                       inputId = "dosis_barrapi_ocu",
                       label = "Dosis",
                       choices = unique(dataTotal$dosis),
                       selected = unique(dataTotal$dosis),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
              ),
              
              column(width = 1,
                     pickerInput(
                       inputId = "comuna_input_apiocu", 
                       label = "Comuna:",
                       choices = unique(dataTotal$ComunaOcurrencia),
                       selected = comunas_seleccionadas,
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
              ),
              
              column(width = 1,),
              
              
              column(width = 1,
                     sliderInput(
                       inputId = "filtro_edad_barresi",
                       label = "Rango de edad:",
                       min = min(dataTotal$edad),
                       max = max(dataTotal$edad),
                       value = c(0, 119)
                     )
              ),
              column(width = 1,
                     pickerInput(
                       inputId = "sexo_barrapi_resi",
                       label = "Sexo",
                       choices = unique(dataTotal$sexo),
                       selected = unique(dataTotal$sexo),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE
                         
                       )
                     )
                     
              ),
              
              
              column(width = 1,
                     pickerInput(
                       inputId = "campaña_barrapi_resi",
                       label = "Campaña",
                       choices = unique(dataTotal$Campaña),
                       selected = unique(dataTotal$Campaña),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
                     
              ),
              
              column(width = 1,
                     pickerInput(
                       inputId = "dosis_barrapi_resi",
                       label = "Dosis",
                       choices = unique(dataTotal$dosis),
                       selected = unique(dataTotal$dosis),
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
              ),
              
              column(width = 1,
                     pickerInput(
                       inputId = "comuna_input_apiresi", 
                       label = "Comuna:",
                       choices = unique(dataTotal$ComunaOcurrencia),
                       selected = comunas_seleccionadas,
                       multiple = TRUE,
                       options = list(
                         `actions-box` = TRUE, # Muestra un botón para mostrar/ocultar todos los elementos
                         size = 10, # Número de elementos visibles
                         liveSearch = TRUE # Activa la búsqueda en vivo
                       )
                     )
              ),
              
            ),
            fluidRow(
              column(
                width = 12,
                splitLayout(cellWidths = c("50%", "50%"),
                            plotlyOutput("grafico_barrapi_ocu"),
                            plotlyOutput("grafico_barrapi_resi"),
                            
                )
              )),
            fluidRow(
              box(width = 12, tags$p(
                style = "font-size: 30px; font-weight: bold;",
                "// Descargar"
              ),
              DTOutput("table2")
              )
            ),
            
          )
###########################################################################################################################################################################################################                        ####################################################################################################################################################################              
        )
        
      )
    )
  )
))%>%shinyjs::hidden()
)

# Server
server <- function(input, output) {
  
  shiny::observe({
    shiny::req(credentials()$user_auth)
    shinyjs::show(id ="mostrar-pagina")
    
  })
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  #Creando valuebox según filtros en comuna ocurrencia
  output$valuebox_ocu <- renderInfoBox({
    if (input$visualizacion == "Gráfico de líneas") {
      
      data_filtrada <- apply_filters(dataTotal, input$comuna_input_ocu1, input$sexo_line_ocu, input$filtro_edad_ocu1, input$campaña_line_ocu, input$dosis_line_ocu,"ComunaOcurrencia")
      create_dynamic_value_box(data_filtrada, "ComunaOcurrencia", "Vacunados por filtro en Comuna de Ocurrencia", icon("syringe"), "yellow")
    }
    
    else  if (input$visualizacion == "Gráfico de barras") {
      
      data_filtrada <- apply_filters(dataTotal, input$comuna_input_ocu, input$sexo_barra_ocu, input$filtro_edad_ocu, input$campaña_barra_ocu, input$dosis_barra_ocu,"ComunaOcurrencia")
      create_dynamic_value_box(data_filtrada, "ComunaOcurrencia", "Vacunados por filtro en Comuna de Ocurrencia", icon("syringe"), "yellow")
    }
    else if (input$visualizacion == "Gráfico de barras apilado") {
      
      data_filtrada <- apply_filters(dataTotal, input$comuna_input_apiocu, input$sexo_barrapi_ocu, input$filtro_edad_barocu, input$campaña_barrapi_ocu, input$dosis_barrapi_ocu,"ComunaOcurrencia")
      create_dynamic_value_box(data_filtrada, "ComunaOcurrencia", "Vacunados por filtro en Comuna de Ocurrencia", icon("syringe"), "yellow")
      
    }
  })
  
  #Creando valuebox según filtros en comuna residencia
  output$valuebox_resi <- renderInfoBox({
    if (input$visualizacion == "Gráfico de líneas") {
      
      data_filtrada <- apply_filters(dataTotal, input$comuna_input_resi1, input$sexo_line_resi, input$filtro_edad_resi1, input$campaña_line_resi, input$dosis_line_resi,"ComunaResidencia")
      create_dynamic_value_box(data_filtrada, "ComunaResidencia", " Vacunados por filtro en Comuna de Residencia", icon("syringe"), "orange")
    }
    else if (input$visualizacion == "Gráfico de barras") {
      
      data_filtrada <- apply_filters(dataTotal, input$comuna_input_resi, input$sexo_barra_resi, input$filtro_edad_resi, input$campaña_barra_resi, input$dosis_barra_resi,"ComunaResidencia")
      create_dynamic_value_box(data_filtrada, "ComunaResidencia"," Vacunados por filtro en Comuna de Residencia", icon("syringe"), "orange")
      
    }
    else if (input$visualizacion == "Gráfico de barras apilado") {
      
      data_filtrada <- apply_filters(dataTotal, input$comuna_input_apiresi, input$sexo_barrapi_resi, input$filtro_edad_barresi, input$campaña_barrapi_resi, input$dosis_barrapi_resi,"ComunaResidencia")
      create_dynamic_value_box(data_filtrada, "ComunaResidencia", " Vacunados por filtro en Comuna de Residencia", icon("syringe"), "orange")
      
    }
  })
  
  #Crear valuebox general
  output$valuebox <- renderInfoBox({
    total_datos <- nrow(dataTotal)
    create_value_box(total_datos, "Total de Vacunados en la Región", icon("syringe"), "aqua")
  })
  
  
  #Crear datatable a traveés de función
  output$table <- renderDT({
    create_table(dataTotal) # Llama a la función con tus datos
  })
  output$table1 <- renderDT({
    create_table(dataTotal) # Llama a la función con tus datos
  })
  output$table2 <- renderDT({
    create_table(dataTotal) # Llama a la función con tus datos
  })
  
  
  #Crear grafico de linea a través de funciones
  
  output$grafico_linea_ocurr <- renderPlotly({
    create_line_ocuresi(dataTotal, input$filtro_edad_ocu1, input$sexo_line_ocu, input$dosis_line_ocu, input$comuna_input_ocu1, input$campaña_line_ocu, "ocurrencia")
  })
  
  output$grafico_linea_resi <- renderPlotly({
    create_line_ocuresi(dataTotal, input$filtro_edad_resi1, input$sexo_line_resi, input$dosis_line_resi, input$comuna_input_resi1, input$campaña_line_resi, "residencia")
  })
  
  # Crear graficos de barra a través de funciones
  output$grafico_barra_ocu <- renderPlotly({
    title <- "Gráfico Influenza Comuna Ocurrencia"
    create_bar_ocuresi(dataTotal, input$filtro_edad_ocu, input$sexo_barra_ocu, input$dosis_barra_ocu, input$campaña_barra_ocu, input$comuna_input_ocu, title, "ComunaOcurrencia")
  })
  
  output$grafico_barra_resi <- renderPlotly({
    title <- "Gráfico Influenza Comuna Residencia"
    create_bar_ocuresi(dataTotal, input$filtro_edad_resi, input$sexo_barra_resi, input$dosis_barra_resi, input$campaña_barra_resi, input$comuna_input_resi, title, "ComunaResidencia")
  })
  
  #Crear gráficos de barra apilado a través de funciones
  output$grafico_barrapi_ocu <- renderPlotly({
    create_stacked_bar_plot(dataTotal, input$filtro_edad_barocu, input$sexo_barrapi_ocu, input$dosis_barrapi_ocu, input$comuna_input_apiocu, input$campaña_barrapi_ocu, "Gráfico Influenza Comuna Ocurrencia", "ComunaOcurrencia")
  })
  
  output$grafico_barrapi_resi <- renderPlotly({
    create_stacked_bar_plot(dataTotal, input$filtro_edad_barresi, input$sexo_barrapi_resi, input$dosis_barrapi_resi, input$comuna_input_apiresi, input$campaña_barrapi_resi, "Gráfico Influenza Comuna Residencia", "ComunaResidencia")
  })
  
  
  
}

shinyApp(ui, server)