##########################################
## Shiny app indicadores económicos Chile
## por Nicolás Rivera G.
##########################################

# Librerías ----
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(mindicador)) install.packages("mindicador")
if(!require(DT)) install.packages("DT")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(plotly)) install.packages("ploty")
if(!require(scales)) install.packages("scales")

library(tidyverse)
library(ggplot2)
library(mindicador)
library(DT)
library(shinythemes)
library(plotly)
library(scales)

# Algunos arreglos previos ----
lista_indicadores <- sort(mindicador_indicadores$codigo[mindicador_indicadores$codigo != "dolar_intercambio"])
lista_indicadores <- gsub("_", " ", lista_indicadores)

inicio <- max(mindicador_indicadores$desde)

agnos <- seq(from = inicio, to = 2021, by = 1)

# App ----
ui <- fluidPage(
    theme = shinytheme("united"), # Se cambia el look
    
    titlePanel(h1("Indicadores económicos para Chile")), 
    
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "agno1",
                "Desde:",
                choices = agnos,
                selected = inicio),
            
                selectInput(
                    "agno2",
                    "Hasta:",
                    choices = agnos,
                    selected = "2021"),

            selectInput(
                "indicador",
                "Indicador:",
                choices = lista_indicadores,
                selected = "dolar"),
            
            withTags({
                div("Fuente: Elaboración propia a través de", a(href="https://mindicador.cl", "#MindicadorChile"))
            })

            ),
        
        mainPanel(
            tabsetPanel(
                id = "tab",
                type = "pill",
                tabPanel("Gráfico", 
                         plotlyOutput("grafico")),
                
                tabPanel("Tabla", DTOutput("tabla"))
        )
        )
    )
)

server <- function(input, output) {
    
    output$grafico <- renderPlotly({
        
    indicador <- input$indicador
    
    indicador <- gsub(" ", "_", indicador)
    
    agno1 <- input$agno1
    agno2 <- input$agno2
        
    d <- mindicador_importar_datos(indicador,
                                   anios = agno1:agno2)
    
    # Se reemplazan valores "raros" de la UF (fuente: BCCh)
    if(indicador == "uf"){
        d$valor[d$fecha == "2014-12-29"] <- 24627.10
        d$valor[d$fecha == "2014-12-30"] <- 24627.10
    }
    
    ggplot(d) +
        geom_line(aes(fecha, valor), color = "#D62D20") +
        labs(title = gsub("_", " ", indicador)) +
        scale_y_continuous(label = comma)
    })
    
    output$tabla <- renderDT({ 
        
        indicador <- input$indicador
        indicador <- gsub(" ", "_", indicador)
        
        agno1 <- input$agno1
        agno2 <- input$agno2
        
        d <- mindicador::mindicador_importar_datos(indicador, 
                                                   anios = agno1:agno2)
        
    # Se reemplazan valores "raros" (fuente: BCCh)
    if(indicador == "uf"){
        d$valor[d$fecha == "2014-12-29"] <- 24627.10
        d$valor[d$fecha == "2014-12-30"] <- 24627.10
    }
    
    d$serie <- gsub("_", " ", d$serie)
    datatable(d) %>% 
        formatCurrency("valor", 
                       currency = "", 
                       interval = 3, 
                       mark = ",")
    })
    
}

shinyApp(ui = ui, server = server)
