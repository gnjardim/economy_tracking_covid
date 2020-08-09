#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(grid)
library(cowplot)
library(gridExtra)
library(plotly)


# source ------------------------------------------------------------------
source('code/_plot_functions.R', encoding = 'UTF-8')


# load data ---------------------------------------------------------------
brasil  <- read_csv("tmp/brasil.csv")
estados <- read_csv("tmp/estados.csv")

choices <- c("Brasil", unique(estados %>% pull(estado)))


# app ---------------------------------------------------------------------
theme_set(theme_bw())

# Define UI for application
ui <- fluidPage(
    
    
    # Application title
    titlePanel("Atividade x COVID"),
    
    # Sidebar with inputs for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId  = "UF",
                        label    = "Região",
                        choices  = choices,
                        selected = "Brasil"),
            
            radioButtons(inputId = "tipo", 
                         label   = "",
                         choices = c("Índice de Atividade" = "atividade",
                                     "Observado x Predito" = "pred"))
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: plotly ----
            plotlyOutput("plot")
            
        )
    )
)

# Define server logic required
server <- function(input, output) {
    
    output$plot <- renderPlotly({
        plot_shiny(UF   = input$UF, 
                   tipo = input$tipo)
        }
    )
}

# Run the application 
shinyApp(ui, server)
