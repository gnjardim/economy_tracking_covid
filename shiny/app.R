#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)
library(widgetframe)
library(ggiraph)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel(""),
    useShinydashboard(),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId  = "UF",
                        label    = "Região",
                        choices  = choices,
                        selected = "Brasil")
            
            ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Índice de Atividade",
                                 box(title = 'Informações - clique no "-" para colapsar', collapsible = TRUE, solidHeader = TRUE,
                                     status="warning",
                                     p("INFORMACOES")),
                                 withSpinner(plotlyOutput("plot_atividade")))),                        
                        tabPanel("Observado x Predito",
                                box(title = 'Informações - clique no "-" para colapsar', collapsible = TRUE, solidHeader = TRUE,
                                    status="warning",
                                        p("INFORMACOES")),
                                    withSpinner(plotlyOutput("plot_pred")))
            
        )
    )
)
)


server <- function(input, output) {
    # The currently selected tab from the first box
    output$plot_atividade <- renderPlot({
        
        plot_shiny(UF = input$UF, 
                   tipo = 'atividade')
        
    })
    
    output$plot_pred <- renderPlot({
        
        plot_shiny(UF = input$UF, 
                   tipo = 'pred')
        
    })
}




# Run the application 
shinyApp(ui, server)


