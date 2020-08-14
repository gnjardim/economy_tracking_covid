#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# packages ----------------------------------------------------------------
library(tidyverse)	
library(grid)	
library(cowplot)	
library(gridExtra)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)
library(widgetframe)


# source ------------------------------------------------------------------
source("load.R", encoding = "UTF-8")


# app ---------------------------------------------------------------------
ui <- fluidPage(
    
    # Application title
    titlePanel(""),
    useShinydashboard(),
    withMathJax(),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            # buttons
            actionButton(inputId = 'ab1', label = "Ir para o Painel de Previsões",
                         icon = icon("th"),
                         onclick = "location.href='https://insightdataanalysis.shinyapps.io/covidforecast/'",
                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            p(),
            actionButton(inputId = 'ab1', label = "Ir para o Painel de Subnotificação",
                         icon = icon("th"),
                         onclick = "location.href='https://insightdataanalysis.shinyapps.io/reportacao/'",
                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            p(),
            actionButton(inputId = 'ab2', label = "Ir para o Painel de Regiões de Saúde",
                         icon = icon("th"),
                         onclick = "location.href='https://insightdataanalysis.shinyapps.io/Rt_RS/'",
                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            p(),
            
            # input
            selectInput(inputId  = "UF",
                        label    = "Região",
                        choices  = choices,
                        selected = "Brasil")
            
            ),
        
        # Main Panel
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Índices de Atividade",
                                 box(width = NULL, collapsible = TRUE, collapsed = FALSE,
                                     title = 'Informações - clique no "-" para colapsar',
                                     status = "warning", solidHeader = TRUE,
                                     p('Essa página apresenta, para cada estado do país, diferentes índices diários que buscam acompanhar a atividade econômica, combinados com uma medida de espalhamento da doença na região.'),
                                     p('O eixo horizontal, comum a todos os gráficos, mostra o número de dias necessários para que o total de casos confirmados dobre. Quanto maior o tempo necessário (mais à direita no gráfico), mais devagar a doença está se difundindo.'),
                                     p('O eixo vertical representa medidas que possuem relação com a atividade econômica, e variam de acordo com o gráfico. Quanto maior o valor do índice, menor é a contração econômica, com a linha vermelha indicando nenhuma contração.'),
                                     p('- Para o gráfico de "Mobilidade", o índice utiliza os números do ', a(href = "https://www.google.com/covid19/mobility/", "Google Mobility Report", rel="noopener", target="_blank"), 'para medir a queda no nível de mobilidade na região, mais especificamente nos setores de "Varejo e lazer", "Mercados e farmácias" e "Locais de trabalho".'),
                                     p('- Para o gráfico de "Atividade/Mobilidade", o índice se baseia nos mesmos dados de mobilidade, ajustando pela relação entre esses valores e o impacto esperado na atividade econômica.'),
                                     p('- Para o gráfico de "Energia Total", o índice mostra a mudança percentual entre o consumo total de energia observado e o que era esperado com base no consumo anterior. Mais detalhes na aba "Observado x Predito".'),
                                     p('- Para o gráfico de "Energia (apenas ACL)", o índice mostra a mudança percentual entre o consumo no ambiente livre de energia observado e o que era esperado com base no consumo anterior. Mais detalhes na aba "Observado x Predito".'),
                                     p('As fases mostradas nos dois primeiros gráficos representam os momentos da economia. A fase de "Response" é o começo da contração econômica, marcada por uma queda abrupta na mobilidade. A fase seguinte, "Trough", mostra o momento em que a mobilidade alcança seu mínimo, devido às medidas de contenção da pandemia. Ver mais em ', a(href = "https://www.luohanacademy.com/PET", "Pandemic Economy Tracker", rel="noopener", target="_blank"))
                                     ),
                                 
                                 withSpinner(plotlyOutput("plot_atividade"))
                                 ),
                        
                        tabPanel("Observado x Predito",
                                 box(width = NULL, collapsible = TRUE, collapsed = FALSE,
                                     title = 'Informações - clique no "-" para colapsar',
                                     status = "warning", solidHeader = TRUE,
                                     p('Essa página apresenta, para cada estado do país, o consumo de energia diário observado e o valor predito com base na regressão abaixo:'),
                                     helpText('\\begin{equation}
                                               \\text{Consumo Diario}_{t} = \\beta_0 + \\sum_{i=2}^{12} \\delta_i D_{\\text{mês}_{it}} + \\sum_{i=2}^{7} \\lambda_i D_{\\text{dia da semana}_{it}} + \\sum_{i=2}^{k} \\theta_i D_{\\text{feriado}_{it}} + \\phi t + \\epsilon_t
                                               \\end{equation}'),
                                     p('A partir do dia 25/02/2020 (marcado pela linha vermelha tracejada), usamos os valores preditos como o esperado para o consumo de energia. A diferença percentual mostrada na aba "Índices de Atividade" para os gráficos "Energia Total" e "Energia (apenas ACL)" se baseia nesses valores.')
                                     ),
                                 
                                 withSpinner(plotlyOutput("plot_pred"))
                                 )
            
                        )
                )
        )
)


# server ------------------------------------------------------------------
server <- function(input, output) {

    # The currently selected tab from the first box
    output$plot_atividade <- renderPlotly({
        
        plot_shiny(UF = input$UF, 
                   tipo = 'atividade')
        
    })
    
    output$plot_pred <- renderPlotly({
        
        plot_shiny(UF = input$UF, 
                   tipo = 'pred')
        
    })
}


# run ---------------------------------------------------------------------
shinyApp(ui, server)
