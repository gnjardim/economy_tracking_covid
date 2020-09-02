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
            p(),
            
            # input
            selectInput(inputId  = "UF",
                        label    = "Região",
                        choices  = choices,
                        selected = "Brasil"),
            
            p(),
            downloadLink("downloadData", "Baixar Dados"),
            p(),
            a(href = "https://github.com/gnjardim/economy_tracking_covid", "Códigos", rel="noopener", target="_blank")
            
            ),
        
        # Main Panel
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Índices de Atividade",
                                 box(width = NULL, collapsible = TRUE, collapsed = FALSE,
                                     title = 'Informações - clique no "-" para colapsar',
                                     status = "warning", solidHeader = TRUE,
                                     p('Essa página apresenta uma ferramenta, inspirada no ', a(href = "https://www.luohanacademy.com/PET", "Global Pandemic Economy Tracker (PET)", rel="noopener", target="_blank"), 'desenvolvido pela Luohan Academy, para acompanhar a evolução da pandemia em cada estado do país, juntamente com proxies para a atividade econômica.'),
                                     p('O eixo horizontal, comum a todos os gráficos, mostra o número de dias necessários para que o total de casos confirmados dobre. Mais especificamente, são os dias entre uma determinada data com um determinado número de casos e a data anterior na qual o número de casos confirmados fosse a metade. Quanto maior o tempo necessário (mais à direita no gráfico), mais devagar o vírus está se difundindo.'),
                                     p('O eixo vertical representa medidas que possuem relação com a atividade econômica, e variam de acordo com o gráfico. Quanto maior o valor do índice, menor é a contração econômica, com a linha vermelha indicando nenhuma contração.'),
                                     p('Como os gráficos não estão organizados no eixo temporal, é possível que a trajetória das curvas "ande para trás", representando um período em que houve uma piora na evolução da pandemia.'),
                                     p('- Para o gráfico de "Mobilidade", o índice utiliza os números do ', a(href = "https://www.google.com/covid19/mobility/", "Google Mobility Report", rel="noopener", target="_blank"), 'para medir a queda no nível de mobilidade na região, mais especificamente nos setores de "Varejo e lazer", "Mercados e farmácias" e "Locais de trabalho".'),
                                     p('- Para o gráfico de "Atividade/Mobilidade", o índice é construído com os mesmos dados de mobilidade, apenas mapeando-o para atividade econômica, de acordo com a regressão estimada pelo Pandemic Economy Tracker com dados de PIB e mobilidade para mais de 50 países no primeiro trimestre de 2020.'),
                                     p('- Para o gráfico de "Energia Total", o índice mostra a mudança percentual entre o consumo total de energia elétrica observado e o que foi projetado por um modelo contrafactual excluindo o efeito da Covid-19. Para mais detalhes, ver aba "Observado x Predito".'),
                                     p('- Para o gráfico de "Energia (apenas ACL)", o índice mostra a mudança percentual entre o consumo no ambiente livre de energia elétrica observado e o que foi projetado por um modelo contrafactual excluindo o efeito da Covid-19. Para mais detalhes, ver aba "Observado x Predito".'),
                                     p('Com uma previsão de alta nas tarifas do ACR, o fluxo já existente de migração do ACR para o ACL pode ter sido ainda mais acelerado com a pandemia. Assim, em alguns estados houve um aumento do consumo observado com relação ao cenário que exclui o efeito da Covid-19 (contrafactual). Por isso, alguns estados ainda permanecem sob investigação e seus gráficos de "Energia Total" e "Energia (apenas ACL)" ainda não estão disponíveis.'),
                                     p('As fases mostradas nos dois primeiros gráficos representam os momentos da economia. A fase de "Response" é o começo da contração econômica, marcada por uma queda abrupta na mobilidade. A fase seguinte, "Trough", mostra o momento em que a mobilidade alcança seu mínimo, devido às medidas de contenção da pandemia. Para mais informações, ver ', a(href = "https://www.luohanacademy.com/PET", "Global Pandemic Economy Tracker.", rel="noopener", target="_blank"))
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
                                     p('A partir do dia 25/02/2020 (marcado pela linha vermelha tracejada), usamos os valores preditos como o esperado para o consumo de energia elétrica. A diferença percentual mostrada na aba "Índices de Atividade" para os gráficos "Energia Total" e "Energia (apenas ACL)" se baseia nesses valores. Os dados utilizados são disponibilizados pela ', a(href = "https://public.tableau.com/profile/ccee.informa.es.ao.mercado#!/vizhome/ConsumodeenergianoSIN/AnlisedeconsumonoSIN", "CCEE.", rel="noopener", target="_blank")),
                                     p('Com uma previsão de alta nas tarifas do ACR, o fluxo já existente de migração do ACR para o ACL pode ter sido ainda mais acelerado com a pandemia. Assim, em alguns estados houve um aumento do consumo observado com relação ao cenário que exclui o efeito da Covid-19 (contrafactual). Por isso, alguns estados ainda permanecem sob investigação e seus gráficos de "Energia Total" e "Energia (apenas ACL)" ainda não estão disponíveis.')
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
    
    output$downloadData <- downloadHandler(
        filename = "covid_atividade.csv",
        content = function(file) {
            write_csv2(rbind(brasil, estados) %>% 
                           rename(ma_doubl_days = smth_date,
                                  ma_mobility   = smth_mob), 
                       file)
        },
        contentType = "text/csv"
    )
}


# run ---------------------------------------------------------------------
shinyApp(ui, server)
