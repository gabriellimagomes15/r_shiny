library(shiny)

#### 4. Inserindo conteudo na app  ####


## front-end (tela que será mostrada para o usuário)
ui = fluidPage(
  ## título da página
  titlePanel("Dashboard PROCON"),
  
  mainPanel(
    ## gráfico de linhas
    plotlyOutput(outputId = 'data',width = '100%'),
    ## texto descritivo do gráfico de linhas
    textOutput(outputId = "descData"),
    
    ## gráfico
    plotlyOutput(outputId = 'uf'),
    ## texto descritivo do gráfico
    textOutput(outputId = "descUf"),
    
    ## gráfico
    plotlyOutput(outputId = 'atendida'),
    ## texto descritivo do gráfico
    textOutput(outputId = "descAtendida"),
    
    ## gráfico
    plotlyOutput(outputId = 'atendidaAno'),
    ## texto descritivo do gráfico
    textOutput(outputId = "descAtendidaAno")
  )
  
)

## back-end (o que o sistema irá executar para retornar para o usuário, front-end)
server = function(input, output, session) {
  output$data <- renderPlotly({ grafico_data  })
  output$uf   <- renderPlotly({ grafico_uf})
  output$atendida    <- renderPlotly({ grafico_atendida})
  output$atendidaAno <- renderPlotly({ grafico_atendida_ano})
  
  output$descData <- renderText({"Gráfico com a quantidade de reclamações feitas por mês-ano"})
  output$descUf   <- renderText({"Gráfico com a quantidade de reclamações feitas por UF"})
  output$descAtendida    <- renderText({"Gráfico com a quantidade de reclamações atendidas e não atendidas"})
  output$descAtendidaAno <- renderText({"Gráfico com a quantidade de reclamações atendidas e não atendidas por Ano"})
  
}


shinyApp(ui, server)
