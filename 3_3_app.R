library(shiny)
library(data.table)
library(dplyr)

#### 3. Criando app web com R ####

## lendo arquivos 
dados <- fread('dados_limpos.csv')

## front-end (tela que será mostrada para o usuário)
ui = fluidPage(
  ## título da página
  titlePanel("Hello Shiny!"),
  
  ## impressão dos dados gerados no 'server'
  verbatimTextOutput("listaNumeros")
  
)

## back-end (o que o sistema irá executar para retornar para o usuário, front-end)
server = function(input, output, session) {
  
  ## manipulando/filtrando dados 
  dados_2 <- reactive({
    dados %>% 
      filter(UF %in% c('DF','SC','GO'))
  })
  
  ## retorna lisa de UF 
  output$listaNumeros <- renderPrint({
    unique(dados_2()$UF)
  })
}

## executa o serviço web
shinyApp(ui, server)
