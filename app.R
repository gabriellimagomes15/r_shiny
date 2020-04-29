#install.packages("shiny")

library(shiny)

#### 3. Criando app web com R ####

## exemplo de app shiny
#runExample('02_text')

## front-end (tela que será mostrada para o usuário)
ui = fluidPage(
      ## título da página
      titlePanel("Hello Shiny!"),
      
      ## impressão dos dados gerados no 'server'
      verbatimTextOutput("listaNumeros")
)

## back-end (o que o sistema irá executar para retornar para o usuário, front-end)
server = function(input, output, session) {
  
  ## geração de uma lista de números que serão impressos na tela do usuário
  output$listaNumeros <- renderPrint({
    seq(1:100)
  })
}


shinyApp(ui, server)

#runApp(app)

