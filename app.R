#install.packages("shiny")

library(shiny)

#### 3. Criando app web com R ####

## exemplo de app shiny
#runExample('02_text')

## front-end (tela que será mostrada para o usuário)
app <- shinyApp(
  ui = fluidPage(
        titlePanel("Hello Shiny!"),
        
        verbatimTextOutput("summary"),
      
      ),


    ## back-end (o que o sistema irá executar para retornar para o usuário, front-end)
    server = function(input, output, session) {
      output$summary <- renderPrint({
        seq(1:100)
      })
    }
)

#shinyApp(ui, server)

runApp(app)
