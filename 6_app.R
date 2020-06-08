library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)

#install.packages("shinyWidgets")
library(shinyWidgets)

#install.packages('shinydashboard')
library(shinydashboard)

#### 5. Inserindo conteudo na app  ####

dados <- fread('dados_limpos.csv',encoding = 'UTF-8')

cabecalho     <- dashboardHeader(title = "Dashboard PROCONs",
                                 titleWidth = '250px')
barra_lateral <- dashboardSidebar(width = '250px')
corpo_pagina  <- dashboardBody()

ui = dashboardPage(header  = cabecalho,
                   sidebar = barra_lateral,
                   body    = corpo_pagina)

## front-end (tela que será mostrada para o usuário)
ui2 = fluidPage(
  ## título da página
  titlePanel("Dashboard PROCON"),
  
  
  
  mainPanel(
    #awesomeCheckboxGroup(inputId = "select_UF",label =  "Estado:",inline = TRUE,
    #           choices = c('TODOS',unique(dados$UF)),selected = 'TODOS'),
    
    ## caixa de seleção 
    checkboxGroupInput(inputId = "select_UF",label =  "Estado:",
                       choices = c('TODOS',unique(dados$UF)),selected = 'TODOS'),
    
    #verbatimTextOutput(outputId = 'dados'),
    
    ## calendário para selecionar período
    dateRangeInput(inputId = "data_abertura",label =  "Data Abertura:",
                   start = min(as.Date(dados$DataAbertura)),#"2001-01-01",
                   end   = max(as.Date(dados$DataAbertura))), #"2010-12-31"),
    #verbatimTextOutput(outputId = 'anos'),
    
    ## seleção de descrição de assunto
    selectizeInput(inputId = "assunto",label =  "Descrição Assunto:",
                   choices = c('TODOS', unique(dados$DescricaoAssunto)),
                   selected = 'TODOS',multiple = T,options = list(maxItems = 5)),
    
    #c("Cylinders" = "cyl","Transmission" = "am","Gears" = "gear")),
    
    ## seleção de ano
    #sliderInput("unifRange", "Range",
    #           min = as.numeric(min(dados$anocalendario)), 
    #          max = as.numeric(max(dados$anocalendario)), 
    #         value = c(as.numeric(min(dados$anocalendario)), 
    #                  as.numeric(max(dados$anocalendario))),sep = ''),
    
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
    textOutput(outputId = "descAtendidaAno"),
    
    
  )
  
)

## back-end (o que o sistema irá executar para retornar para o usuário, front-end)
server = function(input, output, session) {
  
  
  
  dados_2 <- reactive({
    #print(input$select_UF)
    
    if (!'TODOS' %in% input$select_UF  ){
      dados <- dados %>% filter(UF %in% input$select_UF)
    }#else
    #dados
    #cat(file=stderr(), "select_UF: ", unique(dados$UF),"\n")
    if(!'TODOS' %in% input$assunto){
      dados <- dados %>% filter(DescricaoAssunto %in% input$assunto)
    }
    
    dados <- dados %>% filter(as.Date(DataAbertura) >= input$data_abertura[1] & 
                                as.Date(DataAbertura) <= input$data_abertura[2])
    
    
    # cat(file=stderr(), "unique: ", unique(dados$DescricaoAssunto),"\n")
    
    dados
    
  })
  #output$UF    <- renderPrint(unique(dados_2()$UF))
  #output$anos    <- renderPrint(input$data_abertura[1] )
  
  #output$dados <- renderPrint(paste(input$select_UF, length(dados_2()$UF))) 
  
  output$data <- renderPlotly({
    ano_mes <- data.frame(table(format(as.Date(dados_2()$DataAbertura),
                                       '%Y-%m'))) %>% rename(Data = Var1, Qtd=Freq)
    ano_mes$Data <- as.Date(paste(ano_mes$Data,'01',sep = '-'))
    
    ggplotly(
      ggplot(data = ano_mes, aes(Data, Qtd)) +
        geom_line(group = 1) +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45,hjust = 1))+
        ggtitle('Quantidade de Reclamações por Ano-Mês') +
        scale_x_date(date_labels = '%b-%Y',breaks = '6 month')
    )  
    
  })
  output$uf   <- renderPlotly({ 
    ggplotly(
      data.frame(table(dados$UF)) %>% rename(UF = Var1,Qtd = Freq) %>%
        ggplot(aes(x = reorder(UF,Qtd),y = Qtd,
                   text=paste(" UF:", UF, "<br>", "QTD:",Qtd))) + 
        geom_bar(fill = 'blue',stat = 'identity') +  
        coord_flip() +
        xlab('UF') + #ylab('Quantidade') + 
        theme_bw() + 
        ggtitle('Quantidade de Reclamações por UF'),
      tooltip = "text"
    )
  })
  
  output$atendida    <- renderPlotly({ 
    ggplotly(
      ggplot(dados) +
        geom_bar(aes(Atendida),fill = c('red','green'),stat = 'count') +
        ylab('Quantidade') + 
        theme_bw() + 
        ggtitle('Quantidade de Chmados Atendidos')
    )  
  })
  output$atendidaAno <- renderPlotly({ 
    ggplotly(
      data.frame(table(dados$anocalendario,dados$Atendida)) %>%
        rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
        ggplot() +
        geom_bar(aes(x = Ano,y = Qtd, fill = Atendida),
                 stat = 'identity',position = position_dodge2()) +
        theme_bw() + 
        ggtitle('Quantidade de Reclamações Atendidas(não) por Ano')
    )
  })
  
  output$descData <- renderText({"Gráfico com a quantidade de reclamações feitas por mês-ano"})
  output$descUf   <- renderText({"Gráfico com a quantidade de reclamações feitas por UF"})
  output$descAtendida    <- renderText({"Gráfico com a quantidade de reclamações atendidas e não atendidas"})
  output$descAtendidaAno <- renderText({"Gráfico com a quantidade de reclamações atendidas e não atendidas por Ano"})
  
}


shinyApp(ui, server)
