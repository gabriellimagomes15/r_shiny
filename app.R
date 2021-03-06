library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)

#install.packages("shinyWidgets")
library(shinyWidgets)

#### 5. Inserindo conteudo na app  ####

dados <- fread('dados_limpos.csv',encoding = 'UTF-8')

## front-end (tela que ser� mostrada para o usu�rio)
ui = fluidPage(
    ## t�tulo da p�gina
    titlePanel("Dashboard PROCON"),
    
    mainPanel(
        #awesomeCheckboxGroup(inputId = "select_UF",label =  "Estado:",inline = TRUE,
        #           choices = c('TODOS',unique(dados$UF)),selected = 'TODOS'),
        
        ## caixa de sele��o 
        checkboxGroupInput(inputId = "select_UF",label =  "Estado:",
                           choices = c('TODOS',unique(dados$UF)),selected = 'TODOS'),
        
        #verbatimTextOutput(outputId = 'dados'),
        
        ## calend�rio para selecionar per�do
        dateRangeInput(inputId = "data_abertura",label =  "Data Abertura:",
                       start = min(as.Date(dados$DataAbertura)),#"2001-01-01",
                       end   = max(as.Date(dados$DataAbertura))), #"2010-12-31"),
        #verbatimTextOutput(outputId = 'anos'),
        
        ## sele��o de descri��oo de assunto
        selectizeInput(inputId = "assunto",label =  "Descri��o Assunto:",
                       choices = c('TODOS', unique(dados$DescricaoAssunto)),
                       selected = 'TODOS',multiple = T,options = list(maxItems = 5)),
        
        #c("Cylinders" = "cyl","Transmission" = "am","Gears" = "gear")),
        
        ## sele��o de ano (SABER MAIS)
        #sliderInput("unifRange", "Range",
        #           min = as.numeric(min(dados$anocalendario)), 
        #          max = as.numeric(max(dados$anocalendario)), 
        #         value = c(as.numeric(min(dados$anocalendario)), 
        #                  as.numeric(max(dados$anocalendario))),sep = ''),
        
        ## gr�fico de linhas
        plotlyOutput(outputId = 'data',width = '100%'),
        
        ## texto descritivo do gr�fico de linhas
        textOutput(outputId = "descData"),
        
        ## gr�fico
        plotlyOutput(outputId = 'uf'),
        ## texto descritivo do gr�fico
        textOutput(outputId = "descUf"),
        
        ## gr�fico
        plotlyOutput(outputId = 'atendida'),
        ## texto descritivo do gr�fico
        textOutput(outputId = "descAtendida"),
        
        ## gr�fico
        plotlyOutput(outputId = 'atendidaAno'),
        ## texto descritivo do gr�fico
        textOutput(outputId = "descAtendidaAno"),
        
        
    )
    
)

## back-end (o que o sistema ir� executar para retornar para o usu�rio, front-end)
server = function(input, output, session) {
    
    dados_selecionados <- reactive({
        ## filtro UF
        if (!'TODOS' %in% input$select_UF  ){
            dados <- dados %>% filter(UF %in% input$select_UF)
        }
        ## filtro ASSUNTO
        if(!'TODOS' %in% input$assunto){
            dados <- dados %>% filter(DescricaoAssunto %in% input$assunto)
        }
        
        ## filtro DATA
        dados <- dados %>% filter(as.Date(DataAbertura) >= input$data_abertura[1] & 
                                      as.Date(DataAbertura) <= input$data_abertura[2])
        dados
        
    })
    
    ## gr�fico de linhas ano-mes
    output$data <- renderPlotly({
        ano_mes <- data.frame(table(format(as.Date(dados_selecionados()$DataAbertura),
                                           '%Y-%m'))) %>% rename(Data = Var1, Qtd=Freq)
        ano_mes$Data <- as.Date(paste(ano_mes$Data,'01',sep = '-'))
        
        ggplotly(
            ggplot(data = ano_mes, aes(Data, Qtd)) +
                geom_line(group = 1) +
                theme_bw() + 
                theme(axis.text.x = element_text(angle = 45,hjust = 1))+
                ggtitle('Quantidade de Reclama��es por Ano-M�s') +
                scale_x_date(date_labels = '%b-%Y',breaks = '6 month')
        )  
    })
    
    ## gr�fico UF
    output$uf   <- renderPlotly({ 
        ggplotly(
            data.frame(table(dados$UF)) %>% rename(UF = Var1,Qtd = Freq) %>%
                ggplot(aes(x = reorder(UF,Qtd),y = Qtd,
                           text=paste(" UF:", UF, "<br>", "QTD:",Qtd))) + 
                geom_bar(fill = 'blue',stat = 'identity') +  
                coord_flip() +
                xlab('UF') + #ylab('Quantidade') + 
                theme_bw() + 
                ggtitle('Quantidade de Reclama��es por UF'),
            tooltip = "text"
        )
    })
    
    ## gr�fico atendida
    output$atendida    <- renderPlotly({ 
        ggplotly(
            ggplot(dados) +
                geom_bar(aes(Atendida),fill = c('red','green'),stat = 'count') +
                ylab('Quantidade') + 
                theme_bw() + 
                ggtitle('Quantidade de Chmados Atendidos')
        )  
    })
    
    ## gr�fico atendida por ano
    output$atendidaAno <- renderPlotly({ 
        ggplotly(
            data.frame(table(dados$anocalendario,dados$Atendida)) %>%
                rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
                ggplot() +
                geom_bar(aes(x = Ano,y = Qtd, fill = Atendida),
                         stat = 'identity',position = position_dodge2()) +
                theme_bw() + 
                ggtitle('Quantidade de Reclama��es Atendidas(n�o) por Ano')
        )
    })
    
    ## retornando texto para cada campo em espec�fico
    output$descData <- renderText({
        paste("Gr�fico com a quantidade de reclama��es feitas entre:",
              min(dados_selecionados()$DataAbertura),'-',
              max(dados_selecionados()$DataAbertura))
    })
    output$descUf   <- renderText({
        estados <- paste(unique(dados_selecionados()$UF),collapse = ', ')
        paste("Gr�fico com a quantidade de reclama��es feitas por UF: ",estados)
        
    })
    output$descAtendida    <- renderText({"Gr�fico com a quantidade de reclama��es atendidas e n�o atendidas"})
    
    output$descAtendidaAno <- renderText({"Gr�fico com a quantidade de reclama��es atendidas e n�o atendidas por Ano"})
    
}


#shinyApp(ui, server)
runApp(list(ui = ui, server = server),launch.browser = TRUE)
