getwd()
setwd('D:/Projetos/alura/r_shiny')


library(data.table)
library(dplyr)

#### 1. preparando os dados ####
dados <- fread('reclamacao.csv',encoding = 'UTF-8')
unique(dados$DescricaoProblema)

## verificando tipos/valores todas as colunas
summary(dados)

## nome das colunas
colnames(dados)

## excluindo colunas irrelevantes para a base
reclamacao <- dados %>%
                select(-X.1, -V1)

## verificando anos
table(reclamacao$anocalendario)
unique(reclamacao$regiao)
reclamacao <- reclamacao %>%
                filter(regiao != 'N/D')
unique(reclamacao$UF)
unique(reclamacao$Tipo)

unique(reclamacao$Atendida)
reclamacao$Atendida <- gsub(pattern = 'S|Siim',replacement = 'sim',x = reclamacao$Atendida)
reclamacao$Atendida <- gsub(pattern = 'N|nAoo|nao',replacement = 'não',x = reclamacao$Atendida)
reclamacao          <- reclamacao %>% filter(Atendida != '')
unique(reclamacao$Atendida)

unique(reclamacao$SexoConsumidor)  
reclamacao$SexoConsumidor <- gsub(pattern = 'N|NULL',replacement = 'N/I',
                                  x = reclamacao$SexoConsumidor)
unique(reclamacao$SexoConsumidor)

reclamacao <- reclamacao %>% 
                filter(!SexoConsumidor %in% c(''))
unique(reclamacao$SexoConsumidor)

#### 2. Construindo Gráficos interativos ####
#install.packages('plotly')
library(plotly)
library(ggplot2)

### barra vertical (Atendida) ####
grafico_atendida <- ggplot(reclamacao) +
                      geom_bar(aes(Atendida),fill = c('red','green'),stat = 'count') +
                      ylab('Quantidade') + 
                      theme_bw() + 
                      ggtitle('Quantidade de Chmados Atendidos')

grafico_atendida

## interatividade no gráfico
grafico_atendida <- ggplotly(grafico_atendida)
grafico_atendida

### barra horizontal (UF) ####

## gráfico desordenado
ggplot(reclamacao) + 
  geom_bar(aes(UF),stat = 'count',) 

## gráfico ordenado
grafico_uf <- data.frame(table(reclamacao$UF)) %>% rename(UF = Var1,Qtd = Freq) %>%
              ggplot(aes(x = reorder(UF,Qtd),y = Qtd,
                         text=paste(" UF:", UF, "<br>", "QTD:",Qtd))) + 
                geom_bar(fill = 'blue',stat = 'identity') +  
                coord_flip() +
                xlab('UF') + #ylab('Quantidade') + 
                theme_bw() + 
                ggtitle('Quantidade de Reclamações por UF')

grafico_uf
grafico_uf <- ggplotly(grafico_uf,tooltip = "text")
grafico_uf

### gráfico de linha (dataArq) ####

## gráfico de linhas data completa(visualizão ruim)
data.frame(table(as.Date(reclamacao$DataArquivamento))) %>%
  ggplot(aes(Var1,Freq)) +
    geom_line(group = 1)

## gráfico de linhas data ano-mes
grafico_data <- data.frame(table(format(as.Date(reclamacao$DataArquivamento),
                                        '%Y-%m'))) %>% rename(Data = Var1, Qtd=Freq)%>%
                ggplot(aes(Data, Qtd)) +
                  geom_line(group = 1) +
                  theme(axis.text.x = element_text(angle = 45))
                  theme_bw() + 
                  ggtitle('Quantidade de Reclamações por Ano-Mês')


grafico_data

grafico_data <- ggplotly(grafico_data)
grafico_data

### gráfico de barra 2 variaveis (Atendida-ANO) ####

## tabela de frequência Ano-Atendida

## gráfico de barras com 2 variáveis
grafico_atendida_ano <- data.frame(table(reclamacao$anocalendario,reclamacao$Atendida)) %>%
                        rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
                        ggplot() +
                          geom_bar(aes(x = Ano,y = Qtd, fill = Atendida),
                                   stat = 'identity',position = position_dodge2()) +
                          theme_bw() + 
                          ggtitle('Quantidade de Reclamações Atendidas(não) por Ano')

grafico_atendida_ano

## interatividade no gráfico 
grafico_atendida_ano <- ggplotly(grafico_atendida_ano)

grafico_atendida_ano


