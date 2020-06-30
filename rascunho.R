getwd()
setwd('D:/Projetos/alura/r_shiny')

library(data.table)
library(dplyr)
#### 1. preparando os dados ####
dados <- fread('reclamacao.csv',encoding = 'UTF-8')
unique(dados$DescricaoProblema)
#dados[dados$regiao == '','regiao'] <- dados[dados$Regiao != '','Regiao']
#dados$Regiao <- NULL
#dados$SexoConsumidor <- gsub(pattern = 'feminino',replacement = 'F',x = dados$SexoConsumidor)
#dados$SexoConsumidor <- gsub(pattern = 'masculino',replacement = 'M',x = dados$SexoConsumidor)
#fwrite(dados,'reclamacao.csv',row.names = F)

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

reclamacao <- reclamacao %>% filter(!SexoConsumidor %in% c(''))
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
grafico_uf <- data.frame(table(reclamacao$UF)) %>%  
                ggplot() + 
                  geom_bar(aes(x = reorder(Var1,Freq),y = Freq),stat = 'identity') +  
                  coord_flip()

grafico_uf
grafico_uf <- ggplotly(grafico_uf)
grafico_uf

### gráfico de linha (dataArq) ####

## tabela de frequencia com data completa
#qtd_abertura <- 

#View(qtd_abertura)

## gráfico de linhas data completa(visualizão ruim)
data.frame(table(as.Date(reclamacao$DataArquivamento))) %>%
  ggplot(aes(Var1,Freq)) +
  geom_line(group = 1)

## tabela de frequencia somente com o ano-mes
#qtd_abertura <- 
#View(qtd_abertura)

## gráfico de linhas data ano-mes
grafico_data <- data.frame(table(format(as.Date(reclamacao$DataArquivamento),
                                        '%Y-%m'))) %>%
                  ggplot(aes(Var1,Freq)) +
                    geom_line(group = 1)
grafico_data

grafico_data <- ggplotly(grafico_data)
grafico_data

### grafico de barra 2 variaveis (Atendida-ANO) ####

## tabela de frequência Ano-Atendida
#qtd_atendida_ano <- 
#View(qtd_atendida_ano)

## alterando o nome das colunas
#colnames(qtd_atendida_ano) <- c('ano','atendida','qtd')

## gráfico de barras com 2 vari?veis
grafico_atendida_ano <- data.frame(table(reclamacao$anocalendario,reclamacao$Atendida)) %>%
                          ggplot() +
                            geom_bar(aes(x = Var1,y = Freq, fill = Var2),
                                     stat = 'identity',position = position_dodge2())
grafico_atendida_ano

## interatividade no gr?fico 
grafico_atendida_ano <- ggplotly(grafico_atendida_ano)

#### 3. Criando app web com R ####
