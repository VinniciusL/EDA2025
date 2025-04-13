---
title: "Análise Exploratória de Dados - Dados de Vídeos"
author: "Vinnicius Pereira"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output: pdf_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```
library(readxl)
library(ggplot2)
library(summarytools)
library(GGally)
library(ggpubr)
library(mice)
library(shiny)
```

## Introdução

A base de dados utilizada neste projeto se chama `dados_videos.xlsx` e contém informações sobre vídeos curtos e suas métricas de engajamento. A escolha se deu por apresentar variáveis numéricas suficientes para a análise e por conter dados faltantes, o que é ideal para validar técnicas de imputação. Os resultados esperados incluem uma descrição estatística da base, visualização de padrões de correlação entre variáveis e avaliação da normalidade dos dados, além de aplicar técnicas de imputação e desenvolver um dashboard interativo com Shiny.

## Leitura da Base de Dados

```{r leitura-dados}
dados <- read_excel("dados_videos.xlsx")
head(dados)
summary(dados)
```

## Estatísticas Descritivas

```{r descr}
descr(dados[, c("duracao_video", "qtd_visualizacoes", "qtd_curtidas", 
                "qtd_compartilhamento", "qtd_downloads", "qtd_comentarios")])
```

## Matriz de Dispersão

```{r scatter-matrix}
GGally::ggpairs(dados[, c("duracao_video", "qtd_visualizacoes", "qtd_curtidas", 
                          "qtd_compartilhamento", "qtd_downloads", "qtd_comentarios")])
```

## Histogramas por Variável

```{r histogramas}
variaveis <- c("duracao_video", "qtd_visualizacoes", "qtd_curtidas", 
               "qtd_compartilhamento", "qtd_downloads", "qtd_comentarios")

for (var in variaveis) {
  print(
    ggplot(dados, aes_string(x = var)) +
      geom_histogram(bins = 10, fill = "skyblue", color = "black") +
      labs(title = paste("Histograma de", var), x = var, y = "Frequência")
  )
}
```

## Q-Q Plot por Variável

```{r qqplot}
for (var in variaveis) {
  print(
    ggqqplot(dados[[var]], title = paste("Q-Q Plot de", var))
  )
}
```

## Teste de Normalidade (Shapiro-Wilk)

```{r shapiro}
shapiro_resultados <- sapply(dados[variaveis], function(x) shapiro.test(x)$p.value)
shapiro_resultados
```

## Completude dos Dados

```{r completude}
completude <- sapply(dados, function(x) sum(!is.na(x)) / length(x))
completude
```

## Imputação de Dados com MICE

```{r imputacao}
imp <- mice(dados, m = 5, method = "pmm", seed = 123)
dados_imputados <- complete(imp)
head(dados_imputados)
```

## Dashboard com Shiny

```{r shiny-dashboard, eval=FALSE}
library(shiny)

ui <- fluidPage(
  titlePanel("Dashboard de Vídeos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variavel", "Escolha a variável:", choices = variaveis),
      colourInput("cor", "Cor da linha:", value = "blue"),
      sliderInput("xlim", "Limite do eixo X:", min = 0, max = 100, value = c(0, 100)),
      sliderInput("ylim", "Limite do eixo Y:", min = 0, max = 1000000, value = c(0, 500000))
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
)

server <- function(input, output) {
  output$grafico <- renderPlot({
    ggplot(dados, aes_string(x = "duracao_video", y = input$variavel)) +
      geom_line(color = input$cor) +
      coord_cartesian(xlim = input$xlim, ylim = input$ylim) +
      labs(title = paste("Gráfico de Linha para", input$variavel),
           x = "Duração do Vídeo", y = input$variavel)
  })
}

shinyApp(ui = ui, server = server)
```

## Referências

- Fonte dos dados: base simulada `dados_videos.xlsx`
- Pacotes utilizados: `ggplot2`, `summarytools`, `GGally`, `ggpubr`, `mice`, `shiny`
- Curso: Análise Exploratória de Dados com R
