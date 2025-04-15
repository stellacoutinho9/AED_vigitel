#Pacotes
library(shiny)
library(ggplot2)
library(colourpicker)

#Base de dados
# Importando base
dados <- read_excel("Vigitel-2023-peso-rake.xlsx")
dados <- dados |> 
  mutate(across(where(is.character), ~ iconv(.x, from = "", to = "UTF-8", sub = " ")))
names(dados) <- iconv(names(dados), from = "latin1", to = "UTF-8")

# Selecionando e renomeando variáveis de interesse
df1 <- dados %>% filter(cidade == 21) %>%
  select(peso = q9, idade = q6, altura = q11, dias_atividade = q45)
df1[df1 == 777 | df1 == 888] <- NA
head(df1)

# Codigo shiny 
ui <- fluidPage(
  titlePanel("Dashboard Interativo"),
  
  sidebarLayout(
    sidebarPanel(
      # Seletor para escolher a variavel
      selectInput("var", "Selecione a variavel:", 
                  choices = names(df1)),
      
      # Seletor de cor
      colourInput("lineColor", "Escolha a cor da linha:", value = "blue"),
      
      # Ajuste do eixo X
      numericInput("xMin", "Limite inferior do eixo X:", value = 1),
      numericInput("xMax", "Limite superior do eixo X:", value = 100),
      
      # Ajuste do eixo Y
      numericInput("yMin", "Limite inferior do eixo Y:", value = 0),
      numericInput("yMax", "Limite superior do eixo Y:", value = 100)
    ),
    
    mainPanel(
      # Grafico de linha
      plotOutput("linePlot")
    )
  )
)

# Definir o servidor
server <- function(input, output) {
  
  output$linePlot <- renderPlot({
    # Variavel selecionada
    selectedVar <- input$var
    
    # Dados a serem plotados
    plotData <- df1[[selectedVar]]
    
    # Grafico com limites ajustados e cor selecionada
    ggplot(data.frame(x = 1:length(plotData), y = plotData), aes(x = x, y = y)) +
      geom_line(color = input$lineColor) +
      scale_x_continuous(limits = c(input$xMin, input$xMax)) +
      scale_y_continuous(limits = c(input$yMin, input$yMax)) +
      labs(title = paste("Grafico de Linha de", selectedVar),
           x = "Indice",
           y = selectedVar)
  })
}

# Rodar a aplicação
shinyApp(ui = ui, server = server)
