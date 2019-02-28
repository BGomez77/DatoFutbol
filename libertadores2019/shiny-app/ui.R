library(shiny)
library(ggplot2)
library(plotly)

data <- read.csv("data_liberta_2011_18.csv")

fluidPage(
        
        titlePanel("¿Pasaron la fase de grupos?"),
        
        fluidRow(
                column(4,
                       selectInput('x', 'Elige un club [se marcará en negro]', levels(data$Club), selected="river plate")
                )
        ),
        br(),
        "Fase de grupos ediciones 2011-2018 Copa Libertadores",
        mainPanel(
                plotlyOutput('plot'),
                width=16
        )
)