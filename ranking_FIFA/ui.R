library(shiny)
library(ggplot2)
library(plotly)

data <- read.csv("ranking_shiny.csv")

fluidPage(
        
        titlePanel("Ranking FIFA histórico"),
        
        fluidRow(
                column(3,
                       selectInput('x', 'Selección 1', levels(data$Team), selected="Chile")
                ),
                column(3, offset = 1,
                       selectInput('y', 'Selección 2', levels(data$Team), selected="Peru")
                ),
                column(3, offset= 1,
                       selectInput('z', 'Selección 3', levels(data$Team), selected="Ecuador")
                )
        ),
        
        mainPanel(
                sliderInput('year', label = "Año inicio", min = 1993, max = 2017, value = 2010),
                plotlyOutput('plot'),
                width=14
        )
)

