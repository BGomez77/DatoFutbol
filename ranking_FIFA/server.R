library(shiny)
library(ggplot2)
library(plotly)
library(lubridate)
library(zoo)

function(input, output) {
        w<- read.csv("ranking_shiny.csv", stringsAsFactors = F)
        dataset <- reactive({
                w[which( (w$Team==input$x | w$Team==input$y | w$Team==input$z) & (w$ye >= input$year)), c('Team', 'Rank','Date')]
        })
        
        output$plot <- renderPlotly({
                q <- dataset()
                if( (max(q$Rank)-min(q$Rank)) <= 30 ) {b <- 1} else {b <- round((max(q$Rank)-min(q$Rank))/30, 0) }

                q$Date <- as.yearmon(q$Date)
                #q$Date <- as.Date(q$Date, format='%Y-%m')
                q$Details <- paste('<br>Team: ', q$Team, '<br>Rank: ', q$Rank, '<br>Date: ', q$Date)
                p<- ggplot(aes(y=Rank, x=Date, colour=Team, label=Details), data=q, stat="density") +
                        geom_line(size=0.5) + 
                        geom_point(size=1.2)
                p<- p + 
                    scale_y_continuous(trans="reverse", limits=c(max(q$Rank),0), breaks=seq(max(q$Rank), 1, -b), expand = c(0.01,0.01)) +
                    scale_x_continuous(breaks=seq(min(year(q$Date)), max(year(q$Date)), 1), limits=c(min(year(q$Date)), max(year(q$Date))+1), expand = c(0,0)) +
                    theme_bw() +
                    labs(x="Año", y="Ranking FIFA") +
                    scale_colour_brewer(palette = "Set1") +
                    theme(legend.position="top",
                          legend.title = element_blank(),
                          axis.text.x = element_text(angle=90, vjust=1, size=8),
                          axis.title.x = element_text(face="bold", size=12),
                          axis.title.y = element_text(face="bold", size=12)) 
                p <- ggplotly(p, tooltip="label")
                
        })                 
}
