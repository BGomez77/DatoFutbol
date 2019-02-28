library(shiny)
library(ggplot2)
library(plotly)

function(input, output) {
        w<- read.csv("data_liberta_2011_18.csv", stringsAsFactors = F)
        dataset <- reactive({
                w[which(w$Club==input$x), ]
        })
        
        output$plot <- renderPlotly({
                q2 <- dataset()
                q <- w
                q$Detalle <- paste('<br>Club: ', q$Club, '<br>Año: ', q$Year, '<br>Grupo: ', q$Group,
                                   '<br>Valor $ (centrado): ', round(q$Value_centered, 1), '[Millones EUR]',
                                   '<br>Prob. pasar: ', round(q$real_p, 1), ' %', '<br>Puntos: ', q$PTS,
                                   '<br>Posición: ', q$Pos, '<br>Pasó fase grupos: ', q$clas_factor)

                q2$Detalle <- paste('<br>Club: ', q2$Club, '<br>Año: ', q2$Year, '<br>Grupo: ', q2$Group, 
                                    '<br>Valor $ (centrado): ', round(q2$Value_centered, 1), '[Millones EUR]',
                                   '<br>Prob. pasar: ', round(q2$real_p, 1), ' %', '<br>Puntos: ', q2$PTS, 
                                   '<br>Posición: ', q2$Pos, '<br>Pasó fase grupos: ', q2$clas_factor)
                                
                p <- ggplot(q, aes(x=Value_centered, y=real_p, col=clas_factor, label=Detalle)) +
                        geom_point(alpha = 4/10, size=2, shape=16) +
                        geom_point(data=q2, size=2, shape=21, col="black") +
                        labs(x = "Valor centrado [Millones de EUR]\nrespecto al promedio de su grupo", 
                             y = "Probabilidad de clasificar [%]") +
                        theme_bw() +
                        theme(plot.title = element_text(hjust = -0.45, vjust=2.12)) +
                        scale_y_continuous(breaks=seq(0, 100, 5)) +
                        scale_x_continuous(breaks=seq(-60, 70, 10)) +
                        scale_colour_brewer(palette="Set1")
                
                p <- ggplotly(p, tooltip="label")  %>%
                        layout(legend = list(orientation = "h", x = 0.4, y = 1), margin = list(b=90, l=50))
                
        })                 
}