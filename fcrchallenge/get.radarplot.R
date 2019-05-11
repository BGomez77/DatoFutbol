get.radarplot <- function(performance, Players.name="*Nikita Parris"){
        p <- plot_ly(
                type = 'scatterpolar',
                fill = 'toself'
        )
        
        for (i in Players.name)
        {
                p <-  add_trace(p, 
                                r = performance[which(performance$player==i), "value"],
                                theta = performance[which(performance$player==i), "key"],
                                name = i
                )
        } 
        
        p <- layout(p,
                    polar = list(
                            radialaxis = list(
                                    visible = T,
                                    range = c(0, max(performance$value))
                            )
                    ),
                    showlegend = T
        )
        
        return(p)
}