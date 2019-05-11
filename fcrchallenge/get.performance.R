get.performance <- function(AllEvents, Players.name="Nikita Parris"){
        
        counter <- 1
        for (n in Players.name)
        {
                
                p.id <- AllEvents %>%
                        filter(player.name==n) %>%
                        select(player.id) %>%
                        unique() %>%
                        as.numeric()
                
                info.played <- AllEvents %>% 
                        get.minutesplayed() %>%
                        filter(player.id==p.id)
                
                num.matches <- nrow(info.played)
                minutes.played <- sum(info.played$minutes.played)
                
                shots <- AllEvents %>% 
                        filter(player.name == n & type.name == "Shot")  
                
                goals <- shots %>%
                        filter(shot.outcome.name == "Goal")
                
                passes <- AllEvents %>% 
                        filter(player.name == n & type.name == "Pass")
                
                dribbles <- AllEvents %>% 
                        filter(player.name == n & type.name == "Dribble")
                
                #add Pressures
                
                
                pn <- as.data.frame(n)
                
                temp <- pn %>%
                        mutate(player.id = p.id,
                               num.matches = num.matches,
                               minutes.played = minutes.played,
                               num.goals = nrow(goals),
                               penalty_goals = sum(goals$shot.type.name=="Penalty"),
                               xG = round(sum(shots[which(shots$shot.type.name=="Open Play"), "shot.statsbomb_xg"]), 2),
                               goals.90 = round(num.goals/minutes.played*90, 1),
                               num.shots = sum(shots$shot.type.name=="Open Play"),
                               shots_per_goal = round(num.shots/num.goals, 1),
                               eff_percent = round(num.matches/num.shots*100, 1),
                               xG_per_shot = round(xG/num.shots, 2),
                               xG90 = round(xG/num.matches, 2),
                               complete_pass_prop = round((nrow(passes) - sum(table(passes$pass.outcome.name))) / nrow(passes), 2) ,
                               pass_median_length = round(median(passes$pass.length), 1),
                               shot_assist = sum(table(passes$pass.shot_assist)),
                               goal_assist = sum(table(passes$pass.goal_assist)),
                               shot_assist90 = round(shot_assist/num.matches, 2),
                               goal_assist90 = round(goal_assist/num.matches, 2),
                               complete_dribbles = sum(dribbles$dribble.outcome.name=="Complete"),
                               complete_dribbles90 = round(complete_dribbles/num.matches, 2)
                        )
                
                if (counter == 1)
                {
                        df <- temp
                } else { df = bind_rows(df, temp) }
                counter <- 1 + counter 
        }
        
        return(df)
        
}