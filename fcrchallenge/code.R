setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(soccermatics)
library(tidyverse)
library(StatsBombR)
library(plotly)

## Don't forget to load custom functions "get.performance" and "get.radarplot"

## Loading data
events <- StatsBombFreeEvents()   # get all free events from Statsbomb
events2 <- events %>% filter(competition_id==37) # select only WFC events
events3 <- cleanlocations(events2) # parse location data
events4 <- formatelapsedtime(events3)

# transform x,y-coords to real-world units for compatability with soccermatics
events5 <- events4 %>% 
           soccermatics::soccerTransform(method = "statsbomb")

# Nikita shotmap for multiple matches
events5 %>% 
        filter(player.name == "Nikita Parris") %>% 
        soccermatics::soccerShotmap(adj=F,
                                    theme = "dark",
                                    title = "Nikita Parris",
                                    subtitle = "FA Women's Super League 2018/19")


#data <- get.performance(events5)


## Searching players to replace Nikita

# Criterion N°1: Number of goals
goal_ranking <- events5 %>%
        filter(shot.outcome.name=="Goal") %>%
        group_by(player.name, team.name) %>%
        summarise(num_goals=n()) %>%
        arrange(desc(num_goals))

# 1) Vivianne Miedema
# 2) Courtney Sweetman-Kirk
# 3) Bethany England
# 4) Jordan Nobbs

# Fara Williams, Danielle van de Donk y Kim Little had not considered cause they are Midfielders
# Neither Georgia Stanway cause she belongs to Manchester City


# Second criterion: xG
xG_ranking <- events5 %>%
        filter(shot.type.name=="Open Play") %>%
        group_by(player.name, team.name) %>%
        summarise(sum_xG=round(sum(shot.statsbomb_xg), 2)) %>%
        arrange(desc(sum_xG))

# 5) Francesca Kirby
# 6) Brooke Chaplen
# 7) Jane Ross

# Aditional criteria: Goal assist [see plot at final]
# 8) Ellen White

data <- get.performance(events5, c("Nikita Parris", "Vivianne Miedema", 
                                   "Courtney Sweetman-Kirk", "Bethany England",
                                   "Jordan Nobbs", "Francesca Kirby", 
                                   "Brooke Chaplen", "Jane Ross", "Ellen White"))

data <- data %>% as.data.frame() %>% arrange(desc(goals.90, xG90, goal_assist90))
data <- data[1:7, ]
# Last 2 position OUT


##vIZ
data2 <- data[, c(1, 8, 13:14, 18, 19, 21)]
data3 <- gather(data2, "key", "value", -1) %>%
         rename(player=n) %>%
         mutate(player=str_replace(player, "Nikita Parris", "*Nikita Parris")) %>%
         arrange(player)


##Final Analysis
get.radarplot(data3, c("*Nikita Parris", "Vivianne Miedema", 
                       "Courtney Sweetman-Kirk", "Bethany England"))
#Courtney Sweetman-Kirk OUT

get.radarplot(data3, c("*Nikita Parris", "Jordan Nobbs", 
                       "Francesca Kirby", "Ellen White"))
#Ellen White OUT (similar performance than Kirby, but White is 30 years old)

get.radarplot(data3, c("*Nikita Parris", "Vivianne Miedema", "Bethany England",
                       "Jordan Nobbs", "Ellen White"))
#Bethany England OUT


# So, the 3 players choosen to replace Nikita are: Vivianne Miedema, Jordan Nobbs & Francesca Kirby.
# shotmaps
#1) Miedema
events5 %>% 
        filter(player.name == "Vivianne Miedema") %>% 
        soccermatics::soccerShotmap(adj=F,
                                    theme = "dark",
                                    title = "Vivianne Miedema",
                                    subtitle = "FA Women's Super League 2018/19")

#2) Nobbs
events5 %>% 
        filter(player.name == "Jordan Nobbs") %>% 
        soccermatics::soccerShotmap(adj=F,
                                    theme = "dark",
                                    title = "Jordan Nobbs",
                                    subtitle = "FA Women's Super League 2018/19")

#3) Kirby
events5 %>% 
        filter(player.name == "Francesca Kirby") %>% 
        soccermatics::soccerShotmap(adj=F,
                                    theme = "dark",
                                    title = "Francesca Kirby",
                                    subtitle = "FA Women's Super League 2018/19")



#[*plot with all peformance]
library(ggrepel)
data_ap <- get.performance(events5, unique(events5$player.name)) # get all performance, be patience
data_ap <- data_ap %>% mutate(label=ifelse(xG90>0.35, n, ""))
ggplot(data_ap, aes(x=xG90, y=goals.90, label=label)) + 
        geom_jitter(aes(size=goal_assist90), alpha=0.2) +
        geom_text_repel()