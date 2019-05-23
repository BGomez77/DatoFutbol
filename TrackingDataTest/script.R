### Packages
require(reticulate)
require(purrr)
require(data.table)
require(dplyr)
require(tidyr)
require(stringr)
#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("jogall/soccermatics")
require(soccermatics)
#devtools::install_github("torvaney/ggsoccer")
require(ggsoccer)
require(ggplot2)
#devtools::install_github('thomasp85/gganimate')
library(gganimate)
#install.packages("gifski")
library(gifski)

### Load data
#(https://stackoverflow.com/questions/35121192/reading-a-pickle-file-pandas-python-data-frame-in-r)
source_python("script.py")
data <- read_pickle_file("test_data_1.pkl") #filename from Data Insights dataset

### Get dataframe with all data
dt_list <- map(data, as.data.frame)
dt_data <- rbindlist(dt_list, fill=TRUE, idcol=T)
df <- dt_data %>%
      rename(sequ=.id) %>%
      arrange(sequ)

#write.csv(df, "data_test.csv", row.names=F)
#df <- read.csv("data_test.csv", stringsAsFactors = F)

### Transform data structure for 1 sequence
w <- df %>%
     filter(sequ == "sequence_1")

w$sample <- seq(1, nrow(w))

# columns number of x,y coordenates for both teams
idx1 <- seq(2, 22, 2)
idy1 <- seq(3, 23, 2)
idx2 <- seq(24, 44, 2)
idy2 <- seq(25, 45, 2)

names(w)[idx1] <- paste0("def", 1:11, "x")
names(w)[idy1] <- paste0("def", 1:11, "y")
names(w)[idx2] <- paste0("att", 1:11, "x")
names(w)[idy2] <- paste0("att", 1:11, "y")

# reshaping players info
wtx <- gather(w, key, value, -1, -idy1, -idy2, -46, -47, -48) %>%
       select("sequ", "sample", key, value) %>%
       mutate(key2= substr(key, 1, (nchar(key) -1)))

wty <- gather(w, key, value, -1, -idx1, -idx2, -46, -47, -48) %>%
        select("sequ", "sample", key, value) %>%
        mutate(key2= substr(key, 1, (nchar(key) -1)))

wt <- wtx %>% inner_join(wty, by=c("sample", "key2")) %>%
      select(sequ.x, sample, key2, value.x, value.y) %>%
      rename(sequ=sequ.x, key=key2, x=value.x, y=value.y)

wtt <- soccerTransform(wt, xMin=-52.5, xMax=52.5, yMin=-34, yMax=34, 
                       method="manual", lengthPitch = 120, widthPitch = 80)

# reshaping ball info
btx <- gather(w, key, value, -1, -idx1, -idx2, -idy1, -idy2, -47, -48) %>%
        select("sequ", "sample", key, value)

bty <- gather(w, key, value, -1, -idx1, -idx2, -idy1, -idy2, -46, -48) %>%
        select("sequ", "sample", key, value)

bt <- btx %>% inner_join(bty, by="sample") %>%
        select(sequ.x, sample, value.x, value.y) %>%
        rename(sequ=sequ.x, x=value.x, y=value.y)

btt <- soccerTransform(bt, xMin=-52.5, xMax=52.5, yMin=-34, yMax=34, 
                       method="manual", lengthPitch = 120, widthPitch = 80)

btt$key <- "ball"


### Viz + Animation
#glimpse(wtt)
anim <- wtt %>% bind_rows(btt) %>%
        mutate(team_name=substr(key, 1, 3)) %>%
        ggplot() +
        annotate_pitch(colour = "white",
                       fill   = "chartreuse4",
                       dimensions = pitch_statsbomb) +
        theme_pitch() +
        geom_point(aes(x=x, y=y, fill=factor(team_name), size=factor(team_name)), 
                   col="black", 
                   shape=21) +
        transition_time(sample) +
        theme(plot.background = element_rect(fill = "chartreuse4"),
              title = element_text(colour = "white"),
              legend.position = "none") +
        scale_size_manual(values=c(6,3,6)) +
        scale_fill_manual(values=c("#2b8cbe", "yellow", "#dd3497"), 
                          aesthetics = "fill")

animate(anim,
        width = 1024, height = 768,
        nframes=nrow(btt), fps = 10)

anim_save("test.gif")

