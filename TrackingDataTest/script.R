## Packages
library(reticulate)
library(purrr)
library(data.table)
library(dplyr)

## Load data
#(https://stackoverflow.com/questions/35121192/reading-a-pickle-file-pandas-python-data-frame-in-r)
source_python("script.py")
data <- read_pickle_file("train_data.pkl")

## Get dataframe with all data
dt_list <- map(data, as.data.frame)
dt_data <- rbindlist(dt_list, fill=TRUE, idcol=T)
df <- dt_data %>%
      rename(sequ=.id) %>%
      arrange(sequ)

#write.csv(df, "data_train.csv", row.names=F)

## Animation
source("soccerAnimate.R")
soccerAnimate(df, "sequence_1001")
anim_save(paste0(s,".gif"))
