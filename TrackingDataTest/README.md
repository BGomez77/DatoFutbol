Codes and some explanation about process done for this animation (https://twitter.com/DatoFutbol_cl/status/1120521827380543488).

[Sadly i can't share STATS data used, but if you are interested about data structure i could give you some details about data structure]

Process:

1) Reading rawdata (with "pickle" format) using #reticulate package which call a short Python script ("script.py").

2) Preprocessing to get all data in only one dataframe (using #purrr #datatable & #dplyr)

3) Reshaping one sequence of tracking data as example: players and ball coordinates for each sample (using #dplyr #tidyr #stringr & #soccermatics)

4) Viz + animation (using #ggplot2 #ggsoccer & #gganimate)

Done!