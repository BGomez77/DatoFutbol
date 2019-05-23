Codes and some explanation about process done for this animation (https://twitter.com/DatoFutbol_cl/status/1120521827380543488).

[Sadly i can't share this used STATS data, but if you are interested about data structure i could give you some details about: ismaelgomezs@gmail.com]

Processing:

1) Reading rawdata (with "pickle" format) using #reticulate package which call a short Python script ("script.py").

2) Preprocessing to get all data in only one dataframe (using #purrr #datatable & #dplyr)

3) Source "soccerAnimate" function to use it. This way you can (a) reshaping one sequence of tracking data as example: players and ball coordinates for each sample using #dplyr #tidyr #stringr & #soccermatics and (b) get the animation (using #ggplot2 #ggsoccer & #gganimate). You also can add a convex hull for each tam (default) and change the pitch color.

Done!
