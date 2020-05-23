## Beach Volleyball
## 21/05/2020

library(tidyverse)

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000) %>% 
  mutate(Hrs = as.integer(str_extract(duration,regex("\\d+(?=\\:)"))),
         Mins = as.integer(str_extract(duration,regex("(?<=\\:)\\d+"))),
         Game_Time = Hrs*60 + Mins)


# EDA -----------------------------------------------------------------------------------------

View(vb_matches)

## Player counts
vb_matches %>% 
  pivot_longer(c(l_player1,l_player2,w_player1,w_player2),names_to = "PlayerType",values_to = "PlayerName") %>%
  count(PlayerName,sort = TRUE)


## Player Heights
vb_matches %>% 
  pivot_longer(c(w_p1_hgt,w_p2_hgt,l_p1_hgt,l_p2_hgt),names_to = "PlayerType",values_to = "PlayerHeight") %>%
  ggplot(aes(x = PlayerHeight)) +
  geom_histogram(binwidth = 1,col = "black",fill = "dodgerblue")
  

## Game Durations

vb_matches %>% 
  ggplot(aes(x = Game_Time)) +
  geom_histogram(binwidth = 1,col = "black",fill = "dodgerblue") +
  scale_x_continuous(breaks = seq(0,140,15)) +
  labs(title = "Distribution of game duration",
       y = "Counts",
       x = "Game Time (minutes)")
  
## ---------------------------##
##    Predict Game Duration   ##
## ---------------------------##
# PredictLength -------------------------------------------------------------------------------


vb_dur.lm <- lm()

