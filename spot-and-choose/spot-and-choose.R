install.packages("nflfastR")

library(nflfastR)
library(tidyverse)
library(ggplot2)

games_2022 <- nflfastR::load_pbp(2022)

td_curve <- games_2022 %>%
  filter(down == 1) %>%
  filter(play_type %in% c('pass', 'run')) %>%
  filter(ydstogo == 10 | goal_to_go == 1) %>%
  filter(half_seconds_remaining > 120) %>%
  select(posteam, yardline_100, td_prob)

td_curve_KC <- games_2022 %>%
  filter(down == 1) %>%
  filter(play_type %in% c('pass', 'run')) %>%
  filter(ydstogo == 10 | goal_to_go == 1) %>%
  filter(half_seconds_remaining > 120) %>%
  filter(posteam == 'KC') %>%
  select(yardline_100, td_prob)

gg <- ggplot(td_curve, aes(yardline_100, td_prob)) +
  geom_point(data = subset(td_curve, posteam != "KC"), color = "#EEEEEE") +
  geom_point(data = subset(td_curve, posteam == "KC"), alpha=0.4, color = "red") +
  geom_smooth(method = "loess", se = TRUE, linetype = "dashed", color = "black") +  # Regression for Other Teams
  geom_smooth(data = subset(td_curve, posteam == "NYJ"), method = "loess", se = TRUE, linetype = "dashed", color = "darkgreen") + 
  #geom_smooth(data = subset(td_curve, posteam == "KC"), method = "loess", se = TRUE, linetype = "solid", color = "red") +  # Regression for Team A
  geom_hline(yintercept=0.5, color = "red") +
  labs(title = "Given 1st+10, how likely is my drive to end in a TD?",
       x = "Yards from endzone",
       y = "Expected TD prob") +
  #ylim(0.4, 0.6) +
  #xlim(25, 50) +
  theme_minimal()  # You can choose other themes as well
print(gg)

