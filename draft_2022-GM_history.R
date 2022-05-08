# Author: Felix Zurek @felixzurek

library(nflverse)
library(tidyverse)

#--------------------------------------------

#reading in csv
draft_2022 <- read_csv("draft_2022.csv")

#calculating expected age of draft picks
m <- mgcv::gam(age ~ s(pick), data = draft_2022)
df <- draft_2022 %>%
  filter(!is.na(age)) %>%
  mutate(exp_age = m$fitted.values,
         age_oe = age - exp_age)

gm_history <- df %>% 
  group_by(top_executive, team) %>% 
  summarise(avg_age = mean(age),
            avg_exp_age = mean(exp_age),
            avg_age_oe = mean(age_oe),
            picks = n())

p <- ggplot(gm_history,
       aes(x = avg_age_oe, y = reorder(top_executive,avg_age_oe)))+
  geom_col(aes(fill = team), width = 0.66)+
  nflplotR::scale_fill_nfl(type = "primary")+
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .03)+
  labs(title = "NFL Draft 2022 Age Tendencies",
       subtitle = "Expected Age Based On Draft Position",
       x = "Average Age over Expectation",
       caption = "by @felixzurek | Data: pro-football-reference.com, Wikipedia")+
  theme_minimal(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_blank())

ggsave("test.png", p, width = 12, height = 9, bg = "white", dpi = 200)
