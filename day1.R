library(tidyverse)
library(baseballr)
library(RMySQL)
library(gggibbous)
library(scales)
source(file = "config.R")


# GET DATA FROM DB
con <- dbConnect(MySQL(), dbname = dbname, 
                 user = user, 
                 password = password, 
                 host = host)

#stand, p_throws, events, description, fielding_team, batting_team, woba_value, on_3b, on_2b, on_1b, outs_when_up

query <- "SELECT stand, p_throws, events, description, fielding_team, batting_team, 
                woba_value, on_3b, on_2b, on_1b, outs_when_up FROM statcast_full 
          WHERE game_year = 2021 AND game_type = 'R'"

df <- dbGetQuery(con, query)

dbDisconnect(con)

# I want to try to look at woba that comes from the left and right side of the plate by teams
# Or I could look at how teams perform against lefty/righty pitchers.

# let's first look at woba for batters

plate_appearances <- c("field_out", "single", "force_out", "strikeout", "walk", "fielders_choice", "double",
                       "home_run", "grounded_into_double_play", "field_error", "triple", "sac_bunt",
                       "hit_by_pitch", "intent_walk", "sac_fly", "double_play", "strikeout_double_play",
                       "fielders_choice_out", "sac_fly_double_play", "catcher_interf")

at_bats <- c("field_out", "single", "force_out", "strikeout", "fielders_choice", "double", "home_run",
             "grounded_into_double_play", "field_error", "triple", "double_play", "strikeout_double_play",
             "fielders_choice_out")

df %>%
  summarize(mean(woba_value, na.rm = TRUE))

df %>%
  group_by(batting_team, stand) %>%
  summarise(plate_appearances = sum(events %in% plate_appearances),
            woba = mean(woba_value, na.rm = TRUE)) %>%
  pivot_wider(names_from = stand, values_from = c(plate_appearances, woba)) %>%
  ggplot()+
  geom_moon(aes(ratio = plate_appearances_R/(plate_appearances_R + plate_appearances_L), fill = woba_R), x = 1, y = 1, right = TRUE, size = 50) +
  geom_moon(aes(ratio = plate_appearances_L/(plate_appearances_L + plate_appearances_R), fill = woba_L), x = 1, y = 1, right = FALSE, size = 50)+
  geom_text(aes(label = paste0(format(round((plate_appearances_R/(plate_appearances_L + plate_appearances_R))*100,1)), "%"), 
                hjust = ifelse(plate_appearances_R/(plate_appearances_R + plate_appearances_L) > 0.5, 0.4, 1.5)), x=1.25, y=1, size=5.5, 
                color="black")+
  coord_cartesian(xlim=c(0,2), ylim=c(0,2))+
  facet_wrap(~reorder(batting_team,-plate_appearances_R/(plate_appearances_R + plate_appearances_L))) +
  labs(title= "Percentage of Plate Appearances by RHB for MLB Teams",
       subtitle= "2021 Season\n",
       caption= "Data: BaseballSavant and {baseballr} | Inspiration: @etmckinley") +
  theme_bw()+
  scale_fill_gradient2("wOBA",
                       low = muted("blue"),
                       mid = "white",
                       high = muted("red"),
                       midpoint = 0.323,
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill",
                       breaks = c(.280, .300, .320, .340, .360),
                       labels = c(".280", ".300", ".320", ".340", ".360")) +
  theme(legend.position = "right",
        legend.key.height = unit(1.5, "cm"),
        text=element_text(size=30),
        plot.caption=element_text(size=16))

ggsave("Day1_plot.png", height=15, width=15)
