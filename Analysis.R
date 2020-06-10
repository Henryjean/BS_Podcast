library(tidyverse)
library(extrafont)
library(lubridate)
library(paletteer)
library(scales)
library(gganimate)

#Read in data and replace blanks ("") with NAs
df <- read.csv("episode_data.csv", header=T, na.strings=c("","NA"))


#Set theme for charts
theme_owen <- function () { 
  theme_minimal(base_size=11, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = 'floralwhite')
    )
}

# How often each race/gender has appeared on the BS Podcast

df %>% 
  filter(!is.na(Race)) %>% 
  group_by(Race, Gender) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct = (n / sum(n, na.rm = TRUE)), 
         race_gender = paste0(Race, " ", Gender)) %>% 
  ggplot(aes(x = reorder(race_gender, n), y = n)) + 
  geom_bar(stat = 'identity', fill = '#f25f4c', color = '#001858') + 
  theme_owen() + 
  labs(x = "", 
       y = "Total Episode Appearances", 
       title = "Most Frequent Guests On The Bill Simmons Podcast", 
       subtitle = "By episode appearances (Oct. 1, 2015 - June 5, 2020)") +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold', size = 14), 
        axis.text.y = element_text(margin = margin(0, -15, 0, 0))) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1000)) +
  geom_text(family = "Gill Sans MT", size = 3.5, hjust = 0, aes(x = race_gender, y = n, label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")))


ggsave("race_gender.png", width = 6, height = 5, dpi = 300)


# Race/gender composition of BS Podcast guests, ignoring how often they have appeared 

df %>% 
  select(Guest, Race, Gender) %>% 
  distinct() %>% 
  filter(!is.na(Race)) %>% 
  group_by(Race, Gender)  %>% 
  count() %>% 
  ungroup() %>% 
  mutate(pct = (n / sum(n, na.rm = TRUE)), 
         race_gender = paste0(Race, " ", Gender)) %>% 
  ggplot(aes(x = reorder(race_gender, n), y = n)) + 
  geom_bar(stat = 'identity', fill = '#f25f4c', color = '#001858') + 
  theme_owen() + 
  labs(x = "", 
       y = "Number of guests", 
       title = "Most Frequent Guests On The Bill Simmons Podcast", 
       subtitle = "By race/gender of all guests (Oct. 1, 2015 - June 5, 2020)") +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold', size = 14), 
        axis.text.y = element_text(margin = margin(0, -15, 0, 0))) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 225)) +
  geom_text(family = "Gill Sans MT", size = 3.5, hjust = 0, aes(x = race_gender, y = n, label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")))


ggsave("race_gender_composition.png", width = 6, height = 5, dpi = 300)



# Race of the most frequent guests on the BS Podcast

df %>% 
  filter(!is.na(Guest)) %>% 
  group_by(Guest) %>% 
  mutate(n = n()) %>% 
  select(Guest, Gender, Race, n) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(pct = n / sum(n, na.rm = TRUE)) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 5) %>% 
  ggplot(aes(x = reorder(Guest, n), y = n, fill = Race)) + 
  geom_bar(stat = 'identity', color = '#001858', alpha = .5) + 
  coord_flip() + 
  theme_owen() + 
  labs(x = "", 
       fill = "",
       y = "Total Episode Appearances", 
       title = "Most Frequent Guests On The Bill Simmons Podcast", 
       subtitle = "By episode appearances (Oct. 1, 2015 - June 5, 2020)") +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold'), 
        legend.position = 'top',
        plot.caption = element_text(face = 'italic'),
        axis.text.y = element_text(margin = margin(0, -20, 0, 0)))  +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 25)) +
  geom_text(family = "Gill Sans MT", size = 3.5, hjust = 0, aes(x = Guest, y = n, label = n)) +
  scale_fill_paletteer_d("jcolors::default", direction = 1)

ggsave("most_frequent_guests.png", width = 8, height = 8, dpi = 300)



df %>% 
  filter(!is.na(Guest)) %>% 
  group_by(Guest) %>% 
  mutate(n = n()) %>% 
  select(Guest, Gender, Race, n) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(pct = n / sum(n, na.rm = TRUE)) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 5) %>% 
  ggplot(aes(x = reorder(Guest, n), y = n)) + 
  geom_bar(stat = 'identity', fill = '#f25f4c', color = '#001858', alpha = .75) + 
  coord_flip() + 
  theme_owen() + 
  labs(x = "", 
       fill = "",
       y = "Total Episode Appearances", 
       title = "Most Frequent Guests On The Bill Simmons Podcast", 
       subtitle = "By episode appearances (Oct. 1, 2015 - June 10, 2020)") +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold'), 
        legend.position = 'top',
        plot.caption = element_text(face = 'italic'),
        axis.text.y = element_text(margin = margin(0, -15, 0, 0)))  +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 25)) +
  geom_text(family = "Gill Sans MT", size = 3.5, hjust = 0, aes(x = Guest, y = n, label = n))

ggsave("BS_HOF.png", width = 6, height = 7, dpi = 300)



## Animated gif 

df$Date <- mdy(df$Date)


p <- df %>% 
  group_by(Guest) %>% 
  arrange(Date) %>% 
  mutate(running_count = row_number(), 
         total_appearances = n()) %>%
  filter(total_appearances >  10) %>% 
  ggplot(aes(x = Date, y = running_count, group = Guest)) + 
  geom_line(size = 1, linetype = 1, aes(color = Guest), alpha = .65) + 
  geom_segment(aes(xend = as.Date("2020-10-01"), yend = running_count, color = Guest), linetype = 2, colour = 'grey') + 
  geom_point(size = 1.5, aes(color = Guest), alpha = .5) + 
  geom_text(aes(x = as.Date("2020-10-01"), label = Guest, color = Guest), hjust = 0, alpha = .85, fontface = "bold", size = 3) + 
  transition_reveal(Date) + 
  coord_cartesian(clip = 'off') +
  labs(x = "", 
       y = "Total Appearances", 
       title = "Most Frequent Guests On The Bill Simmons Podcast", 
       subtitle = "By episode appearances (Oct. 1, 2015 - June 10, 2020)") +
  theme_owen() + 
  theme(legend.position = 'none', 
        plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold')) +
  scale_y_continuous(breaks = seq(0, 150, 10)) + 
  scale_x_date(limits = c(as.Date("2015-10-01"), as.Date("2021-09-01")),
               date_labels = "%Y",
               breaks = c(as.Date("2016-01-01"), as.Date("2017-01-01"), as.Date("2018-01-01"), as.Date("2019-01-01"), as.Date("2020-01-01")))

options(gganimate.dev_args = list(width = 6, height = 6, units = 'in', res=300))

anim <- animate(p, 
        nframes = 100,  
        end_pause = 25,
        fps = 10, 
        rewind = FALSE, 
        duration = 15)

anim_save("BS_HOF.gif", anim)
