future::plan("multisession")
pbp <- nflfastR::load_pbp(2021)

avg_distance <- pbp %>% 
  filter(play_type %in% c('pass', 'run')) %>% 
  group_by(posteam) %>% 
  summarise(
    mean_distance2 = mean(ydstogo[down == 2], na.rm = T),
    mean_distance3 = mean(ydstogo[down == 3], na.rm = T)
  ) %>% 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

avg_distance %>% 
  ggplot(aes(x = mean_distance2, y = mean_distance3))+
  geom_image(image = avg_distance$team_logo_espn, asp = 16/9)+
  geom_hline(yintercept = mean(avg_distance$mean_distance3), lty = 'dashed', color = 'red')+
  geom_vline(xintercept = mean(avg_distance$mean_distance2), lty = 'dashed', color = 'red')+
  theme_fivethirtyeight() +
  labs(title = "Average 2nd and 3rd Down distance for every offense",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
       subtitle = glue::glue("Weeks 1-8 | Vikings find themselves in a lot of long distance 2nd and 3rd downs"))+
  theme(axis.title = element_text(size = 18)) + ylab('Average 3rd down distance') + xlab("Average 2nd down distance")+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text = element_text(size = 17))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
  theme(strip.text = element_text(size = 20, face = "bold"))+
  scale_y_reverse(breaks = scales::pretty_breaks(n=7))+
  scale_x_reverse(breaks = scales::pretty_breaks(n=7))
ggsave('downavgdistance.png', width = 14, height = 10, dpi = "retina")

avg_distanceD <- pbp %>% 
  filter(play_type %in% c('pass', 'run')) %>% 
  group_by(defteam) %>% 
  summarise(
    mean_distance2 = mean(ydstogo[down == 2], na.rm = T),
    mean_distance3 = mean(ydstogo[down == 3], na.rm = T)
  ) %>% 
  left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))

avg_distanceD <- avg_distanceD %>% 
  left_join(pass_rush, by = c('team_nick' = 'Team')) %>% 
  group_by(team_nick) %>% 
  slice(1)

avg_distanceD %>% 
  ggplot(aes(x = `PR%`, y = mean_distance3))+
  geom_image(image = avg_distanceD$team_logo_espn, asp = 16/9)+
  geom_hline(yintercept = mean(avg_distanceD$mean_distance3), lty = 'dashed', color = 'red')+
  geom_vline(xintercept = mean(avg_distanceD$`PR%`), lty = 'dashed', color = 'red')+
  geom_smooth(method = 'lm', color = 'gray', se = FALSE)+
  theme_fivethirtyeight() +
  labs(title = "Average 3rd Down distance on defense and pressure rate",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
       subtitle = glue::glue("Weeks 1-9 | Pressure rate on all downs | No correlation between 2 variables"))+
  theme(axis.title = element_text(size = 18)) + ylab('Average 3rd down distance on defense') + xlab("Total Pressure Rate")+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text = element_text(size = 17))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=7))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=7))
ggsave('pressureavgdistance.png', width = 14, height = 10, dpi = "retina")
