playoffs <- pbp01 %>% 
  filter(week > 17) %>% 
  filter(season < 2021)

playoff_kickers <- playoffs %>% 
  filter(!is.na(wpa)) %>% 
  filter(field_goal_attempt == 1) %>% 
  group_by(kicker_player_name, posteam, season) %>% 
  summarise(
    kicker_wpa = sum(wpa),
    kicker_epa = sum(epa),
    kicks = n()
  ) %>% 
  arrange(-kicker_wpa) %>% 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

playoff_kickers21 <- pbp %>% 
  filter(week > 18) %>% 
  filter(!is.na(wpa)) %>% 
  filter(field_goal_attempt == 1) %>% 
  group_by(kicker_player_name, posteam, season) %>% 
  summarise(
    kicker_wpa = sum(wpa),
    kicker_epa = sum(epa),
    kicks = n()
  ) %>% 
  arrange(-kicker_wpa) %>% 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

allplayoff <- rbind(playoff_kickers, playoff_kickers21) %>% 
  filter(kicks > 4)

allplayoff %>% 
  ggplot(aes(x = kicker_wpa, y = kicker_epa))+
  geom_point(aes(size = kicks), shape = 21, fill = allplayoff$team_color2, color = allplayoff$team_color)+
  geom_text_repel(aes(label = paste0(kicker_player_name, ", ", season)), size = 5)+
  theme_fivethirtyeight()+
  labs(title = "Best NFL playoff kicking performances",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: NFLFastR',
       subtitle = glue::glue("Minimum 5 field goals in the playoffs | All kickers from 2001-2021 | Dot Size = # of kicks"))+
  theme(axis.title = element_text(size = 18)) + ylab('Total EPA from Field Goals') + xlab("Total WPA from Field Goals")+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text = element_text(size = 17))+
  theme(legend.position = 'none')+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=8))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=8))
ggsave('playoffkicker.png', width = 14, height = 10, dpi = "retina")

Tuckre <- playoffs %>% filter(kicker_player_name == 'J.Tucker') %>% 
  select(desc, epa, ep, everything())

playoffs %>% filter(kicker_player_name == 'N.Kaeding') %>% 
 filter(season == 2009) %>% 
  filter(!is.na(wpa)) %>% 
  filter(field_goal_attempt == 1) %>% 
  group_by(kicker_player_name, posteam, season) %>% 
  summarise(
    kicker_wpa = sum(wpa),
    kicker_epa = mean(epa),
    kicks = n()
  ) 
