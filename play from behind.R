play_behind <- pbp %>% 
  filter(!is.na(epa)) %>% 
  filter(wp <= 0.3) %>% 
  group_by(name, posteam) %>% 
  summarise(
    behind_EPA = mean(epa, na.rm = T),
    behind_CPOE = mean(cpoe, na.rm = T),
    attempts = sum(qb_dropback)
  ) %>% 
  filter(attempts > 30) %>% 
  arrange(desc(behind_EPA)) %>% 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

play_behind %>% 
  ggplot(aes(x = behind_EPA, y = fct_reorder(name, behind_EPA)))+
  geom_col(fill = play_behind$team_color)+
  geom_image(image = play_behind$team_logo_espn, asp = 16/9, size = 0.025)+
  theme_fivethirtyeight() +
  labs(title = "Which Quarterbacks are the best when playing from behind",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
       subtitle = glue::glue("Weeks 1-9 | Plays when Win Probability < 0.3 | Includes QB Runs"))+
  theme(axis.title = element_text(size = 18)) + ylab('') + xlab("EPA/Play from Behind")+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text = element_text(size = 17))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=7))
ggsave('QBbehind.png', width = 14, height = 10, dpi = "retina")
