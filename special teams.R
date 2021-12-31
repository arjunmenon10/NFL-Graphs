ST_pbp <- pbp %>% filter(!is.na(epa) & touchback != 1 & special == 1)

kicks <- ST_pbp %>% 
  group_by(team = posteam) %>% 
  mutate(epa = dplyr::if_else(play_type == "kickoff", -epa, epa)) %>% 
  summarize(epa_kick = mean(epa), plays_kick = n())

receive <- ST_pbp %>% 
  group_by(team = defteam) %>% 
  mutate(epa = dplyr::if_else(play_type == "kickoff", epa, -epa)) %>% 
  summarize(epa_receive = mean(epa), plays_receive = n())

epa <- kicks |>
  left_join(receive, by = "team") |>
  mutate(
    epa_combined = (epa_kick * plays_kick + epa_receive * plays_receive) / (plays_kick + plays_receive),
    plays_combined = plays_kick + plays_receive
  ) |>
  inner_join(nflfastR::teams_colors_logos, by = c("team" = "team_abbr")) |>
  arrange(desc(epa_combined))

MIN <- ST_pbp %>% 
  filter(home_team == "MIN") %>% select(epa, everything())

epa %>% 
  ggplot(aes(x = fct_reorder(team, epa_combined), y = epa_combined))+
  geom_col(color = epa$team_color2, fill = epa$team_color, width = 0.75)+
  geom_image(aes(y = ifelse(epa_combined > 0, -0.01, 0.01)), image = epa$team_logo_espn, asp = 16/9, size = 0.035)+
  geom_text(aes(y = ifelse(epa_combined > 0, epa_combined + 0.005, epa_combined - 0.005), label = paste0('(', plays_combined, ')')))+
  theme_fivethirtyeight()+
  labs(title = "2022 Special Teams Performances by all teams",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
       subtitle = glue::glue("Vikings have had the worst special teams in the league | # special teams plays in brackets"))+
  theme(axis.title = element_text(size = 16)) + xlab("Teams") + ylab("Special Teams EPA")+
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_blank())+
  theme(legend.position = "none")+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=6))
ggsave('specialteams.png', width = 14, height = 10, dpi = "retina")

  