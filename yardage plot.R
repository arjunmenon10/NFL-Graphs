pbp <- nflfastR::load_pbp(2021)

passing_yards <- pbp %>% 
  filter(season == 2021) %>% 
  mutate(
    passer_player_name = case_when(
      passer_player_name == 'Aa.Rodgers' ~ "A.Rodgers",
      TRUE ~ passer_player_name
    )
  ) %>% 
  filter(pass == 1) %>% 
  filter(!is.na(passing_yards)) %>% 
  filter(!is.na(qb_epa)) %>% 
  group_by(passer_player_name, posteam) %>% 
  summarise(
    pass_yards = sum(passing_yards),
    attempts = sum(qb_dropback),
    qb_EPA = sum(qb_epa)
  ) %>% 
  arrange(-pass_yards) %>% 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

rushing_yards <- pbp %>% 
  filter(!is.na(rushing_yards)) %>%
  filter(!is.na(epa)) %>% 
  mutate(
    rusher_player_name = case_when(
      rusher_player_name == 'Aa.Rodgers' ~ "A.Rodgers",
      TRUE ~ rusher_player_name
    )
  ) %>% 
  group_by(rusher_player_name, posteam) %>% 
  summarise(
    rush_yards = sum(rushing_yards),
    rattempts = sum(rush_attempt),
    qb_rush_epa = sum(epa[rush == 1])
  ) %>% 
  arrange(-rush_yards)

total_yards <- pbp %>% 
  group_by(posteam) %>% 
  summarise(
    total_yards = sum(yards_gained, na.rm = T),
    total_epa = sum(epa, na.rm = T)
  ) %>% 
  arrange(-total_yards)

all_yards <- passing_yards %>% 
  left_join(rushing_yards, by = c('passer_player_name' = 'rusher_player_name', 'posteam')) %>% 
  left_join(total_yards, by = 'posteam') 

all_yards <- all_yards %>% 
  mutate(
    yards_resp = sum(pass_yards + rush_yards),
    perc_yards = round((yards_resp/total_yards)*100, 2),
    epa_resp = sum(qb_EPA + qb_rush_epa),
    perc_epa = round((epa_resp/total_epa)*100, 2)
  ) %>% 
  arrange(-yards_resp) %>% 
  head(33) %>% 
  filter(passer_player_name != 'J.Brissett')

all_yards %>% 
  ggplot()+
  geom_bar(aes(x = epa_resp, y = fct_reorder(passer_player_name, epa_resp)), fill = all_yards$team_color, stat = 'identity', alpha = 1, width = 0.85)+
  geom_bar(aes(x = total_epa, y = passer_player_name), stat = 'identity', fill = 'gray', alpha = 0.6, width = 0.85)+
  geom_image(aes(x = epa_resp +1, y = passer_player_name), image = all_yards$team_logo_espn, asp = 16/9, size = 0.025)+
  geom_text(aes(x = epa_resp - 2, y = passer_player_name, label = paste0(perc_epa,"%")), color = 'white')+
  theme_fivethirtyeight() +
  labs(title = "Which Quarterbacks are responsible for most of their team's total yards",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: NFLFastR',
       subtitle = glue::glue("Weeks 1-16 | Passing and rushing yards included | % of total yards included in bars"))+
  theme(axis.title = element_text(size = 18)) + ylab('') + xlab("Yards")+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text = element_text(size = 17))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5),
        axis.line.y = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n=7))
ggsave('QBrespYards.png', width = 14, height = 10, dpi = "retina")


