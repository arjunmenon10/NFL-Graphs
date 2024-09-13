pbp <- nflreadr::load_pbp(2023)


success_rate <- pbp |> 
  filter(week > 18, rush == 1 | pass == 1, wp >= 0.025 & wp <= 0.975) |> 
  group_by(posteam, defteam, game_id) |> 
  summarise(
    successr = mean(success, na.rm = T)
  ) 

success_rate2 <- pbp |> 
  filter(week > 18, rush == 1 | pass == 1, wp >= 0.025 & wp <= 0.975) |> 
  group_by(posteam, defteam, game_id) |> 
  summarise(
    successr = mean(success, na.rm = T)
  ) |> 
  left_join(success_rate, by = c('defteam' = 'posteam', 'game_id')) |> 
  mutate(
    net_success = successr.x - successr.y
  ) |> 
  arrange(-net_success) |> group_by(game_id) |> 
  slice(1)

games <- nflreadr::load_schedules()

games <- games |> mutate(winner = case_when(
  result > 0 ~ home_team,
  result < 0 ~ away_team,
  TRUE ~ 'Tie'
))

success_rate2 <- left_join(success_rate2, games |> select(game_id, winner), by = 'game_id')

success_rate2 <- success_rate2 |> 
  mutate(
    loser = ifelse(winner == posteam, defteam, posteam),
    net_success = ifelse(posteam == winner, net_success, net_success*-1)
  )

success_rate2 <- success_rate2 |> 
  left_join(teams_colors_logos, by = c('winner' = 'team_abbr')) |> 
  rename(team_logo_espnw = "team_logo_espn",
         colorwin = "team_color") |> 
  left_join(teams_colors_logos, by = c('loser' = 'team_abbr')) |> 
  rename(team_logo_espnl = "team_logo_espn") |> 
  arrange(net_success)

success_rate2 |> 
  mutate(game_id = paste(winner, "v", loser)) |> 
  ggplot(aes(x = reorder(game_id, net_success)))+
  geom_image(aes(y = ifelse(net_success > 0, -0.01, 0.01)), image = success_rate2$team_logo_espnl, asp = 16/9, size = 0.05)+
  geom_image(aes(y = ifelse(net_success > 0, net_success + 0.01, net_success - 0.01)), 
                 image = success_rate2$team_logo_espnw, asp = 16/9, size = 0.05)+
  geom_col(aes(y = net_success), fill = success_rate2$colorwin, alpha = 0.75)+
  theme_fivethirtyeight()+
  labs(title = "Did we really get beat that bad?",
       caption = 'By: Arjun Menon | @arjunmenon100 | Inspiration: @statsowar',
       subtitle = glue::glue("Playoff Games Net Success Rates for all games in non-garbage time (WP between 2.5% & 97.5%)"))+
  theme(axis.title = element_text(size = 18)) +xlab('') + ylab("Net Success Rate")+
  theme(axis.text = element_text(size = 15),
        legend.position = 'none',
        axis.text.x = element_text(size = 17, angle = 45))+
  theme(panel.grid.minor.x=element_blank())+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 20, hjust = 0.5),
        plot.caption = element_text(size = 15),
        panel.grid.major.x = element_blank())+
  scale_y_continuous(breaks = scales::pretty_breaks(n=8))
ggsave('successrates.png', width = 14, height = 10, dpi = "retina")
  
tbl <- success_rate2 |> 
  ungroup() |> 
  arrange(-net_success) |> head(10) |> 
  select(week, season, posteam, successr.x, defteam, successr.y, net_success) |> 
  gt() |> 
  cols_label(
    posteam = 'Winner',
    defteam = "Loser",
    successr.x = "Winner Success rate",
    successr.y = "Loser Success rate",
    net_success = "Net Success Rate"
  ) %>%
  cols_align(
    align = "center"
  ) %>% 
  tab_source_note("Table: (@arjunmenon100) | Data: PFF") |> 
  tab_header(
    title = md("Biggest difference in success rates in a game since 2013"),
    subtitle = "Looking at all plays regardless of garbage time or not"
  ) |> gt_theme_538() |> 
  fmt_percent(c(successr.x, successr.y, net_success), decimals = 2)
gtsave(tbl, "blowoutsuccess.png")
