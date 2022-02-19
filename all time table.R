third_down_pen_QB <- passing_pbp %>% 
  filter(penalty == 'Defensive pass interference') %>% 
  group_by(passer_name) %>% 
  summarise(
    EPA = round(sum(EPA, na.rm = T), 2),
    DPIs = n()
  ) %>% 
  arrange(desc(EPA)) %>% 
  left_join(pictures, by = c('passer_name' = 'full_name')) %>% 
  slice(1)

third_down_pen_WR <- passing_pbp %>% 
  filter(penalty == 'Defensive pass interference') %>%
  group_by(target_name) %>% 
  summarise(
    EPA = round(sum(EPA, na.rm = T), 2),
    DPIs = n()
  ) %>% 
  arrange(desc(EPA)) %>% 
  ungroup() %>% 
  mutate(
    target_name = case_when(
      target_name == "Marvin Jones Jr." ~ "Marvin Jones",
      target_name == "Allen Robinson II" ~ "Allen Robinson",
      TRUE ~ target_name
    )
  ) %>% 
  left_join(pictures, by = c('target_name' = 'full_name')) %>% 
  slice(1:3)

third_down_pen_RB <- passing_pbp %>% 
  filter(penalty == 'Defensive pass interference') %>% 
  filter(target_season_position == "HB") %>% 
  group_by(target_name) %>% 
  summarise(
    EPA = round(sum(EPA, na.rm = T), 2),
    DPIs = n()
  ) %>% 
  arrange(desc(EPA)) %>% 
  ungroup() %>% 
  mutate(
    target_name = case_when(
      target_name == "Duke Johnson Jr." ~ "Duke Johnson",
      TRUE ~ target_name
    )
  ) %>% 
  left_join(pictures, by = c('target_name' = 'full_name')) %>% 
  slice(1)

third_down_pen_TE <- passing_pbp %>% 
  filter(penalty == 'Defensive pass interference') %>% 
  filter(target_season_position == "TE") %>% 
  group_by(target_name) %>% 
  summarise(
    EPA = round(sum(EPA, na.rm = T), 2),
    DPIs = n()
  ) %>% 
  arrange(desc(EPA)) %>% 
  ungroup() %>% 
  left_join(pictures, by = c('target_name' = 'full_name')) %>% 
  slice(1)

all_third_down_pen <- rbind(third_down_pen_QB, third_down_pen_RB, third_down_pen_WR, third_down_pen_TE)

position <- c('QB', 'RB', 'WR', 'WR', 'WR', 'TE')

all_third_down_pen <- all_third_down_pen %>% 
  cbind(position)

third_down_roster <- all_third_down_pen %>% 
  select(target_name, position, headshot_url, EPA, DPIs) %>% 
  gt() %>% 
  gt_theme_538() %>% 
  gt_merge_stack(col1 = EPA, col2 = DPIs, color = "#505056") %>%
  gt_img_rows(headshot_url) %>% 
  cols_align(
    align = "center",
    columns = c(target_name, headshot_url, EPA, DPIs)
  ) %>% 
  cols_label(
    target_name = "Players",
    headshot_url = "",
    EPA = "Total EPA Generated") %>% 
  tab_header(
    title = "What team is the greatest show on turf?",
    subtitle = "2011-2021 | # of PI's under EPA | Top players at drawing DPIs in game"
  ) %>% 
  tab_source_note("Table: @arjunmenon100 | Data: PFF")
gtsave(third_down_roster, "third_down_roster.png")
