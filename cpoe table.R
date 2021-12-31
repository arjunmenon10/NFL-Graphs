cpoe_distance <- pbp %>% 
  mutate(
    throw_length = case_when(
      air_yards < 0 ~ "Behind LOS",
      air_yards >= 0 & air_yards < 10 ~ "Short",
      air_yards >= 10 & air_yards < 20 ~ "Intermediate",
      air_yards >= 20 ~ "Deep"
    )) %>% 
  mutate(
    passer_player_name = case_when(
      passer_player_name == "Jos.Allen" ~ "J.Allen",
      TRUE ~ passer_player_name
    )
  )

cpoe_qbs <- cpoe_distance %>% 
  filter(!is.na(cpoe)) %>% 
  group_by(passer_player_name, posteam) %>% 
  summarise(
    cpoe_behind = round(mean(cpoe[throw_length == "Behind LOS"]), 2),
    cpoe_short = round(mean(cpoe[throw_length == "Short"]),2),
    cpoe_int = round(mean(cpoe[throw_length == "Intermediate"]),2),
    cpoe_deep = round(mean(cpoe[throw_length == "Deep"]), 2),
    attempts_behind = round(sum(qb_dropback[throw_length == "Behind LOS"]), 2),
    attempts_short = round(sum(qb_dropback[throw_length == "Short"]), 2),
    attempts_int = round(sum(qb_dropback[throw_length == "Intermediate"]), 2),
    attempts_deep = round(sum(qb_dropback[throw_length == "Deep"]), 2),
    overall_cpoe = round(mean(cpoe), 2)
  ) %>% 
  filter(attempts_int > 20) %>% 
  arrange(desc(overall_cpoe)) %>% 
  ungroup() %>% 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

cpoe_qb_tbl <- cpoe_qbs %>% 
  select(passer_player_name, team_logo_espn, cpoe_behind, cpoe_short, cpoe_int, cpoe_deep, attempts_behind, attempts_short, attempts_int, attempts_deep, overall_cpoe) %>%
  slice(1:17) %>% 
  gt() %>% 
  gt_theme_538() %>% 
  gt_merge_stack(col1 = cpoe_behind, col2 = attempts_behind, color = "#505056") %>%
  gt_merge_stack(col1 = cpoe_short, col2 = attempts_short, color = "#505056") %>%
  gt_merge_stack(col1 = cpoe_int, col2 = attempts_int, color = "#505056") %>%
  gt_merge_stack(col1 = cpoe_deep, col2 = attempts_deep, color = "#505056") %>% 
  gt_color_rows(
    cpoe_behind:cpoe_deep, palette = c('#f02720', '#ea6b73','white', '#6ba3d6','#2c69b0'),
    use_paletteer = FALSE, domain = -18:22) %>%
  gt_color_rows(
    overall_cpoe, palette = c('#f02720', '#ea6b73','white', '#6ba3d6','#2c69b0'),
    use_paletteer = FALSE, domain = -7:9) %>%
  gt_img_rows(team_logo_espn) %>% 
  cols_align(
    align = "center",
    columns = c(passer_player_name, team_logo_espn, cpoe_behind, cpoe_short, cpoe_int, cpoe_deep, attempts_behind, attempts_short, attempts_int, attempts_deep, overall_cpoe)
  ) %>% 
  cols_label(
    passer_player_name = "QB",
    team_logo_espn = "",
    cpoe_behind = "Behind LOS",
    cpoe_short = "Short",
    cpoe_int = "Intermediate",
    cpoe_deep = "Deep",
    overall_cpoe = "Overall CPOE") %>% 
  tab_header(
    title = "Most accurate Quarterbacks at each level of the field",
    subtitle = "Weeks 1-10 | Number of dropbacks at each level under CPOE"
  ) %>% 
  tab_source_note("Table: @arjunmenon100 | Data: NFLFastR")
gtsave(cpoe_qb_tbl, "cpoe_qb_tbl.png")

cpoe_qb_tbl1 <- cpoe_qbs %>% 
  select(passer_player_name, team_logo_espn, cpoe_behind, cpoe_short, cpoe_int, cpoe_deep, attempts_behind, attempts_short, attempts_int, attempts_deep, overall_cpoe) %>%
  slice(18:34) %>% 
  gt() %>% 
  gt_theme_538() %>% 
  gt_merge_stack(col1 = cpoe_behind, col2 = attempts_behind, color = "#505056") %>%
  gt_merge_stack(col1 = cpoe_short, col2 = attempts_short, color = "#505056") %>%
  gt_merge_stack(col1 = cpoe_int, col2 = attempts_int, color = "#505056") %>%
  gt_merge_stack(col1 = cpoe_deep, col2 = attempts_deep, color = "#505056") %>% 
  gt_color_rows(
    cpoe_behind:cpoe_deep, palette = c('#f02720', '#ea6b73','white', '#6ba3d6','#2c69b0'),
    use_paletteer = FALSE, domain = -18:22) %>%
  gt_color_rows(
    overall_cpoe, palette = c('#f02720', '#ea6b73','white', '#6ba3d6','#2c69b0'),
    use_paletteer = FALSE, domain = -7:9) %>%
  gt_img_rows(team_logo_espn) %>% 
  cols_align(
    align = "center",
    columns = c(passer_player_name, team_logo_espn, cpoe_behind, cpoe_short, cpoe_int, cpoe_deep, attempts_behind, attempts_short, attempts_int, attempts_deep, overall_cpoe)
  ) %>% 
  cols_label(
    passer_player_name = "QB",
    team_logo_espn = "",
    cpoe_behind = "Behind LOS",
    cpoe_short = "Short",
    cpoe_int = "Intermediate",
    cpoe_deep = "Deep",
    overall_cpoe = "Overall CPOE") %>% 
  tab_header(
    title = "Most accurate Quarterbacks at each level of the field",
    subtitle = "Weeks 1-10 | Number of dropbacks at each level under CPOE"
  ) %>% 
  tab_source_note("Table: @arjunmenon100 | Data: NFLFastR")

gtsave(cpoe_qb_tbl, "cpoe_qb_tbl.png")
gtsave(cpoe_qb_tbl1, "cpoe_qb_tbl1.png")

img1 <- magick::image_read("cpoe_qb_tbl.png")
img2 <- magick::image_read("cpoe_qb_tbl1.png")
imgboth <- magick::image_append(c(img1, img2))
image_write(imgboth, path = "cpoe_qb_tblfin.png", format = "png") 

