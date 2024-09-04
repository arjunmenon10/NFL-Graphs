library(tidyverse)
library(nflreadr)
library(gt)
library(gtExtras)
pbp <- load_pbp(2023)

pbp <- pbp |> 
  mutate(
    expl = ifelse(rush == 1 & yards_gained >= 10 | pass == 1 & yards_gained >= 20, 1, 0)
  ) |> select(expl, everything())

games <- nflreadr::load_player_stats(2023) |> 
  group_by(player_id, recent_team) |> 
  summarise(
    totgames = n()
  ) |> group_by(recent_team) |> summarise(gamesp = max(totgames))

### in filter statement, do week >= [whatever week you want to look at]
tbl <- pbp |> 
  filter(pass == 1 | rush == 1) |> 
  group_by(defteam) |> 
  summarise(
    `rush(10+)` = sum(expl[rush == 1], na.rm = T),
    rushperplay = mean(expl[rush == 1], na.rm = T),
    `pass(20+)` = sum(expl[pass == 1], na.rm = T),
    passperplay = mean(expl[pass == 1], na.rm = T),
    `Expl Plays` = sum(expl, na.rm = T),
    Explperplay = mean(expl, na.rm = T)
  ) |> left_join(games, by = c('defteam' = 'recent_team')) |> 
  mutate(
    rushpergame = round(`rush(10+)`/gamesp, 1),
    passpergame = round(`pass(20+)`/gamesp, 1),
    explpergame = round(`Expl Plays`/gamesp, 1)
  ) |> 
  arrange(`Expl Plays`) |> 
  select(defteam, `rush(10+)`, rushperplay, rushpergame, `pass(20+)`, passperplay, passpergame, `Expl Plays`, Explperplay, explpergame) |> 
  gt() |> 
  data_color(
    columns = c(`rush(10+)`, `pass(20+)`, `Expl Plays`,),
    colors = scales::col_numeric(
      palette = c("#e15759", "#edc948", "#59a14f"),
      domain = NULL,
      reverse = TRUE
    )
  ) %>%
  fmt_percent(columns = c(rushperplay, passperplay, Explperplay), decimals = 1) |> 
  opt_align_table_header(align = "center") %>%
  cols_align("center") %>%
  opt_row_striping() %>%
  cols_label(defteam = "Defense") |> 
  gt_theme_espn() |> 
  tab_header(title = md("Most Explosive Plays allowed in 2023 season (weeks 1-18)"),
             subtitle = "Explosive Plays = 10+ yard gain in run game or 20+ yard gain in pass game") |> 
  tab_source_note("Table: @arjunmenon100 | Data: nflreadr | Inspiration: @Marcus_Mosher")

gtsave(tbl, "expltbl.png")

pbp <- load_pbp(2023)

pbp <- pbp |> 
  mutate(
    expl = ifelse(rush == 1 & yards_gained >= 10 | pass == 1 & yards_gained >= 20, 1, 0),
    expl2 = ifelse(rush == 1 & yards_gained >= 20 | pass == 1 & yards_gained >= 30, 1, 0),
    expl3 = ifelse(rush == 1 & yards_gained >= 30 | pass == 1 & yards_gained >= 40, 1, 0),
  ) |> select(expl, everything())

tbl <- pbp |> 
  filter(pass == 1 | rush == 1, week >= 12) |> 
  group_by(defteam) |> 
  summarise(
    `rush(10+)` = sum(expl[rush == 1], na.rm = T),
    `pass(20+)` = sum(expl[pass == 1], na.rm = T),
    `rush(20+)` = sum(expl2[rush == 1], na.rm = T),
    `pass(30+)` = sum(expl2[pass == 1], na.rm = T),
    `rush(30+)` = sum(expl3[rush == 1], na.rm = T),
    `pass(40+)` = sum(expl3[pass == 1], na.rm = T),
    `Expl Plays` = sum(expl, na.rm = T)
  ) |> 
  arrange(-`Expl Plays`) |> 
  select(-`Expl Plays`) |> 
  gt() |> 
  data_color(
    columns = c(`rush(10+)`, `pass(20+)`, `rush(20+)`, `pass(30+)`, `rush(30+)`, `pass(40+)`),
    colors = scales::col_numeric(
      palette = c("#e15759", "#edc948", "#59a14f"),
      domain = NULL,
      reverse = TRUE
    )
  ) %>%
  opt_align_table_header(align = "center") %>%
  cols_align("center") %>%
  opt_row_striping() %>%
  cols_label(defteam = "Defense") |> 
  gt_theme_espn() |> 
  tab_header(title = md("Most Explosive Plays allowed in 2023 season (weeks 12-18)"),
             subtitle = "Explosive Plays = 10+ yard gain in run game or 20+ yard gain in pass game") |> 
  tab_source_note("Table: @arjunmenon100 | Data: nflreadr | Inspiration: @Marcus_Mosher")

gtsave(tbl, "expltbl2.png")

spbp <- load_pbp(2023)

pbp <- pbp |> 
  mutate(
    expl = ifelse(rush == 1 & yards_gained >= 10 | pass == 1 & yards_gained >= 20, 1, 0)
  ) |> select(expl, everything())

tbl <- pbp |> 
  filter(pass == 1 | rush == 1) |> 
  group_by(posteam) |> 
  summarise(
    `rush(10+)` = sum(expl[rush == 1], na.rm = T),
    `pass(20+)` = sum(expl[pass == 1], na.rm = T),
    `Expl Plays` = sum(expl, na.rm = T)
  ) |> 
  arrange(-`Expl Plays`) |> 
  gt() |> 
  data_color(
    columns = c(`rush(10+)`, `pass(20+)`, `Expl Plays`),
    colors = scales::col_numeric(
      palette = c("#e15759", "#edc948", "#59a14f"),
      domain = NULL
    )
  ) %>%
  opt_align_table_header(align = "center") %>%
  cols_align("center") %>%
  opt_row_striping() %>%
  cols_label(posteam = "Offense") |> 
  gt_theme_espn() |> 
  tab_header(title = md("Most Explosive Plays on offense in 2023"),
             subtitle = "Explosive Plays = 10+ yard gain in run game or 20+ yard gain in pass game") |> 
  tab_source_note("Table: @arjunmenon100 | Data: nflreadr | Inspiration: @Marcus_Mosher")

gtsave(tbl, "explofftbl.png")
  

pbp |> 
  filter(pass == 1 | rush == 1) |> 
  group_by(defteam, posteam, week) |> 
  summarise(
    `rush(10+)` = sum(expl[rush == 1], na.rm = T),
    rushperplay = mean(expl[rush == 1], na.rm = T),
    `pass(20+)` = sum(expl[pass == 1], na.rm = T),
    passperplay = mean(expl[pass == 1], na.rm = T),
    `Expl Plays` = sum(expl, na.rm = T),
    Explperplay = mean(expl, na.rm = T)
  ) |> view()
