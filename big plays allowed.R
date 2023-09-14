install.packages("nflreadr")
install.packages("tidyverse")
install.packages("gt")
install.packages("gtExtras")
library(nflreadr)
library(tidyverse)
library(gt)
library(gtExtras)

pbp <- load_pbp(2023)

pbp <- pbp |> 
  mutate(
    expl = ifelse(rush == 1 & yards_gained >= 10 | pass == 1 & yards_gained >= 15, 1, 0)
  ) |> select(expl, everything())

tbl <- pbp |> 
  filter(pass == 1 | rush == 1) |> 
  group_by(defteam) |> 
  summarise(
    `rush(10+)` = sum(expl[rush == 1], na.rm = T),
    `pass(15+)` = sum(expl[pass == 1], na.rm = T),
    `Expl Plays` = sum(expl, na.rm = T)
  ) |> 
  arrange(-`Expl Plays`) |> 
  gt() |> 
  data_color(
    columns = c(`rush(10+)`, `pass(15+)`, `Expl Plays`),
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
  tab_header(title = md("Most Explosive Plays allowed during 2023 season"),
             subtitle = "Explosive Plays = 10+ yard gain in run game or 15+ yard gain in pass game") |> 
  tab_source_note("Table: @arjunmenon100 | Data: nflreadr | Inspiration: @Marcus_Mosher")

gtsave(tbl, "expltbl.png")

pbp |> 
  filter(passer %in% c("T.Tagovailoa")) |> 
  group_by(passer, pass_location) |> 
  summarise(
    EPA = mean(epa, na.rm = T),
    successr = mean(success, na.rm = T),
    passs = sum(pass)
  )
  
  
  