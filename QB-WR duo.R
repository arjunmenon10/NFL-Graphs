library(nflfastR) 
library(ggimage)
library(gtExtras)
library(tidyverse)

pbp <- load_pbp(2021)

pictures <- nflfastR::fast_scraper_roster(2021) %>% 
  select(full_name, headshot_url, gsis_id)

passcatchduo <- pbp %>% 
  filter(pass == 1) %>% 
  filter(posteam %in% c('CIN', 'LA')) %>% 
  filter(!is.na(receiver_player_name)) %>% 
  group_by(passer_player_id, receiver_player_id) %>% 
  summarise(
    EPA_target = round(sum(epa, na.rm = T), 2),
    EPA_mean = round(mean(epa, na.rm = T), 2),
    targets = sum(pass)
  ) %>% 
  left_join(pictures, by = c('passer_player_id' = 'gsis_id')) %>% 
  rename(headshotQB = 'headshot_url', fullnameQB = 'full_name') %>% 
  left_join(pictures, by = c('receiver_player_id' = 'gsis_id')) %>% 
  ungroup()

passcatchduotbl <- passcatchduo %>% 
  arrange(desc(EPA_target)) %>% 
  head(10) %>% 
  select(fullnameQB, headshotQB, full_name, headshot_url, EPA_target, EPA_mean, targets) %>% 
  gt() %>% 
  gt_theme_538() %>% 
  gt_img_rows(headshotQB) %>% 
  gt_img_rows(headshot_url) %>%   
  cols_align(
    align = "center",
    columns = c(fullnameQB, headshotQB, full_name, headshot_url, EPA_target, EPA_mean, targets)
  ) %>% 
  cols_label(
    fullnameQB = "QB",
    headshotQB = "",
    full_name = 'Receiver',
    headshot_url = "",
    EPA_target = "Total EPA",
    EPA_mean = "Average EPA",
    targets = 'Targets') %>% 
  tab_header(
    title = "Top QB-Receiver Duos in the Super Bowl",
    subtitle = "All Regular Season and Postseason plays included"
  ) %>% 
  tab_source_note("Table: @arjunmenon100 | Data: NFLFastR")
gtsave(passcatchduotbl, "passcatchduotbl.png")
