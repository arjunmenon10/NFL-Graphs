all_safeties <- read_csv("pff-data (71).csv")

all_safeties <- all_safeties |>
  filter(DEF...13 > 200) |> 
  group_by(Name) |> 
  mutate(
    idline_rate = IDL/DEF...13,
    edge_rate = EDGE/DEF...13,
    ilb_rate = ILB/DEF...13,
    ss_rate = SS/DEF...13,
    fs_rate = FS/DEF...13,
    scb_rate = SCB/DEF...13,
    cb_rate = CB/DEF...13,
    nonrun = DEF...13 - RUND,
    COVper = COV/nonrun,
    PSSRUSHper = PRSH/nonrun
  )

Final_allsafeties <- all_safeties %>% 
  select(Name, Team, POS, idline_rate, edge_rate, ilb_rate,
          ss_rate, fs_rate, scb_rate, cb_rate, COVper, PSSRUSHper)

nug = 10^(-3)

Final_allsafeties <- Final_allsafeties %>%
  mutate(shannon = 
           -idline_rate*log(idline_rate + nug, 2) - edge_rate*log(edge_rate + nug, 2) - ilb_rate*log(ilb_rate + nug, 2) - 
            ss_rate*log(ss_rate + nug, 2) - fs_rate*log(fs_rate + nug, 2) -
           scb_rate*log(scb_rate + nug, 2) - cb_rate*log(cb_rate + nug, 2) - COVper*log(COVper + nug, 2)- PSSRUSHper*log(PSSRUSHper + nug, 2) ) %>% 
  arrange(-shannon) %>% as.data.frame() -> all_years_safeties

all_years_safeties <- all_years_safeties |> 
  filter(POS == 'S')

pictures <- nflreadr::load_rosters(2022) |> 
  select(full_name, headshot_url) |> 
  mutate(full_name = ifelse(full_name == 'Jessie Bates', 'Jessie Bates III', full_name))

all_years_safeties <- left_join(all_years_safeties, pictures,
                                by = c('Name' = 'full_name'))

entropytbl <- all_years_safeties |> 
  arrange(-shannon) |> 
  mutate(rank = rank(-shannon),
         shannon = round(shannon, 3)) |>
  filter(Name %in% c('Xavier McKinney', 'Kyle Dugger', 'Grant Delpit', 'Antoine Winfield Jr.', 
                     'Jeremy Chinn', 'Brandon Jones', 'Terrell Burgess', 'Jaylinn Hawkins', 
                     "L'Jarius Sneed", 'Kenny Robinson', 'Daniel Thomas', 'Antoine Brooks', 'Kamren Curl',
                     'Jordan Fuller')) |> 
  ungroup() |> 
  select(rank, Name, Team, headshot_url, shannon) |> 
  gt() |> 
  cols_label(
    headshot_url = "",
    shannon = "Shannon Entropy Score") %>%
  data_color(
    columns = c(shannon),
    colors = scales::col_numeric(
      palette = c("light green", "#76b7b2"),
      domain = NULL
    )
  ) %>%
  gt_img_rows(headshot_url) |> 
  opt_align_table_header(align = "center") %>%
  cols_align("center") %>%
  opt_row_striping() %>%
  gt_theme_espn() |> 
  tab_header(title = md("Most versatile free agent safeties from 2020 NFL Draft (out of 123 safeties)"),
             subtitle = "Shannon Entropy: Measure of predictability | Higher entropy score = more versatile & line up in different spots more often") |> 
tab_source_note("Table & Data: PFF")
gtsave(entropytbl, "entropytbl20.png")

CB_names <- CB_names %>% 
  left_join(all_safeties1, by = c('Name' = 'player'))

Final_allCB <- CB_names %>% 
  select(Name, Team, POS, Box, DLine, `Wide Corner`, `Slot Corner`, `Free Safety`, total_snaps)

Final_allCB <- Final_allCB %>% 
  mutate(
    box_rate = Box/total_snaps,
    dline_rate = DLine/total_snaps,
    widecorner_rate = `Wide Corner`/total_snaps,
    slotcorner_rate = `Slot Corner`/total_snaps,
    freesafety_rate = `Free Safety`/total_snaps
  )

Final_allCB <- Final_allCB %>% 
  select(Name, Team, POS, box_rate, dline_rate, slotcorner_rate, widecorner_rate, freesafety_rate)

nug = 10^(-3)

Final_allCB <- Final_allCB %>%
  mutate(shannon = 
           -box_rate*log(box_rate + nug, 2) - dline_rate*log(dline_rate + nug, 2) - slotcorner_rate*log(slotcorner_rate + nug, 2) - 
           widecorner_rate*log(widecorner_rate + nug, 2) - freesafety_rate*log(freesafety_rate + nug, 2)) %>% 
  arrange(-shannon) %>% as.data.frame() -> all_years_CB

all_years_CB <- all_years_CB %>% 
  filter(!Name %in% c('Minkah Fitzpatrick', 'Kareem Jackson', 'Eric Rowe', 'Budda Baker'))

safeties_table <- all_years_safeties %>% 
  select(Name, Team, POS, shannon) %>% 
  head(10) %>% 
  left_join(pictures, by = c("Name" = "full_name")) %>% 
  slice(-10)

CB_table <- all_years_CB %>% 
  select(Name, Team, POS, shannon) %>% 
  head(10) %>% 
  left_join(pictures, by = c("Name" = "full_name"))


S_tbl <- safeties_table %>%
  select(Name, headshot_url, POS, shannon) %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(c(headshot_url)),
    fn = function(x){
      web_image(
        url = x,
        height = px(55)
      )
    }
  ) %>% 
  data_color(
    columns = c(shannon),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d("ggsci::blue_material", n = 10)),
      domain = NULL
    )
  ) %>% 
  fmt_number(
    columns = c(shannon),
    decimals = 2
  ) %>% 
  cols_align(
    align = "center",
    columns = c(Name, headshot_url, POS, shannon)
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(Name, POS, shannon)
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = c(Name, headshot_url, POS, shannon)
      )
    )
  ) %>% 
  cols_label(
    Name = "Safety Name",
    headshot_url = "",
    POS = "Position",
    shannon = "Shannon Entropy Score"
  ) %>%
  tab_source_note("Table: @arjunmenon100 | Data: PFF") %>%
  tab_header(
    title = md("Top 10 Safeties Shannon Entropy Score since 2018"),
    subtitle = "Jabrill Peppers has had the highest safety score since 2018"
  ) %>% 
  gt_theme_538(table.width = px(550))

CBs_tbl <- CB_table %>%
  select(Name, headshot_url, POS, shannon) %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(c(headshot_url)),
    fn = function(x){
      web_image(
        url = x,
        height = px(55)
      )
    }
  ) %>% 
  data_color(
    columns = c(shannon),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d("ggsci::blue_material", n = 10)),
      domain = NULL
    )
  ) %>% 
  fmt_number(
    columns = c(shannon),
    decimals = 2
  ) %>% 
  cols_align(
    align = "center",
    columns = c(Name, headshot_url, POS, shannon)
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(Name, POS, shannon)
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = c(Name, headshot_url, POS, shannon)
      )
    )
  ) %>% 
  cols_label(
    Name = "Cornerback Name",
    headshot_url = "",
    POS = "Position",
    shannon = "Shannon Entropy Score"
  ) %>%
  tab_source_note("Table: @arjunmenon100 | Data: PFF") %>%
  tab_header(
    title = md("Top 10 Cornerbacks Shannon Entropy Score since 2018"),
    subtitle = "Tramon Williams has had the highest Cornerback score since 2018"
  ) %>% 
  gt_theme_538(table.width = px(550))

gtsave(S_tbl, "S_tbl.png")
gtsave(CBs_tbl, "CB_tbl.png")

imgS <- magick::image_read("S_tbl.png")
imgCB <- magick::image_read("CB_tbl.png")
imgboth <- magick::image_append(c(imgS, imgCB))
image_write(imgboth, path = "entropy_final_tbl", format = "png") 



