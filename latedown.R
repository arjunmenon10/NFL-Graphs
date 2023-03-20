library(nflreadr)
library(tidyverse)
library(nflfastR)

pbp <- nflreadr::load_pbp(2011:2022)

latedown <- pbp |> 
  filter(down %in% c(3, 4) & ydstogo >= 7, pass == 1) |> 
  group_by(id) |> 
  summarise(passer_name = first(name),
           EPAthr = mean(epa[qb_scramble == 0], na.rm = T),
           EPAsc = mean(epa[qb_scramble == 1], na.rm = T),
           passes = sum(pass, na.rm = T),
           scrambles = sum(qb_scramble, na.rm = T),
           team_abbr = last(posteam)) |> 
  filter(passes > 300, scrambles >= 10) |> 
  left_join(teams_colors_logos, by = 'team_abbr')

latedown |> 
  ggplot(aes(x = EPAthr, y = EPAsc))+
  geom_point(aes(size = scrambles), fill = latedown$team_color, color = 'black', shape = 21)+
  geom_text_repel(aes(label = passer_name), color = 'black', size = 5.5)+
  geom_vline(xintercept = mean(latedown$EPAthr), lty = 'dashed', color = 'red')+
  geom_hline(yintercept = mean(latedown$EPAsc), lty = 'dashed', color = 'red')+
  theme_fivethirtyeight()+
  labs(title = "Quarterback EPA/play on 3rd/4th and 7+ yards broken down by throws/sacks and scrambles",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: nflreadr',
       subtitle = glue::glue("2011-2022 | Minimum 300 dropbacks & 10 scrambles in such situations | Dot size = # of scrambles"))+
  theme(axis.title = element_text(size = 18)) + xlab('EPA/play when throwing or taking a sack') + ylab("EPA/play when scrambling")+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text = element_text(size = 17))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 17, hjust = 0.5),
        legend.position = 'none')+
  scale_y_continuous(breaks = scales::pretty_breaks(n=8))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=8))
ggsave('qbripya.png', width = 14, height = 10, dpi = "retina")

