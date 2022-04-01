unique_entropy <- left_join(biggest_change, entropy_data, by = c('playcaller', 'season', 'defense'))

unique_entropy <- unique_entropy %>% 
  select(defense, season, playcaller, unique_level, shannon)

unique_entropy <- unique_entropy %>% 
  left_join(teams_colors_logos, by = c('defense' = 'team_abbr'))

unique_entropy <- unique_entropy %>% 
  ungroup()

unique_entropy %>% 
  select(shannon, unique_level) %>% 
  cor(use = "complete.obs") %>% 
  round(2)


ggplot(unique_entropy, aes(x = shannon, y = unique_level))+
  geom_point(fill = unique_entropy$team_color2, color = "black", size = 3, shape = 21)+
  geom_text_repel(aes(label = paste(defense,season)), color = "black", max.overlaps = 2)+
  geom_hline(yintercept = mean(unique_entropy$unique_level), color = "red", lty = "dashed")+
  geom_vline(xintercept = mean(unique_entropy$shannon), color = "red", lty = "dashed")+
  annotate(x = 1.5, y = 3.7, geom = "text", size = 6, hjust = "middle", label = "Very Unique Scheme \n Very Predictable", color = "blue")+
  annotate(x = 2.7, y = 4.9, geom = "text", size = 6, hjust = "middle", label = "Very Unique Scheme \n Very Unpredictable", color = "blue")+
  theme_fivethirtyeight()+
  labs(title = "Shannon Entropy vs Defensive Coverage Scheme Uniqueness",
       caption = 'By: Arjun Menon | @arjunmenon100 | Data: PFF',
       subtitle = glue::glue("2015-2019 | Most teams with a low entropy score happen to have ran some variation of the Seattle Cover 3"))+
  theme(axis.title = element_text(size = 18)) + ylab('Uniqueness Level') + xlab("Entropy")+
  theme(panel.grid.minor=element_blank())+
  theme(axis.text = element_text(size = 14))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 17, hjust = 0.5))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=6))+
  scale_y_continuous(breaks = scales::pretty_breaks(n=8))
ggsave('uniqueentropy.png', width = 14, height = 10, dpi = "retina")
