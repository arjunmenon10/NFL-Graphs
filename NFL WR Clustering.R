#load in stats
WR_clustering <- read.csv(file = 'receiving_summary10.csv', TRUE, ",")

#WR1 <- read.csv(file = 'pc3_WR1.csv', TRUE, ",")

#WR1 <- WR1 %>% 
  filter(!is.na(cluster))

#filter targets of 50 or more
WR_clustering <- WR_clustering %>% 
  filter(position == "WR") %>% 
  filter(targets > 50)

#select stats to be used
WR_clustering <- WR_clustering %>% 
  select(player, position, team_name, yards_after_catch_per_reception, avg_depth_of_target, contested_receptions, drop_rate, slot_rate, yards_per_reception, yprr)

#stats without names
no_player_name <- WR_clustering %>% 
  select(-player, - position, -team_name) %>% 
  scale()

set.seed(222)
MAX_K <- 20
sse <- c()

for (k in 1:MAX_K) {
  algo_k <- kmeans(no_player_name, centers = k, nstart = 22, iter.max = 20)
  sse <- c(sse, algo_k$tot.withinss)
}

tibble(k = 1:MAX_K, SSE = sse) %>% 
  ggplot(aes(x = k , y = SSE))+
  geom_point(color = "red")+
  geom_line(color = "blue")+
  labs(x = "K", y = "SSE", title ="K means")+
  scale_x_continuous(breaks = seq(1, MAX_K, 1))+
  theme_fivethirtyeight()

tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse) + lead(sse, 2)) %>% 
  dplyr::filter(k < MAX_K-1) %>% 
  ggplot(aes(x = k , y = SSE_difference))+
  geom_point(color = "red")+
  geom_line(color = "blue")+
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title ="K means")+
  scale_x_continuous(breaks = seq(1, MAX_K, 1))+
  theme_fivethirtyeight()

set.seed(22)

K <- 6
kmeans6 <- kmeans(no_player_name, centers = K, nstart = 22, iter.max = 20)
kmcenters <- as.data.frame(kmeans6$centers)

kmcenters$cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4', 'Cluster 5', 'Cluster 6')

kmcenters <- kmcenters %>% 
  rename(c("YAC/R" = "yards_after_catch_per_reception",
           "ADOT" = "avg_depth_of_target",
           "Y/REC" = "yards_per_reception",
           "Yards/RR" = "yprr",
            "slot rate" = "slot_rate",
            "drop rate" = "drop_rate",
            "contested receptions" = "contested_receptions")) %>% 
  pivot_longer(!cluster, names_to = "feature", values_to = "z_val")

kmcenters$feature <- factor(kmcenters$feature, levels = c('YAC/R', "ADOT", "Y/REC", "Yards/RR", "slot rate", "drop rate", "contested receptions"))

kmcenters$cluster <- factor(kmcenters$cluster, levels = c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4', 'Cluster 5', 'Cluster 6'))

player_clusters <- tibble(Cluster = kmeans6$cluster, player = WR_clustering$player, team = WR_clustering$team_name)

player_clusters <- player_clusters %>% 
  mutate(
    team = case_when(
      team == 'ARZ' ~ 'ARI',
      team == 'CLV' ~ 'CLE',
      team == 'HST' ~ 'HOU',
      team == 'BLT' ~ 'BAL',
      team == 'SD' ~ 'LAC',
      team == 'OAK' ~ 'LV',
      team == 'SL' ~ 'LA',
      TRUE ~ team
    )
  )

player_clusters <- player_clusters %>% 
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

TM_stats <- pc3 %>% 
  filter(cluster == 3)

TM_stats <- TM_stats %>% 
  left_join(WR_clustering, by = c('player' = 'player'))

kmcenters %>% 
  ggplot(aes(x = feature, y = z_val, color = cluster))+
  geom_point(size = 3)+
  scale_color_brewer(palette = "Set1")+
  gghighlight(use_direct_label = "FALSE")+
  facet_wrap(~ cluster, ncol = 3)+
  theme_fivethirtyeight()+
  labs(
       title = "Each Cluster's Features",
       caption = "By Arjun Menon | @arjunmenon100 | Data: PFF") + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=9),
        panel.grid.minor = element_blank())+
  theme(axis.title = element_text()) + ylab('Cluster Center') + xlab("Feature")+
  theme(plot.title = element_markdown(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5))
ggsave('cluster_features.png', height = 12, width = 16, dpi = 300)

pca <- prcomp(no_player_name)
pca_summary <- summary(pca)  

pc2 <- as.data.frame(pca$x[, 1:2])
pc2$cluster <- as.factor(kmeans6$cluster)
cluster1_var <- round(pca_summary$importance[2, 1], 4) * 100
cluster2_var <- round(pca_summary$importance[2, 2], 4) *100

pc2 <- pc2 %>% 
  arrange(cluster)

player_clusters <- player_clusters %>% 
  arrange(Cluster)

pc3 <- cbind(pc2, player_clusters)

write.csv(pc3, "pc4.csv")

pc3$cluster <- as.factor(pc3$cluster)

pc3 %>% 
  ggplot(aes(x=PC1, y=PC2, shape = cluster)) + 
  geom_point(aes(), color = pc3$team_color, fill = pc3$team_color2, shape = 21, size = 2) +
  geom_text_repel(label = pc3$player, box.padding = 0.05, size = 3, max.overlaps = 3)+
  theme_fivethirtyeight() +
  theme(legend.position = "none")+
  geom_text(aes(x=PC1, y=PC2, label = as.integer(cluster)), nudge_x = 0.13, nudge_y = -0.13, size = 3) +
  scale_shape_identity() +
  labs(xlab = paste0("PC1 (Accounts for 36.51% of Variance)"),
       ylab = paste0('PC2 (Accounts for 23.28% of Variance)'), 
       title = 'Cluster Analysis of NFL WR in 2020',
       subtitle = "Player's cluster is the number next to their name",
       caption = "By Arjun Menon | @arjunmenon100 | Data: PFF") +
  theme(axis.title = element_text()) 

  ggplot(data = WR1) + 
  geom_image(mapping = aes(x = cluster , y = List), image = WR1$team_logo_espn, asp = 16/9) +
  geom_text_repel(mapping = aes(x = cluster , y = List + 1), label = WR1$player, size = 5.5, nudge_y = 0.3)+
  theme_fivethirtyeight() +
  labs(xlab = "Cluster", 
       y = "",
       title = "Which cluster does every team's WR1 fall under",
       subtitle = "",
       caption = "By Arjun Menon | @arjunmenon100 | Data: PFF") + 
    theme(axis.title = element_text()) + ylab('') + xlab("Cluster")+
    theme(axis.text.y = element_blank())+
    theme(axis.text.x = element_text(size = 12))+
    theme(plot.title = element_markdown(size = 22, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = 12, hjust = 0.5))+
    scale_x_continuous(breaks = scales::pretty_breaks(n=6))+  
    scale_y_continuous(breaks = scales::pretty_breaks(n=10))
  ggsave('WR1cluster.png', height = 12, width = 16, dpi = 300)
  
  pc4 <- pc3 %>% 
    select(player, team_wordmark, cluster)
  
pc4 <- pc4 %>%  select(player, team_wordmark, cluster)

ggplot(data = pc3, aes(x=PC1, y=PC2, color=cluster, shape=cluster)) + 
  geom_point(alpha=0.3) + 
  geom_text_repel(aes(label = player)) +
  scale_color_brewer(palette="Dark2") +
  geom_rug() + 
  theme_minimal() + stat_ellipse(level=(2/3)) + 
  scale_shape_manual(values=seq(0,15)) + 
  theme_fivethirtyeight() +
  labs(title = "NFL  Wide Receiver Clusters",
       subtitle = 'Receivers drafted in the 1st or 2nd round since 2016 and all draft elgible receivers this year',
       caption = 'By Arjun Menon | @arjunmenon100 | Data:PFF') + 
  theme(axis.title = element_text()) + ylab('PC2 (Accounts for 23.28% of Variance)') + xlab("PC1 (Accounts for 36.51% % of Variance)")+
  theme(axis.text.y = element_blank())+
  theme(axis.text.x = element_text(size = 12))+
  theme(plot.title = element_markdown(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5))+
  theme(legend.position = "none") 

devtools::install_github("gadenbuie/ggpomological")

library(ggthemes)
    

WR1_table2 <- pc4 %>% 
      gt() %>% 
      text_transform(
        locations = cells_body(c(team_wordmark)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>% 
      data_color(
        columns = c(Cluster),
        colors = scales::col_numeric(
          palette = c("#F28E2B", "#F1CE63", "#86BCB6"),
          domain = NULL
        )
      ) %>% 
      fmt_number(
        columns = c(Cluster),
        decimals = 0
      ) %>% 
      cols_align(
        align = "right",
        columns = c(Cluster)
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
            columns = Cluster
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
            columns = Cluster
          )
        )
      ) %>% 
      cols_label(
        player = "Player",
        Cluster = "Cluster",
        team_wordmark = ""
      ) %>%
      tab_source_note("Table: @arjunmenon100 | Data: PFF") %>%
      tab_header(
        title = md("NFL WRs and their cluster"),
        subtitle = ""
      ) %>% 
      gt_theme_538(table.width = px(550))
    
    write.csv(pc4, "pc4.csv")
    pc4 <- read.csv(file = 'pc4.csv', TRUE, ",")
  
    gtsave(WR1_table, "WRFirstHalf.png")
    gtsave(WR1_table1, "WRSecondHalf.png")
    gtsave(WR1_table2, "WRThirdHalf.png")
  
    img1 <- magick::image_read("WRFirstHalf.png")
    img3 <- magick::image_read("WRSecondHalf.png")
    img2 <- magick::image_read("WRThirdHalf.png")
    
    img4 <- magick::image_append(c(img1, img2, img3))
    img4
    
    image_write(img4, path = "WR_finaltable", format = "png")
    
   Tim <- TM_stats %>% 
      select(player, cluster, team_logo_espn, yards_after_catch_per_reception, avg_depth_of_target, contested_receptions, drop_rate, slot_rate, yards_per_reception, yprr) %>% 
      gt() %>% 
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>% 
      data_color(
        columns = c(yards_after_catch_per_reception, yards_per_reception, yprr, avg_depth_of_target, contested_receptions, drop_rate, slot_rate),
        colors = scales::col_numeric(
          palette = as.character(paletteer::paletteer_d("ggsci::green_material", n = 10)),
          domain = NULL
        )
      ) %>% 
      fmt_number(
        columns = c(yards_after_catch_per_reception, yards_per_reception, yprr, avg_depth_of_target, contested_receptions, drop_rate, slot_rate),
        decimals =2
      ) %>% 
      cols_align(
        align = "right",
        columns = c(yards_after_catch_per_reception, yards_per_reception, yprr, avg_depth_of_target, contested_receptions, drop_rate, slot_rate)
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
            columns = c(yards_after_catch_per_reception, yards_per_reception, yprr, avg_depth_of_target, contested_receptions, drop_rate, slot_rate)
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
            columns = c(yards_after_catch_per_reception, yards_per_reception, yprr, avg_depth_of_target, contested_receptions, drop_rate, slot_rate)
          )
        )
      ) %>% 
      cols_label(
        player = "Player",
        cluster = 'Cluster',
        yards_after_catch_per_reception = "YAC/R",
        yards_per_reception = "YPC",
        yprr = 'Yards per Route Run',
        avg_depth_of_target = 'ADOT',
        contested_receptions = 'contested receptions',
        slot_rate = 'Slot rate',
        drop_rate = 'Drop rate',
        team_logo_espn = ''
      ) %>%
      tab_source_note("Table: @arjunmenon100 | Data: PFF") %>%
      tab_header(
        title = md("NFL WRs and their cluster"),
        subtitle = "Cluster 3 players only"
      ) %>% 
      gt_theme_538(table.width = px(550))
    
   gtsave(Tim, "Tim.png")
   