scheme_data <- read.csv(file = 'scheme_data.csv', TRUE, ",")
NFL_Coaches <- read.csv(file = 'NFL_Coaches.csv', TRUE, ",")

def_playcallers <- NFL_Coaches %>%
  mutate(playcaller = ifelse(DPC == 1, HC, DC)) %>%
  select(season, franchise_id, playcaller)

coverage_data <- scheme_data %>%
  filter(rps == "P") 

coverage_data <- coverage_data %>%
  select(defense, defense_franchise_id, distance, down, game_id, offense,
         offense_franchise_id, quarter, season, week, defense_personnel, 
         blitz, num_pass_rush_players, coverage_scheme, mofo_coverage_played)

coverage_data %>%
  group_by(coverage_scheme) %>%
  summarize(plays = n()) %>%
  arrange(desc(plays))

common_coverages <- c("3", "1", "2", "4", "6", "3S", "2M", "0")

coverage_data <- coverage_data %>%
  filter(coverage_scheme %in% common_coverages) %>%
  mutate(zone = ifelse(coverage_scheme %in% c("6", "3", "4", "3s", "2"), 1, 0))

coverage_data <- coverage_data %>%
  left_join(def_playcallers, by = c("defense_franchise_id" = "franchise_id", "season"))

coverage_data <- coverage_data %>%
  mutate(offense = case_when(
    offense == "SD" ~ "LAC",
    offense == "BLT" ~ "BAL",
    offense == "OAK" ~ "LV",
    offense == "HST" ~ "HOU",
    offense == "SL" ~ "LA",
    offense == "CLV" ~ "CLE", 
    offense == "ARZ" ~ "ARI",
    TRUE ~ offense
  )) %>%
  ungroup()

coverage_data <- coverage_data %>%
  mutate(defense = case_when(
    defense == "SD" ~ "LAC",
    defense == "BLT" ~ "BAL",
    defense == "OAK" ~ "LV",
    defense == "HST" ~ "HOU",
    defense == "SL" ~ "LA",
    defense == "CLV" ~ "CLE", 
    defense == "ARZ" ~ "ARI",
    TRUE ~ defense
  )) %>%
  ungroup()

colSums(is.na(coverage_data))

write.csv(coverage_data, "coverage_data.csv")

coverage_season_rates <- coverage_data %>%
  group_by(defense, season, playcaller) %>%
  summarize(plays = n(),
            rate_zone = sum(zone) / plays,
            rate_man = 1 - rate_zone)
  
  coverage_season_join <- coverage_data %>%
  group_by(defense, season, playcaller) %>%
  count(coverage_scheme) %>%
  pivot_wider(
    id_cols = c(defense, season, playcaller),
    names_from = coverage_scheme,
    values_from = n
  )
coverage_season_join[is.na(coverage_season_join)] <- 0

coverage_season_rates <- coverage_season_rates %>%
  left_join(coverage_season_join, by = c("defense", "season", "playcaller"))

coverage_season_rates <- coverage_season_rates %>%
  mutate(rate_0 = `0` / plays,
         rate_1 = `1` / plays,
         rate_2 = `2` / plays,
         rate_2M = `2M` / plays,
         rate_3 = `3` / plays,
         rate_3S = `3S` / plays,
         rate_4 = `4` / plays,
         rate_6 = `6` / plays) %>%
  select(-c(`0`, `1`, `2`, `2M`, `3`, `3S`, `4`, `6`)) %>%
  ungroup()

X <- coverage_season_rates %>% 
  select(starts_with("rate")) %>%
  scale()  

set.seed(2011)
MAX_K <- 20 # max number of clusters
sse <- c() # vector to hold SSE of each model

for (k in 1:MAX_K) {
  algo_k <- kmeans(X, centers=k, nstart=22, iter.max=20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
} 

tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title = "An Even Clearer Picture") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_fivethirtyeight()+
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

set.seed(2014)
# re-run K-Means with 6 clusters
K <- 6
kmeans6 <- kmeans(X, centers=K, nstart=22, iter.max=20)
km_centers <- as.data.frame(kmeans6$centers) # SCALED cluster centers/means  

# name clusters before pivoting
km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4', 'Cluster 5', 'Cluster 6') 

# massage data
km_centers <- km_centers %>%
  rename(c('ZONE'='rate_zone', 'MAN'='rate_man', # give predictors a shorter name for plotting
           '0'='rate_0', '1'='rate_1',
           '2'='rate_2', '2M'='rate_2M',
           '3'='rate_3', '3S'='rate_3S', '4'='rate_4',
           '6'='rate_6')) %>% 
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier  

km_centers$feature <- factor(km_centers$feature, levels=c('ZONE', 'MAN', '0', '1', 
                                                          '2','2M', '3', '3S', 
                                                          '4', '6'))   

km_centers$Cluster <- factor(km_centers$Cluster, levels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 
                                                          'Cluster 4', 'Cluster 5', 'Cluster 6'))


km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point(size = 5) + # plot points
  scale_color_brewer(palette="Dark2") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ Cluster, ncol=3) + # create seperate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "K-Means Cluster Makeups of Coverage Schemes") + 
  theme_minimal() + theme_fivethirtyeight() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black", hjust = 0.5))
ggsave('coverage-1.png', width = 14, height = 10, dpi = "retina")

coverage_clusters <- tibble(cluster=kmeans6$cluster, defense=coverage_season_rates$defense, season=coverage_season_rates$season, playcaller = coverage_season_rates$playcaller) 

pca <- prcomp(X) # perform Principle Component Analysis 
pca_summary <- summary(pca) # summary of PCA model

pc2 <- as.data.frame(pca$x[,1:2]) # extract first two PCs
pc2$Cluster <- as.factor(kmeans6$cluster) # add player clusters 
cluster1_var <- round(pca_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_var <- round(pca_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

coverage_clusters <- cbind(coverage_clusters, pc2)

distance <- function(a, b) sqrt((a - 0)^2 + (b - 0)^2)
coverage_clusters <- coverage_clusters %>%
  group_by(defense, season) %>%
  mutate(unique_level = distance(PC1, PC2))

coverage_clusters <- coverage_clusters %>%
  arrange(season) %>%
  group_by(defense) %>%
  mutate(next_playcaller = lead(playcaller),
         playcaller_change = ifelse(next_playcaller == playcaller, "Same Scheme", "Different Scheme"),
         next_unique_level = lead(unique_level))

coverage_clusters <- coverage_clusters %>%
  left_join(teams_colors_logos, by = c("defense" = "team_abbr"))

coverage_clusters_2020 <- coverage_clusters %>%
  filter(season == 2020)

coverage_clusters_2019 <- coverage_clusters %>%
  filter(season == 2019)

coverage_clusters_2018 <- coverage_clusters %>%
  filter(season == 2018)

coverage_clusters_2017 <- coverage_clusters %>%
  filter(season == 2017)

coverage_clusters_2020 <- coverage_clusters_2020 %>%
  left_join(coverage_season_rates, by = c("defense", "season"))

coverage_clusters_2019 <- coverage_clusters_2019 %>%
  left_join(coverage_season_rates, by = c("defense", "season"))

coverage_clusters_2018 <- coverage_clusters_2018 %>%
  left_join(coverage_season_rates, by = c("defense", "season"))

coverage_clusters_2017 <- coverage_clusters_2017 %>%
  left_join(coverage_season_rates, by = c("defense", "season"))

coverage_clusters_2018 <- coverage_clusters_2018 %>% 
  head(10) %>% 
  arrange(desc(unique_level)) %>% 
  ungroup()

cluster_groups <- coverage_clusters_2020 %>%
  group_by(Cluster)
  
  
  
  coverage_clusters_2020 %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  geom_text(aes(x=PC1, y=PC2, label = as.integer(cluster)), nudge_x = 0.18, nudge_y = -0.18) +
  labs(x = paste0('PC2 (Accounts for ', cluster1_var, '% of Variance)'), # define cluster 1 % of variance
       y = paste0('PC1 (Accounts for ', cluster2_var, '% of Variance)'), # define cluster 2 % of variance
       title = 'Clustering Coverages for the 2020 NFL Season',
       subtitle = "Text next to each team's logo is the cluster they're apart of") + 
  theme_fivethirtyeight() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
ggsave('coverage-2.png', width = 14, height = 10, dpi = "retina")  

coverage_clusters %>%
  filter(!is.na(playcaller_change)) %>%
  ggplot(aes(x = unique_level, y = next_unique_level)) +
  geom_point(aes(color = playcaller_change), size = 4, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "gray", size = 2) +
  theme_fivethirtyeight() +
  labs(x = "Coverage Uniqueness in Year N",
       y = "Coverage Uniqueness in Year N + 1",
       title = "How Coverage Uniqueness Changed Based on Playcaller Changes",
       subtitle = "A playcaller change has a strong effect on a team") +
  facet_wrap(~playcaller_change) +
  theme(strip.text.x = element_text(size = 12, colour = "black", hjust = 0.5, face = "bold"))
ggsave('coverage-3.png', width = 14, height = 10, dpi = "retina")  

coverage_clusters_2020 %>%
  ggplot(aes(x = reorder(defense, unique_level), y = unique_level)) +
  geom_bar(aes(color = team_color2, fill = team_color), position="stack", stat="identity", alpha = 0.90) +
  geom_image(aes(image = team_logo_espn, y = unique_level + 0.15), asp = 16/9, size = 0.035) +
  scale_color_identity(aesthetics =  c("fill", "color")) +
  theme_fivethirtyeight() +
  labs(x = "Team",
       y = "Coverage Scheme Uniqueness",
       title = "Each Defense's Coverage Scheme Uniqueness, 2020",
       subtitle = "Uniqueness is determined by distance from the average scheme cluster") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_line( size=.1, color="gray"),
        panel.grid.major.y = element_line(size = .1, color = "gray"))
ggsave('coverage-4.png', width = 14, height = 10, dpi = "retina")  

#############################################################################
coverage_games_rates <- coverage_data %>%
  group_by(defense, season, game_id, week) %>%
  summarize(plays = n(),
            rate_zone = sum(zone) / plays,
            rate_man = 1 - rate_zone)

coverage_games_join <- coverage_data %>%
  group_by(defense, season, game_id, week) %>%
  count(coverage_scheme) %>%
  pivot_wider(
    id_cols = c(defense, season, game_id, week),
    names_from = coverage_scheme,
    values_from = n
  )
coverage_games_join[is.na(coverage_games_join)] <- 0

coverage_games_rates <- coverage_games_rates %>%
  left_join(coverage_games_join, by = c("defense", "season", "game_id", "week"))

coverage_games_rates <- coverage_games_rates %>%
  mutate(rate_0 = `0` / plays,
         rate_1 = `1` / plays,
         rate_2 = `2` / plays,
         rate_2M = `2M` / plays,
         rate_3 = `3` / plays,
         rate_3S = `3S` / plays,
         rate_4 = `4` / plays,
         rate_6 = `6` / plays) %>%
  select(-c(`0`, `1`, `2`, `2M`, `3`, `3S`, `4`, `6`)) %>%
  ungroup()

X2 <- coverage_games_rates %>% 
  select(starts_with("rate")) %>%
  scale()  

set.seed(2011)
MAX_K <- 20 # max number of clusters
sse <- c() # vector to hold SSE of each model

for (k in 1:MAX_K) {
  algo_k <- kmeans(X2, centers=k, nstart=22, iter.max=20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
} 

tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title = "An Even Clearer Picture") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() + theme_reach() + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

set.seed(1991)
# re-run K-Means with 6 clusters
K <- 6
kmeans6_2 <- kmeans(X2, centers=K, nstart=22, iter.max=20)
km_centers_2 <- as.data.frame(kmeans6_2$centers) # SCALED cluster centers/means  

# name clusters before pivoting
km_centers_2$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                          'Cluster 4', 'Cluster 5', 'Cluster 6') 

# massage data
km_centers_2 <- km_centers_2 %>%
  rename(c('ZONE'='rate_zone', 'MAN'='rate_man', # give predictors a shorter name for plotting
           '0'='rate_0', '1'='rate_1',
           '2'='rate_2', '2M'='rate_2M',
           '3'='rate_3', '3S'='rate_3S', '4'='rate_4',
           '6'='rate_6')) %>% 
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier  

km_centers_2$feature <- factor(km_centers_2$feature, levels=c('ZONE', 'MAN', '0', '1', 
                                                              '2','2M', '3', '3S', 
                                                              '4', '6'))   

km_centers_2$Cluster <- factor(km_centers_2$Cluster, levels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 
                                                              'Cluster 4', 'Cluster 5', 'Cluster 6'))

km_centers_2 %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point(size = 5) + # plot points
  scale_color_brewer(palette="Dark2") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ Cluster, ncol=3) + # create seperate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "K-Means Cluster Makeups of Coverage Schemes") + 
  theme_minimal() + theme_fivethirtyeight() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black", hjust = 0.5))

coverage_game_clusters <- tibble(cluster=kmeans6_2$cluster, 
                                 defense=coverage_games_rates$defense, 
                                 season=coverage_games_rates$season, 
                                 week = coverage_games_rates$week) 

pca_2 <- prcomp(X2) # perform Principle Component Analysis 

pc2_2 <- as.data.frame(pca_2$x[,1:2]) # extract first two PCs
pc2_2$Cluster <- as.factor(kmeans6_2$cluster) # add player clusters 
cluster1_var_2 <- round(pca_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_var_2 <- round(pca_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

coverage_game_clusters <- cbind(coverage_game_clusters, pc2_2)

coverage_game_clusters <- coverage_game_clusters %>%
  group_by(defense, season, week) %>%
  mutate(unique_level = distance(PC1, PC2))

coverage_game_clusters <- coverage_game_clusters %>%
  left_join(teams_colors_logos, by = c("defense" = "team_abbr")) %>%
  ungroup()

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

coverage_game_16 <- coverage_game_clusters %>%
  filter(season == 2016) %>%
  filter(week < 18) 

coverage_half_19 <- coverage_game_clusters %>% 
  filter(season == 2019) %>% 
  filter(week > 11) %>%
  group_by(defense) %>% 
  summarise(
    uniqueness = mean(unique_level, na.rm = T)
  )

rams_16 <- coverage_game_16 %>%
  filter(defense == "LA")

ggplot() +
  geom_point(data = coverage_game_16, aes(x = PC1, y = PC2), alpha = 0.10, size = 3) +
  geom_point(data = rams_16, aes(x = PC1, y = PC2, fill = ifelse(week > 14, team_color2, team_color), 
                                 color = ifelse(week > 14, team_color, team_color2)), alpha = 1.0, 
             size = 6, shape = 21) +
  theme_reach() +
  geom_text(data = rams_16, aes(x=PC1, y=PC2, label = as.factor(week)), nudge_x = 0.10, nudge_y = -0.10) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(x = "Coverage Variable PC1",
       y = "Coverage Variable PC2",
       title = "How the Rams Coverage Scheme Changed Throughout the 2016 Season",
       subtitle = "Jeff Fisher fired in week 14, bubble color switches from blue to gold to represent that")

ggplot() +
  geom_jitter(data = coverage_game_16, aes(x = week, y = unique_level), alpha = 0.15, width = 0.05, size = 2) +
  geom_smooth(data = rams_16, aes(x = week, y = unique_level, color = team_color2), se = FALSE, size = 3) +
  theme_reach() +
  geom_vline(xintercept = 8, linetype = "dashed") +
  labs(x = "Week",
       y = "Coverage Scheme Uniqueness",
       title = "2014 Raiders Coverage Scheme Uniqueness By Week",
       subtitle = "Compared to the rest of the NFL, the Raiders coverage schemes stayed the same after their coaching change") +
  scale_x_continuous(breaks = pretty_breaks(n = 17)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_identity() +
  annotate("text", x = 5.5, y = 0.95, label = "Coaching change", size = 5, color = "black") +
  annotate("curve", x = 5.5, y = 0.89, xend = 7.9, yend = 0.80, angle = 120,  
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) 


coverage_game_18 <- coverage_game_clusters %>%
  filter(season == 2018) %>%
  filter(week < 18) %>%
  filter(unique_level < 3) %>%
  filter(unique_level > 0.5)

browns_18 <- coverage_game_18 %>%
  filter(defense == "CLE")


ggplot() +
  geom_jitter(data = coverage_game_18, aes(x = week, y = unique_level), alpha = 0.15, width = 0.05, size = 2) +
  geom_smooth(data = browns_18, aes(x = week, y = unique_level), se = FALSE, color = "#fb4f14", size = 3) +
  theme_reach() +
  geom_vline(xintercept = 8, linetype = "dashed") +
  labs(x = "Week",
       y = "Coverage Scheme Uniqueness",
       title = "2018 Browns Coverage Scheme Uniqueness By Week",
       subtitle = "Compared to the rest of the NFL, the Browns coverage schemes got more unique after their coaching change") +
  scale_x_continuous(breaks = pretty_breaks(n = 17)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  annotate("text", x = 5.5, y = 0.935, label = "Coaching change", size = 5, color = "black") +
  annotate("curve", x = 5.5, y = 0.89, xend = 7.9, yend = 0.80, angle = 120,  
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) 
ggsave('coverage-5.png', height = 10, width = 14, dpi = "retina")  

coverage_game_20 <- coverage_game_clusters %>%
  filter(season == 2020) %>%
  filter(week < 18) 

lions_20 <- coverage_game_20 %>%
  filter(defense == "DET")

coverage_game_19 <- coverage_game_clusters %>% 
  filter(season == 2019) %>% 
  filter(week < 18)

chargers_19 <- coverage_game_19 %>% 
  filter(defense == "LAC")


ggplot() +
  geom_jitter(data = coverage_game_19, aes(x = week, y = unique_level), alpha = 0.15, width = 0.05, size = 2) +
  geom_smooth(data = chargers_19, aes(x = week, y = unique_level, color = team_color), se = FALSE, size = 3) +
  theme_fivethirtyeight() +
  geom_vline(xintercept = 12, linetype = "dashed") +
  labs(x = "Week",
       y = "Coverage Scheme Uniqueness",
       title = "2019 Chargers Coverage Scheme Uniqueness By Week",
       subtitle = "Compared to the rest of the NFL, the Chargers coverage schemes became one of the most unique following the return of Derwin James") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8))+  
  scale_y_continuous(breaks = scales::pretty_breaks(n=8))+
  theme(axis.title = element_text(size = 16)) + ylab("Coverage Unique Score") + xlab("Week")+
  theme(axis.text = element_text(size = 14))+
  theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 16, hjust = 0.5))+
  scale_color_identity() +
  annotate("text", x = 9.5, y = 0.935, label = "Derwin James returns from injury", size = 7, color = "black") +
  annotate("curve", x = 9.5, y = 0.89, xend = 11.9, yend = 0.80, angle = 120,  
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) 
ggsave('DerwinJamescov.png', width = 14, height = 10, dpi = "retina")

coaching_changes <- read.csv("~/Downloads/coaching_changes.csv")

coaching_changes <- coaching_changes %>%
  mutate(label = paste0(team, season)) %>%
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

coaching_changes %>%
  ggplot(aes(x = scheme_un_before, y = scheme_un_after)) +
  geom_point(shape = 21, aes(fill = team_color, color = team_color2), size = 7) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  geom_text(aes(x=scheme_un_before, y=scheme_un_after, label = label), nudge_x = 0.045, nudge_y = -0.045, size = 5.5) +
  theme_reach() +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(x = "Scheme Uniqueness Before Coaching Change",
       y = "Scheme Uniqueness After Coaching Change",
       title = "How Coverage Schemes Uniqueness Changes Based on Mid-Season Coaching Changes",
       subtitle = "Mid-season coaching changes are when HC and/or DC changes during the middle of the season") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10))
ggsave('coverage-6.png', width = 14, height = 10, dpi = "retina")

future::plan("multisession")
pbp <- nflfastR::load_pbp(2014:2020)

pass_def <- pbp %>% 
  filter(pass == 1) %>%
  filter(!is.na(epa)) %>%
  group_by(defteam, season) %>%
  summarize(pass_epa = mean(epa))

coverage_clusters <- coverage_clusters %>%
  left_join(pass_def, by = c("defense" = "defteam", "season"))

coverage_clusters_graph <- coverage_clusters %>%
  filter(season < 2020)

coverage_clusters_2020 <- coverage_clusters %>%
  filter(season == 2020)

ggplot() +
  geom_point(data = coverage_clusters_graph, aes(x = unique_level, y = pass_epa, 
                                                 fill = team_color, color = team_color2), shape = 21, alpha = 0.3, size = 4) +
  geom_image(data = coverage_clusters_2020, aes(x = unique_level, y = pass_epa, image = team_logo_espn), 
             asp = 16/9, size = 0.05) +
  theme_reach() +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_smooth(data = coverage_clusters, aes(x = unique_level, y = pass_epa), method = "lm", color = "gray", se = FALSE) +
  labs(x = "Coverage Scheme Uniquness",
       y = "EPA/Pass",
       title = "More Unique Coverage Schemes Doesn't Mean Defenses are Better at Defending the Pass",
       subtitle = "Dots are for defenses 2014-2019, logos are for 2020") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_reverse(breaks = pretty_breaks(n = 10))
ggsave('coverage-7.png', width = 14, height = 10, dpi = "retina")

change <- coverage_game_clusters %>% 
  mutate(back = ifelse(week > 8, 1, 0)) %>% 
  group_by(defense, season, back) %>% 
  summarize(scheme_un = mean(unique_level))

change <- change %>%
  pivot_wider(
    id_cols = c(defense, season),
    names_from = back,
    values_from = scheme_un
  ) 

change <- change %>%
  mutate(change = `1` - `0`)

coverage_game_14 <- coverage_game_clusters %>%
  filter(season == 2014) %>%
  filter(week < 18) 

pats_14 <- coverage_game_14 %>%
  filter(defense == "NE")

pats <- coverage_game_clusters %>%
  filter(defense == "NE") %>%
  group_by(week, defense) %>%
  summarize(scheme_un = mean(unique_level)) %>%
  left_join(teams_colors_logos, by = c("defense" = "team_abbr"))

pats2 <- coverage_game_clusters %>%
  filter(defense == "NE")

ggplot() +
  geom_smooth(data = pats2, aes(x = week, y = unique_level, group = season), se = FALSE, color = "gray", alpha = 0.4) +
  geom_smooth(data = pats, aes(x = week, y = scheme_un, color = team_color), se = FALSE, size = 3) +
  scale_color_identity() +
  theme_reach() +
  geom_vline(xintercept = 17, linetype = "dashed") +
  labs(x = "Week",
       y = "Coverage Scheme Uniquiness",
       title = "Belichick's Coverage Schemes Get More Unique in the Playoffs",
       subtitle = "Each year from 2014-2020 is plotted with his average highlighted in blue") +
  scale_x_continuous(breaks = pretty_breaks(n = 16)) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  annotate("text", x = 19.5, y = 3.3, label = "Playoff Belichick's \n Yearly Average", size = 4, color = "black") +
  annotate("curve", x = 19.3, y = 3.15, xend = 19.5, yend = 2.7, angle = 120,  
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) 
ggsave('coverage-9.png', width = 14, height = 10, dpi = "retina")

pats_stats <- coverage_games_rates %>%
  #mutate(playoff = ifelse(week > 17, 1, 0)) %>%
  #filter(defense == "NE") %>%
  #group_by(playoff) %>%
  summarize(rate_zone = mean(rate_zone),
            rate_man = mean(rate_man),
            rate_0 = mean(rate_0),
            rate_1 = mean(rate_1),
            rate_2 = mean(rate_2),
            rate_2M = mean(rate_2M),
            rate_3 = mean(rate_3),
            rate_3S = mean(rate_3S),
            rate_4 = mean(rate_4),
            rate_6 = mean(rate_6))

pats_stats <- pats_stats %>%
  pivot_longer(!playoff, names_to = "coverages", values_to = "rates")

bell <- read.csv("~/Downloads/bell.csv")

bell <- bell %>%
  left_join(teams_colors_logos, by = c("team_abbr"))

bell_gt <- bell %>%
  mutate(diff_in_playoff = playoff_rate - non_playoff_rate) %>% 
  select(coverage, nfl_average_rate, team_wordmark, non_playoff_rate, playoff_rate, diff_in_playoff) %>%
  arrange(-diff_in_playoff)

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "middle",
      ...
    )}

bellichick_gt <- bell_gt %>% gt() %>%
  text_transform(
    locations = cells_body(c(team_wordmark)),
    fn = function(x){
      web_image(
        url = x,
        height = px(30)
      )
    }
  ) %>% 
  cols_label(
    coverage = "Coverage",
    nfl_average_rate = "NFL Average Rate",
    team_wordmark = "Team",
    non_playoff_rate = "Regular Season Rate",
    playoff_rate = "Playoff Rate",
    diff_in_playoff = "Difference") %>%
  data_color(
    columns = c(diff_in_playoff),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>% 
  opt_align_table_header(align = "center") %>%
  tab_header(
    title = md("**How Belichick's Coverages Change in the Playoffs**")
  ) %>% 
  gt_theme_538(table.width = px(700))
bellichick_gt
gtsave(bellichick_gt, 'bellichick_gt.png')

scheme_usage_bar <- scheme_data %>%
  select(defense, coverage_scheme, season) %>%
  filter(coverage_scheme %in% c("1","2","2M","3", "3S", "4", "6"), 
         defense %in% c("LA","NYJ"), 
         season == 2020) %>%
  group_by(defense) %>%
  mutate(total_plays = n()) %>%
  ungroup() %>%
  group_by(defense, coverage_scheme) %>%
  mutate(freq = n() / total_plays) %>%
  distinct() %>%
  left_join(teams_colors_logos, by = c("defense" = "team_abbr"))

scheme_usage_bar <- scheme_usage_bar %>%
  mutate(team_color2 = ifelse(defense == "NYJ", "#377363", team_color2))


nfl_usage_bar <- scheme_data %>%
  select(coverage_scheme, season) %>%
  filter(coverage_scheme %in% c("1","2","2M","3", "3S", "4", "6"), 
         season == 2020) %>%
  group_by(season) %>%
  mutate(league_plays = n()) %>%
  ungroup() %>%
  group_by(coverage_scheme) %>%
  mutate(league_freq = n() / league_plays) %>%
  distinct()

ggplot() + 
  geom_bar(data = scheme_usage_bar, aes(x = coverage_scheme, y = freq, fill = team_color2, color = team_color), 
           position="dodge", stat="identity") +
  geom_col(data = nfl_usage_bar, aes(x = coverage_scheme, y = league_freq), width = 0.5, alpha = 0.8, fill = "black") +
  facet_wrap(~defense) +
  theme_reach() +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(x = "Coverage Scheme",
       y = "Frequency",
       title = "How the Rams and Jets Coverage Frequencies Compared to the Rest of the NFL in 2020",
       subtitle = "Team colored bars to represent each team's coverage frequency with dark bars to represent the NFL's average") +
  theme(strip.text.x = element_text(size = 14, colour = "black", hjust = 0.5)) +
  annotate("text", x = 2.5, y = 0.33, label = "NFL Average") +
  annotate("curve", x = 2.27, y = 0.326, xend = 3.6, yend = 0.303, angle = 120,  
           curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) 
ggsave('coverage-10.png', width = 14, height = 10, dpi = "retina")