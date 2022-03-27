library(nflfastR)
library(tidyverse)
library(gridExtra)
library(gt)
library(scales)

#assign the seasons in Chark's career
seasons = 2018:2021

#load in play by play data over the course of Chark's career
seasons_of_chark = load_pbp(seasons)


#getting chark's player id
seasons_of_chark %>%
  filter(posteam == 'JAX', play_type == "pass", receiver_player_name == "D.Chark") %>%
  summarise(max(receiver_player_id))


#return a data frame of all Chark's targets throughout his career
all_chark_targets =
  seasons_of_chark %>%
  filter(!is.na(air_yards), receiver_player_id == '00-0034777')


#grouping chark targets by season (this only factors in the games in which he received at least one target)
seasonal_targets =
  all_chark_targets %>%
    group_by(season) %>%
    summarize(total_targs = n(), avg_air_yards = mean(air_yards))

#grab pfr + pff data
games_played = c(11, 15, 13, 4)

pff_rec_grade = c(51.6, 75.8, 71.2, 64.1)

#add games_play & rec_grade to seasonal_targets
seasonal_targets$games_played = games_played

seasonal_targets$rec_grade = pff_rec_grade


#lets see how many are in each category --> BLOS, short, medium, long
chark_targets_depth =
  all_chark_targets %>%
    mutate(target_depth = 
             case_when(
      air_yards < 0 ~ 'BLOS',
      air_yards >= 0 & air_yards < 10 ~ 'Short',
      air_yards >= 10 & air_yards < 20 ~ 'Medium',
      air_yards >= 20 ~ 'Long')) %>%
    group_by(season, target_depth) %>%
    summarize(area_targs = n()) %>%
    ungroup()


#joining seasonal targets to target depth targets
joined_chark =
  inner_join(chark_targets_depth, seasonal_targets, by = 'season')


#dividing area targets by total targets in each season to get a seasonal tgt percentage by target area
chark_final =
  joined_chark %>%
    mutate(tgt_pct = area_targs / total_targs) 



#chart of DJ Chark Target Percentage by Season & Area of the Field
chark_tgt_pct =
chark_final %>%
  ggplot(aes(season, tgt_pct)) + 
  geom_line(aes(group = target_depth, color = target_depth), size = 1) +
  theme_bw() + 
  labs(x = "Season",
       y = "Target Percentage", 
       title = "DJ Chark Target Percentage by Season & Area of the Field") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_color_discrete(name = "Target Depth")


#adding a targets per game played attribute
seasonal_targets =
  seasonal_targets %>%
    mutate(tgts_per_gm = total_targs / games_played)

#targets by game in each season column chart
chark_tgts_per_game =
seasonal_targets %>%
  ggplot(aes(season, tgts_per_gm)) +
  geom_col(fill = "#9F792C") +
  theme_bw() +
  labs(x = "Season",
       y = "Targets per Game",
       title = "DJ Chark Targets by Game in each Season") + 
  theme(plot.title = element_text(hjust = 0.5))

#games played graph
games_played_graph =
seasonal_targets %>%
  ggplot(aes(season, games_played)) +
  geom_col(fill = "#9F792C") +
  geom_label(aes(label = games_played), vjust = - 0.1) +
  theme_bw() +
  labs(x = "Season",
       y = "Games Played",
       title = "DJ Chark Games Played Throughout Career") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  ylim(0,16)
  

#pff receiving grade graph
pff_grade_graph =
seasonal_targets %>%
  ggplot(aes(season, pff_rec_grade)) +
  geom_col(fill = "#9F792C") +
  geom_label(aes(label = pff_rec_grade), vjust = - 0.25) +
  theme_bw() +
  labs(x = "Season",
       y = "PFF Grade",
       title = "DJ Chark PFF Receiving Grade by Season") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  ylim(0,100)

##########lions wr target pct by area of field###############

#data frame of total amount of targets by each Detroit Lion in 2021
all_lions_targets =
load_pbp(2021) %>%
  filter(posteam == "DET", play_type == "pass", !is.na(air_yards)) %>%
  group_by(receiver_player_id) %>%
  summarize(total_targets = n())

  

# lions targets by area of field in 2021
lions_targets =
load_pbp(2021) %>%
  filter(posteam == "DET", play_type == "pass", !is.na(air_yards)) %>%
  mutate(target_depth = 
           case_when(
             air_yards < 0 ~ 'BLOS',
             air_yards >= 0 & air_yards < 10 ~ 'Short',
             air_yards >= 10 & air_yards < 20 ~ 'Medium',
             air_yards >= 20 ~ 'Long')) %>%
  group_by(receiver_player_id, target_depth) %>%
  summarize(area_targets = n())

#data frame of all Detroit WRs in 2021
detroit_wrs =
  fast_scraper_roster(2021) %>%
    filter(position == "WR" & team == "DET") %>%
  select(gsis_id, first_name, last_name)

#joining depth targets of Lions in 2021 to data frame of all lions WRs in 2021
joined_lions_targets =
inner_join(lions_targets, detroit_wrs, by = c("receiver_player_id" = "gsis_id"))

#joining total targets per WR in 2021 
joined_lions_targets = 
  inner_join(joined_lions_targets, all_lions_targets, by = "receiver_player_id")

#adding area target percentage attribute per WR
joined_lions_targets =
  joined_lions_targets %>%
    mutate(area_tgt_pct = area_targets / total_targets) %>%
  select(receiver_player_id, last_name, target_depth, area_targets, area_tgt_pct, total_targets)
  
#creating a Chark data frame that can bind to joined_lions_targets
chark_stats =
  chark_final %>%
    group_by(target_depth) %>%
    summarize(area_targets = sum(area_targs)) %>%
    mutate(total_targets = sum(area_targets)) %>%
    mutate(area_tgt_pct = area_targets / total_targets,
           last_name = "Chark",
           receiver_player_id = "00-0034777") %>%
  select(receiver_player_id, last_name, target_depth, area_targets, area_tgt_pct, total_targets)

#binding charks career data to 2021 lions data
chark_w_lions =
rbind(chark_stats, joined_lions_targets)

###creating tables of each target depth to compare Chark to 2021 Lions WRs

#BLOS Table
blos_targets_table =
chark_w_lions %>%
  filter(target_depth == "BLOS") %>%
  select(last_name, target_depth, area_tgt_pct) %>%
  arrange(-area_tgt_pct) %>%
  mutate(area_tgt_pct = label_percent()(area_tgt_pct)) %>%
  gt(rowname_col = "last_name") %>%
  tab_header(
    title = "Percent of Targets - BLOS",
    subtitle =  "For Lions Receivers in 2021 & DJ Chark's Career") %>%
  cols_label(target_depth = "Target Area",
             area_tgt_pct = "Pct of Targets") %>%
  cols_width(
    last_name ~ 100,
    target_depth ~ 100,
    area_tgt_pct ~ 100
  ) %>%
  tab_style(locations = cells_body(
    columns = everything(),
    rows = "Chark"),
  style = list(cell_fill(color = "#9F792C"))
    )


#short table
short_targets_table =
chark_w_lions %>%
  filter(target_depth == "Short") %>%
  select(last_name, target_depth, area_tgt_pct) %>%
  arrange(-area_tgt_pct) %>%
  mutate(area_tgt_pct = label_percent()(area_tgt_pct)) %>%
  gt(rowname_col = "last_name") %>%
  tab_header(
    title = "Percent of Targets - Short",
    subtitle =  "For Lions Receivers in 2021 & DJ Chark's Career") %>%
  cols_label(target_depth = "Target Area",
             area_tgt_pct = "Pct of Targets") %>%
  cols_width(
    last_name ~ 100,
    target_depth ~ 100,
    area_tgt_pct ~ 100
  ) %>%
  tab_style(locations = cells_body(
    columns = everything(),
    rows = "Chark"),
    style = list(cell_fill(color = "#9F792C"))
  )


#medium table
medium_targets_table =
chark_w_lions %>%
  filter(target_depth == "Medium") %>%
  select(last_name, target_depth, area_tgt_pct) %>%
  arrange(-area_tgt_pct) %>%
  mutate(area_tgt_pct = label_percent()(area_tgt_pct)) %>%
  gt(rowname_col = "last_name") %>%
  tab_header(
    title = "Percent of Targets - Medium",
    subtitle =  "For Lions Receivers in 2021 & DJ Chark's Career") %>%
  cols_label(target_depth = "Target Area",
             area_tgt_pct = "Pct of Targets") %>%
  cols_width(
    last_name ~ 100,
    target_depth ~ 100,
    area_tgt_pct ~ 100
  ) %>%
  tab_style(locations = cells_body(
    columns = everything(),
    rows = "Chark"),
    style = list(cell_fill(color = "#9F792C"))
  )



#long table
long_targets_table =
chark_w_lions %>%
  filter(target_depth == "Long") %>%
  select(last_name, target_depth, area_tgt_pct) %>%
  arrange(-area_tgt_pct) %>%
  mutate(area_tgt_pct = label_percent()(area_tgt_pct)) %>%
  gt(rowname_col = "last_name") %>%
  tab_header(
  title = "Percent of Targets - Long",
  subtitle =  "For Lions Receivers in 2021 & DJ Chark's Career") %>%
  cols_label(target_depth = "Target Area",
             area_tgt_pct = "Pct of Targets") %>%
  cols_width(
    last_name ~ 100,
    target_depth ~ 100,
    area_tgt_pct ~ 100
  ) %>%
  tab_style(locations = cells_body(
    columns = everything(),
    rows = "Chark"),
    style = list(cell_fill(color = "#9F792C"))
  )

#lions WR pff receiving grades and number of games played
lions_wr_rec_grades = c(80.0, 60.6, 66.9, 66.7, 47.6, 70.9, 55.2)

lions_wr_games = c(17, 16, 7, 16, 8, 5, 12)

#creating a table with all games playes and targets for 2021 Lions WRs
joined_lions_targets %>%
  group_by(last_name) %>%
  summarize(total_targets = max(total_targets)) %>%
  arrange(-total_targets) %>%
  mutate(rec_grade = lions_wr_rec_grades,
         games_played = lions_wr_games) %>%
  mutate(targ_gm = round(total_targets / games_played, digits = 2)) %>%
  arrange(- targ_gm) %>%
  select(last_name, games_played, total_targets, targ_gm, rec_grade) %>%
  gt(rowname_col = "last_name") %>%
  tab_header(
    title = "Lions 2021 WRs",
    subtitle =  "For Lions Receivers in 2021") %>%
  cols_label(games_played = "Games Played",
             total_targets = "Total Targets", 
             targ_gm = "Targets per Game",
             rec_grade = "PFF Receiving Grade") %>%
  cols_width(
    last_name ~ 100,
    games_played ~ 125,
    total_targets ~ 125,
    rec_grade ~ 200,
    targ_gm ~ 150
  )














