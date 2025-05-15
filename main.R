devtools::install_github("statsbomb/StatsBombR")
devtools::install_github("FCrSTATS/SBpitch")
library("StatsBombR")
library("tidyverse")

# 2015/2016 EPL
Comps <- FreeCompetitions()
Comps <- Comps %>%
  filter(competition_name == "Premier League" & season_name == "2015/2016")
Matches <- FreeMatches(Comps)
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = TRUE)
StatsBombData <- allclean(StatsBombData)
dim(StatsBombData)

# Summarize event types
nrows <- nrow(StatsBombData)
StatsBombData %>%
  group_by(type.name) %>%
  summarise(n = n(), prop = n / nrows) %>%
  arrange(desc(n))

# Add metric for forward passes
StatsBombData <- StatsBombData %>%
  mutate(
    is_forward_pass = case_when(
      type.name == "Pass" & pass.angle >= -pi/4 & pass.angle <= pi/4 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Filter to early/late games
early <- StatsBombData %>% filter(minute < 75)
late <- StatsBombData %>% filter(minute >= 75)

# Early team summary
early_team_summary <- early %>%
  group_by(match_id, team.name) %>%
  summarise(
    forward_pass_rate = sum(is_forward_pass) / sum(type.name == "Pass"),
    total_xg = sum(shot.statsbomb_xg, na.rm = TRUE),
    shot_count = sum(type.name == "Shot", na.rm = TRUE),
    poss_time_sec = sum(TimeInPoss, na.rm = TRUE),
    xg_per_min = (total_xg / poss_time_sec) * 60,
    shots_per_min = (shot_count / poss_time_sec) * 60,
    .groups = "drop"
  )

# Late team summary
late_team_summary <- late %>%
  group_by(match_id, team.name) %>%
  summarise(
    forward_pass_rate = sum(is_forward_pass) / sum(type.name == "Pass"),
    total_xg = sum(shot.statsbomb_xg, na.rm = TRUE),
    shot_count = sum(type.name == "Shot", na.rm = TRUE),
    poss_time_sec = sum(TimeInPoss, na.rm = TRUE),
    xg_per_min = (total_xg / poss_time_sec) * 60,
    shots_per_min = (shot_count / poss_time_sec) * 60,
    .groups = "drop"
  )

# Join early and late team summaries
total <- early_team_summary %>%
  left_join(late_team_summary, join_by(match_id == match_id, team.name == team.name))

# Add `park_the_bus` metric
total <- total %>%
  mutate(
    park_the_bus = case_when(
      xg_per_min.y < xg_per_min.x & 
        shots_per_min.y < shots_per_min.x & 
        forward_pass_rate.y < forward_pass_rate.x ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Quick sanity check
total %>% summarise(prop = sum(park_the_bus) / n())

# Total goals
goals_final <- StatsBombData %>%
  group_by(match_id, team.name) %>%
  summarise(goals_final = sum(type.name == "Shot" & shot.outcome.name == "Goal"),
            .groups = "drop")

# Goals at 75'
goals_75 <- early %>%
  group_by(match_id, team.name) %>%
  summarise(goals_75 = sum(type.name == "Shot" & shot.outcome.name == "Goal"),
            .groups = "drop")

# Join goals together
goals <- goals_75 %>%
  left_join(goals_final, join_by(match_id == match_id, team.name == team.name))

# Add late xG (last 15 minutes)
xG <- late %>%
  group_by(match_id, team.name) %>%
  summarise(xG_last15 = sum(shot.statsbomb_xg, na.rm = T), .groups = "drop")

# Join goals with xG
goals <- goals %>%
  left_join(xG, join_by(match_id == match_id, team.name == team.name))

# Add opponent goals and xG
goals <- goals %>%
  left_join(goals, join_by(match_id == match_id), suffix = c("",  "_opp")) %>%
  filter(team.name != team.name_opp)

# Join with `park_the_bus`
goals <- goals %>%
  left_join(total, join_by(match_id == match_id, team.name == team.name)) %>%
  select(match_id, team.name, goals_75, goals_final, xG_last15, goals_75_opp,
         goals_final_opp, xG_last15_opp, park_the_bus)

# Add game state
goals <- goals %>%
  mutate(game_state_75 = case_when(
    goals_75 > goals_75_opp ~ "Winning",
    goals_75 == goals_75_opp ~ "Drawing",
    goals_75 < goals_75_opp ~ "Losing"
    ), game_state_final = case_when(
      goals_final > goals_final_opp ~ "Win",
      goals_final == goals_final_opp ~ "Draw",
      goals_final < goals_final_opp ~ "Loss"
    )
  )

# Conceded xG vs. PTB
ggplot(goals, aes(x = as.factor(park_the_bus), y = xG_last15_opp, fill = game_state_75)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "Drawing" = "#619CFF",
      "Losing" = "#F8766D",
      "Winning" = "#00BA38"
    )
  ) +
  labs(
    x = "Parked the Bus?",
    y = "xG Conceded After 75th Minute",
    fill = "Game State at 75'",
    title = "xG Conceded Post-75' by Parking the Bus and Game State"
  ) +
  theme_minimal()

# Difference in xG vs. PTB
ggplot(goals, aes(x = as.factor(park_the_bus), y = xG_last15 - xG_last15_opp, fill = game_state_75)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(
      "Drawing" = "#619CFF",
      "Losing" = "#F8766D",
      "Winning" = "#00BA38"
    )
  ) +
  labs(
    x = "Parked the Bus?",
    y = "(xG - Opponent xG) After 75th Minute",
    fill = "Game State at 75'",
    title = "Difference in xG Post-75' by Parking the Bus and Game State"
  ) +
  theme_minimal()

# Is PTB good when already leading?
ggplot(goals %>% filter(game_state_75 == "Winning"), aes(x = as.factor(park_the_bus), fill = game_state_final)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Draw" = "#619CFF",
      "Loss" = "#F8766D",
      "Win" = "#00BA38"
    )
  ) +
  labs(
    x = "Parked the Bus?",
    y = "Proportion of Outcomes",
    fill = "Final Outcome",
    title = "Effect of Parking the Bus on Final Match Outcome when Leading at 75'"
  ) +
  theme_minimal()

# Is PTB good when tied?
ggplot(goals %>% filter(game_state_75 == "Drawing"), aes(x = as.factor(park_the_bus), fill = game_state_final)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Draw" = "#619CFF",
      "Loss" = "#F8766D",
      "Win" = "#00BA38"
    )
  ) +
  labs(
    x = "Parked the Bus?",
    y = "Proportion of Outcomes",
    fill = "Final Outcome",
    title = "Effect of Parking the Bus on Final Match Outcome when Tied at 75'"
  ) +
  theme_minimal()

# Is PTB good when losing?
ggplot(goals %>% filter(game_state_75 == "Losing"), aes(x = as.factor(park_the_bus), fill = game_state_final)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    values = c(
      "Draw" = "#619CFF",
      "Loss" = "#F8766D",
      "Win" = "#00BA38"
    )
  ) +
  labs(
    x = "Parked the Bus?",
    y = "Proportion of Outcomes",
    fill = "Final Outcome",
    title = "Effect of Parking the Bus on Final Match Outcome when Losing at 75'"
  ) +
  theme_minimal()
