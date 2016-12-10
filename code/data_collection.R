# @knitr library
library("RSQLite") # Connecting to RSQLite
library("dplyr")   # Working
library("readr")   # Reading and writing files
library("tidyverse") # For all the tidying functions from the tidy universe
library("tibble")    # For working with tibbles
library('stringr')   # Working with strings and regex
library('lubridate') # Working with dates
library('purrr')
library(XML)

# @knitr download_data
# Download the file to temp folder
temp <- tempfile()
download.file(
  "https://kaggle2.blob.core.windows.net/datasets/63/589/database.sqlite.zip?sv=2015-12-11&sr=b&sig=btA9gYGczWR6SFFq6rG84Eok8jazkwa8JNwfYKAGtAc%3D&se=2016-12-12T16%3A25%3A12Z&sp=r",
  temp,
  method = 'curl'
)

# Extract to get the sqlite file
data <- unzip(temp)
filename <- str_c(temp, str_sub(data, 2))

# Connect to the sqlite file
con = dbConnect(RSQLite::SQLite(), dbname = unzip(temp))
con = dbConnect(RSQLite::SQLite(), dbname = 'data/database.sqlite')
# get a list of all tables
dbListTables(con)

# Read all tables
myQuery <- dbSendQuery(con, "SELECT * FROM Player")
Player <- dbFetch(myQuery, n = -1)
myQuery <- dbSendQuery(con, "SELECT * FROM Country")
Country <- dbFetch(myQuery, n = -1)
myQuery <- dbSendQuery(con, "SELECT * FROM League")
League <- dbFetch(myQuery, n = -1)
myQuery <- dbSendQuery(con, "SELECT * FROM Player_Attributes")
Player_Attributes <- dbFetch(myQuery, n = -1)
myQuery <- dbSendQuery(con, "SELECT * FROM Match")
Match <- dbFetch(myQuery, n = -1)
myQuery <- dbSendQuery(con, "SELECT * FROM Team_Attributes")
Team_Attributes <- dbFetch(myQuery, n = -1)
myQuery <- dbSendQuery(con, "SELECT * FROM Team")
Team <- dbFetch(myQuery, n = -1)
# Clean up the environment
rm(temp, filename, data, con, myQuery)


# Find the country id for England's Premier League
England_id <- League %>%
  filter(name == "England Premier League") %>%
  select(id) %>%
  as.double()

# Filter the matches data for only England
Match <- Match %>%
  as_tibble %>%
  filter(league_id == England_id)

# Clean up data
# Match table has a season column - this should be a factor variable
# Also the date is only
# Result is defined as:
# if home_goal > away_goal : home
# else if home_goal == away_goal : draw
# else away

getResult <- function(home_goal, away_goal) {
  return (ifelse(home_goal > away_goal,
                 'home',
                 (
                   ifelse(home_goal == away_goal,
                          'draw', 'away')
                 )))
}



tidy_match <- Match %>%
  mutate(
    season = (season %>% factor(levels = unique(season))),
    date = as.Date(date),
    stage = (stage %>% factor(levels = c(1:38))),
    result =  getResult(home_team_goal, away_team_goal)
  ) %>%
  mutate(result = factor(result, levels = c("home", "draw", "away"))) %>%
  select(matches("id|season|stage|date|result|team_goal|goal"))

convertXMLtoDFAndAddMatchID <- function(goal_xml, match_id) {
  df <- xmlToDataFrame(goal_xml)
  if (nrow(df) > 0) {
    return(cbind(df, match_id = match_id))
  }
  return(df)
}

Goals <- tidy_match$goal %>%
  map2(tidy_match$match_api_id, convertXMLtoDFAndAddMatchID) %>%
  bind_rows() %>%
  as_tibble %>%
  transmute(
    type = fct_recode(
      comment,
      "normal" = "n",
      "penalty" = "p",
      "disallowed goal" = "dg",
      "own goal" = "o",
      "penalty saved" = "npm",
      "penalty missed" = "psm",
      "retake penalty" = "rp"
    ),
    elapsed = as.integer(elapsed),
    player1 = as.integer(player1),
    player2 = as.integer(player2),
    subtype = factor(subtype, levels = unique(subtype)),
    team = as.integer(team),
    match_id = match_id
  )

# tidy_match %>%
#   group_by(result) %>%
#   summarize(perc = (n() / nrow(.)) * 100)


# tmp <- str_match_all(x, "<goal_type>(.+?)<//goal_type>")
# tmp_goal_type <- unlist(sapply(tmp,
#                                   function(y){
#                                     return(str_c(y[,2], collapse = '|'))
#                                   }))
#
# tmp <- str_match_all(tidy_Match$goal, "<subtype>(.+?)</subtype>")
# tmp_goal_subtype <- unlist(sapply(tmp,
#                            function(y){
#                              return(str_c(y[,2], collapse = '|'))
#                              }))
# tidy_Match$goal_type <- tmp_goal_type
# tidy_Match$goal_subtype <- tmp_goal_subtype

summary(tidy_match$home_team_goal)
summary(tidy_match$away_team_goal)
summary(tidy_match$home_team_goal + tidy_match$away_team_goal)

# Top 3 goal types
Goals %>%
  group_by(type) %>%
  summarize(goals = n()) %>%
  arrange(desc(goals))
top_n(3, wt = goals)

# Top 3 subtypes for normal goals
Goals %>%
  group_by(subtype) %>%
  filter(type == "normal") %>%
  summarize(goals = n()) %>%
  arrange(desc(goals))
top_n(3, wt = goals)

getWinner <- function(home_team_id, away_team_id, result) {
  return(ifelse(
    result == 'draw',
    NA,
    ifelse(result == 'home', home_team_id, away_team_id)
  ))
}

tidy_match <-
  tidy_match %>%
  mutate(winner = getWinner(home_team_api_id, away_team_api_id, result))

tidy_match %>%
  left_join(Team, c("winner" = "team_api_id")) %>%
  select(matches("team_long_name")) %>%
  na.omit %>%
  group_by(Team = team_long_name) %>%
  summarize(wins = n()) %>%
  arrange(desc(wins)) %>%
  top_n(5, wt = wins)

# Top scoring teams
home_goals_scored <- tidy_match %>%
  group_by(home_team_api_id) %>%
  summarize(home_goals = sum(home_team_goal))

away_goals_scored <- tidy_match %>%
  group_by(away_team_api_id) %>%
  summarize(away_goals = sum(away_team_goal))

home_goals_scored %>%
  left_join(away_goals_scored, c("home_team_api_id" = "away_team_api_id")) %>%
  left_join(Team, c("home_team_api_id" = "team_api_id")) %>%
  select(c(team_long_name, home_goals, away_goals)) %>%
  mutate(total_goals = home_goals + away_goals) %>%
  arrange(desc(total_goals)) %>%
  top_n(5, wt = total_goals)

# Which team has allowed the most goals? At home ? Away ? Least ?
home_goals_allowed <- tidy_match %>%
  group_by(home_team_api_id) %>%
  summarize(home_goals_allowed = sum(away_team_goal))

away_goals_allowed <- tidy_match %>%
  group_by(away_team_api_id) %>%
  summarize(away_goals_allowed = sum(home_team_goal))

home_goals_allowed %>%
  left_join(away_goals_allowed,
            c("home_team_api_id" = "away_team_api_id")) %>%
  left_join(Team, c("home_team_api_id" = "team_api_id")) %>%
  select(c(team_long_name, home_goals_allowed, away_goals_allowed)) %>%
  mutate(total_goals_allowed = home_goals_allowed + away_goals_allowed) %>%
  arrange(desc(total_goals_allowed)) %>%
  top_n(5, wt = total_goals_allowed)

# * Which team scores the most goals in first sixth? Last Sixth ? Least?
findPhase <- function(time) {
  return(ifelse(time < 15, 1,
                ifelse(time < 30, 2,
                       ifelse(
                         time < 45, 3,
                         ifelse(time < 60, 4,
                                ifelse(time < 90, 5, 6))
                       ))))
}
Goals <- 
  Goals %>%
  filter(type %in% c("normal", "penalty", "own goal")) %>%
  mutate(phase = findPhase(elapsed))

Goals %>%  
  left_join(Team, c('team' = 'team_api_id')) %>%
  group_by(phase, team_long_name) %>%
  summarize(goals = n()) %>%
  arrange(phase, desc(goals)) %>%
  top_n(1)
  
Goals %>%
  filter(type == 'penalty') %>%
  left_join(Team, c('team' = 'team_api_id')) %>%
  group_by(team_long_name) %>%
  summarize(goals = n()) %>%
  arrange(desc(goals)) %>%
  top_n(5)

# * Which player has scored the most goals?

Goals %>%
  filter(type %in% c('penalty', 'normal')) %>%
  group_by(player = player1) %>%
  summarize(goals = n()) %>%
  left_join(Player, c('player' = 'player_api_id')) %>%
  select(c(player_name, goals)) %>%
  arrange(goals) %>%
  top_n(10, goals) %>%
  ggplot() +
  geom_bar(mapping = aes(x = reorder(player_name, goals),
                         y = goals),
           stat = "identity",
           fill = "#3498db",
           alpha = 0.9,
           width = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Player", y = "Goals") +
  ggtitle("Top 10 Scorers")
  
  
# Which player should you start with ? (scores the most goals in first sixth?)
# Which player should you end with ? (scores the most goals in last sixth?
Goals %>%
  group_by(phase, player = player1) %>%
  summarize(goals = n()) %>%
  left_join(Player, c('player' = 'player_api_id')) %>%
  select(c(player_name, goals, phase)) %>%
  arrange(goals) %>%
  top_n(2, goals) %>%
  ggplot() +
  geom_bar(mapping = aes(x = phase,
                         y = goals,
                         fill = player_name),
           stat = "identity",
           alpha = 0.9,
           position = 'dodge',
           width = 0.5) +
  scale_fill_manual(values = c("#2980b9", "#f1c40f", "#bdc3c7", "#8e44ad", "#e74c3c" )) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Phase", y = "Goals") +
  ggtitle("Top Scorers in phases")

# * Who is the best penalty taker ?

Goals %>%
  filter(type == "penalty") %>%
  group_by(player = player1) %>%
  summarize(goals = n()) %>%
  left_join(Player, c('player' = 'player_api_id')) %>%
  select(c(player_name, goals)) %>%
  arrange(goals) %>%
  top_n(5, goals) %>%
  ggplot() +
  geom_bar(mapping = aes(x = reorder(player_name, goals),
                         y = goals),
           stat = "identity",
           alpha = 0.9,
           fill = "#74AFAD",
           position = 'dodge',
           width = 0.25) +
  scale_fill_manual(values = c("#2980b9", "#f1c40f", "#bdc3c7", "#8e44ad", "#e74c3c" )) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Phase", y = "Goals") +
  ggtitle("Top scorers from penalties")
  
# * What is the distribution other type of goals other than shots ?

Goals %>%
  filter(type == 'normal', subtype != "shot") %>%
  group_by(subtype) %>%
  summarize(goals = n()) %>%
  na.omit %>%
  arrange(goals) %>%
  ggplot() +
  geom_bar(mapping = aes(x = reorder(subtype, goals),
                         y = goals),
           stat = "identity",
           alpha = 0.9,
           width = 0.5,
           fill = "#34495e",
           position = position_dodge(width = 1)) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Goal subtype", y = "Goals") +
  ggtitle("Goal subtype distribution")

# Do the tall players score more than the shorter players?
Goals %>%
  filter(type %in% c("normal", "penalty", "own goal")) %>%
  left_join(Player, c('player1' = 'player_api_id')) %>%
  select(c(player_name, height, player1)) %>%
  na.omit %>%
  mutate(height = ifelse(height > mean(height, na.rm = T), "tall", "short")) %>%
  group_by(height, player_name) %>%
  summarize(avg_goals = n()) %>%
  summarize(avg_goals = round(mean(avg_goals), 2))


# * What is the home advantage in the game of football ?
tidy_match %>%
  group_by(result) %>%
  summarize(percentage = round((n()/nrow(tidy_match)) * 100, 2))

list_players <- function(players){
  str_c(players, collapse = ',')
}

players_matches <-
  Match %>%
  select(home_player_1:away_player_11) %>%
  unite_(col = "players", from = names(.), sep = ',')

players_matches_goals <- 
  Goals %>%
  filter(type %in% c('penalty', 'normal')) %>%
  group_by(player = player1) %>%
  summarize(goals = n()) %>%
  ungroup() %>%
  mutate(matches = str_count(str_c(players_matches$players, collapse = ','),
                                 pattern = as.character(player)),
         highlight = (goals > 75)) %>%
  left_join(Player, c('player' = 'player_api_id')) %>%
  select(c(player_name, goals, matches, highlight))

ggplot(data = players_matches_goals) +
geom_point(mapping = aes(x = matches,
                         y = goals,
                         color = highlight),
           alpha = 0.85) +
scale_color_manual(values = c("#95a5a6", "#e74c3c")) +
geom_text(data = subset(players_matches_goals, highlight),
          aes(matches, goals, label = player_name),
          hjust = -0.05, vjust = 0) +
  theme_minimal() +
  labs(x = "Matches", y = "Goals") +
  guides(color = F) +
  ggtitle("Matches  vs Goals")

