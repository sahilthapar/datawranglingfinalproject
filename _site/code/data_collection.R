library("RSQLite")
library("dplyr")
library("readr")
library("tidyverse")
library("tibble")
library('stringr')
library('lubridate')

# Download the file to temp folder
temp <- tempfile()
download.file("https://kaggle2.blob.core.windows.net/datasets/63/589/database.sqlite.zip",
              temp, method = 'curl')

# Extract to get the sqlite file
data <- unzip(temp)
filename <- str_c(temp, str_sub(data, 2))

# Connect to the sqlite file
con = dbConnect(RSQLite::SQLite(), dbname = unzip(temp))
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
unlink(temp)

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

getResult <- function(home_goal, away_goal){
  return (ifelse(home_goal > away_goal,
                 'home',
                 (ifelse(home_goal == away_goal,
                         'draw', 'away'))
  ))
}



tidy_Match <- Match %>%
  mutate(season = (season %>% factor(levels = unique(season))),
         date = as.Date(date),
         stage = (stage %>% factor(levels = c(1:38))),
         result =  getResult(home_team_goal, away_team_goal)) %>% 
  mutate(result = factor(result, levels = c("home", "draw", "away"))) %>%
  select(matches("id|season|stage|date|result|team_goal|goal"))

tidy_Match %>%
  group_by(result) %>%
  summarize(perc = (n()/nrow(.)) * 100) 


# tmp <- str_match_all(x, "<goal_type>(.+?)</goal_type>")
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

