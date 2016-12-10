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
  summarize(
    goals = n()) %>%
  arrange(desc(goals))
top_n(3, wt = goals)

# Top 3 subtypes for normal goals
Goals %>%
  group_by(subtype) %>%
  filter(type == "normal") %>%
  summarize(
    goals = n()) %>%
  arrange(desc(goals))
top_n(3, wt = goals)


















