# HOW TO SCRAPE MLB API FOR GAME LOG DATA FOR HITTERS AND PITCHERS

# IMPORTANT: The below code requires first setting up a Statcast database as
# explained in the Database_Creation_Statcast.R file of this same folder.

# Sources: Most of code derives from Robert Frey's YouTube tutorial linked below
# https://www.youtube.com/watch?v=2WcYK_TFDHE

library(baseballr)
library(tidyverse)
library(jsonlite)
library(janitor)
library(purrr)

## mlb_game_logs2 function removes clean_names to reduce error probability
mlb_game_logs <- function(id,stat_group,year) {
  
  url <- paste0("http://statsapi.mlb.com/api/v1/people/",id,"/stats?stats=gameLog,statSplits&group=",stat_group,"&season=",year,"&language=en")
  
  resp <- url %>% baseballr:::mlb_api_call()
  
  df <- jsonlite::fromJSON(jsonlite::toJSON(resp[['stats']]), flatten = TRUE)[[2]][[1]] 
  
  df <- df %>% 
    tibble()
  
  df
}

alonso <- mlb_game_logs(624413,"hitting",2024)
gray <- mlb_game_logs(543243,"pitching",2024)
severino <- mlb_game_logs(622663,"pitching",2024)

## TO BUILD DATABASE OF 2023 GAME LOGS, FIRST GET ALL UNIQUE HITTER ID'S
 # Note we need to leverage the table we wrote in the Database_Creation_Statcast.R
 # and make sure it is up-to-date first

# Connect to database
sdb <- dbConnect(drv = SQLite(), "statcast_db.sqlite")

### HITTER GAME LOGS
# Pull all 2024 statcast hitters and pull game logs for each using map function
statcast <- dbGetQuery(conn = sdb, 
                       "SELECT DISTINCT matchup_batter_id FROM statcast_data")
unique_batters_24 <- unique(statcast$matchup_batter_id)
game_logs_24 <- map_dfr(unique_batters_24, \(x) mlb_game_logs(x,"hitting",2024),
                        .progress = T) %>% 
  select(-1) %>% 
  janitor::clean_names() %>% 
  select(-positions_played)

# write game_logs_hitting table
dbWriteTable(conn = sdb, name = "game_logs_hitting", value = game_logs_24, 
             overwrite = T)

# pull data using SQL to test database connection and setup
df_test <- dbGetQuery(conn = sdb, "SELECT * FROM game_logs_hitting")

# When done writing or using our database, we disconnect from it.
dbDisconnect(sdb)

### PITCHER GAME LOGS
  # We repeate the process now for pitchers instead of hitters

# Pull all 2024 statcast pitchers and pull game logs for each using map function
statcast <- dbGetQuery(conn = sdb, 
                       "SELECT DISTINCT matchup_pitcher_id FROM statcast_data
                       WHERE year = 2024")
unique_pitchers_24 <- unique(statcast$matchup_pitcher_id)
game_logs_pitchers <- map_dfr(unique_pitchers_24, \(x) mlb_game_logs(x,"pitching",2024),
                              .progress = T) %>% 
  select(-1) %>% 
  janitor::clean_names()

# write game_logs_pitching table
dbWriteTable(conn = sdb,name = "game_logs_pitching",value=game_logs_pitchers,
             overwrite = T)

# pull data using SQL to test database connection and setup
df_test <- dbGetQuery(conn = sdb, "SELECT * FROM game_logs_pitching") 

# When done writing or using our database, we disconnect from it.
dbDisconnect(sdb)


