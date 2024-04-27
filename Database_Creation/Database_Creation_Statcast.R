# DATABASE CREATION

################################
### WRITE STATCAST DATA ###
################################

library(tidyverse)
library(baseballr)
library(janitor)
library(RSQLite)

# create function to pull all game data
get_all_game_data <- function(game_list, complete = TRUE) {
  if(complete == TRUE) {
    game_list = game_list %>%
      filter(status %in% c("Final","Completed Early"))
  } else {
    #if the game status is still "Scheduled" it wasn't played
    game_list = game_list %>% filter(status != "Scheduled")
  }	
  n = nrow(game_list)
  
  print(n)
  
  return_game_list = list()
  
  for (i in 1:n) {
    game_pk = game_list$game_pk[i]
    
    game_info = paste0(
      game_list$game_pk[i], " - ", 
      game_list$home_team[i], " vs. ",
      game_list$away_team[i]
    )
    
    cat(game_info, sep="\n")
    
    df = try(get_pbp_mlb(game_pk))
    
    if (!is.null(df) & is.data.frame(df)) {
      
      return_game_list[[i]] = df
    }
  }
  game_df = dplyr::bind_rows(return_game_list)
  return(game_df)
}

## pull all games for a specific year
games = mlb_schedule(2024, 1) %>%
  rename(
    home_team = teams_home_team_name,
    away_team = teams_away_team_name,
    status = status_abstract_game_state
  ) %>%
  # filter on regular season games from a specified time period
  filter(game_type == 'R', game_date >= '2024-03-20', game_date != Sys.Date())

# Run get_all_game_data function on the 'games' dataframe
df <- get_all_game_data(games, TRUE) %>% 
  as.data.frame() %>% 
  janitor::clean_names() %>%
  
  # The below columns were included in game_pk's prior to 4/22/24 and were
  # subsequently removed circa late April, so we have to handle this difference.
  # As somewhat of a hack, we set these fields equal to zero, which adds 
  # them if they don't exist and modifies them if they do exist. We then delete 
  # them. If we don't add them first, then the code to delete them below 
  # will error out. This issue can probably be handled better w/ a conditional 
  # function later in time.
  
  dplyr::mutate(year = 2024,review_details_is_overturned_x = 0, 
                review_details_in_progress_x = 0, review_details_review_type_x = 0,
                review_details_is_overturned_y = 0,review_details_in_progress_y = 0,
                review_details_review_type_y = 0, review_details_challenge_team_id_x = 0,
                review_details_challenge_team_id_y = 0, count_balls_x = 0, 
                count_strikes_x = 0, count_outs_x = 0, count_balls_y = 0,count_strikes_y = 0, 
                count_outs_y = 0, review_details_additional_reviews = 0) %>%
  
  # delete unneeded/duplicate columns
  select(-c(review_details_is_overturned_x, review_details_in_progress_x,
            review_details_review_type_x,review_details_is_overturned_y,
            review_details_in_progress_y,review_details_review_type_y,
            review_details_challenge_team_id_x,review_details_challenge_team_id_y,
            count_balls_x, count_strikes_x, count_outs_x, count_balls_y,
            count_strikes_y, count_outs_y,review_details_additional_reviews,
            matchup_post_on_first_link, matchup_post_on_second_link,
            matchup_post_on_third_link
  )
  ) %>% 
  
  # Re-sequence first ~50 columns to front-load most frequently used columns,
  # then use everything() to add all other columns in their default order
  select(home_level_id,home_level_name,batting_team,fielding_team,game_pk,
         game_date,at_bat_index,index,pitch_number,last_pitch_of_ab,
         type,play_id, about_inning,about_half_inning,matchup_pitcher_id,
         matchup_pitcher_full_name,matchup_batter_id,matchup_batter_full_name,
         matchup_splits_pitcher, matchup_splits_batter, details_type_description,
         batted_ball_result,result_type,result_event,result_event_type,result_description,
         details_description, details_event,details_code,details_call_description,
         count_balls_start,count_strikes_start,count_outs_start,count_balls_end,
         count_strikes_end,count_outs_end,details_type_code,pitch_data_start_speed,
         pitch_data_breaks_spin_rate,hit_data_launch_angle,hit_data_total_distance,
         hit_data_launch_speed,everything()
  ) %>% 
  
  # Sort data by game_date, game_pk, at_bat_index,index
  arrange(game_date, game_pk, at_bat_index,index) %>% 
  
  # The count_balls_start and count_strikes_start columns are not accurate in the
  # raw data as they represent the count *after* the pitch catalogued in any
  # given row. We rename columns and then address this issue:
  
  rename(count_balls_after_pitch = count_balls_start,
         count_strikes_after_pitch = count_strikes_start) %>% 
  mutate(count_balls_before_pitch = case_when(
    index == 0 ~ 0,
    type == 'action' ~ count_balls_after_pitch,
    .default = lag(count_balls_after_pitch, n=1)
  ),
  count_strikes_before_pitch = case_when(
    index == 0 ~ 0,
    type == 'action' ~ count_strikes_after_pitch,
    .default = lag(count_strikes_after_pitch, n=1)
  )
  ) %>% 
  relocate(count_balls_before_pitch:count_strikes_before_pitch,
           .before = count_balls_after_pitch) %>% 
  relocate(year, .before = 1)

# Write data to SQL table using RSQLite package
sdb <- dbConnect(drv = SQLite(), "statcast_db.sqlite")
dbWriteTable(conn = sdb, name = "statcast_data", value = df, overwrite = T)

# pull sample data using SQL to test database connection and setup
df_test <- dbGetQuery(conn = sdb, "SELECT * FROM statcast_data
                        WHERE year = 2024 
                        LIMIT 1000
                        ") 

### IMPORTANT ###
 # Since we are creating a database and writing data to it for the
 # first time, we use "overwrite = T". If we were adding to an existing database,
 # we would use "overwrite = F, append = T". For example, if we want to add the 
 # prior day's game the morning after they are played, we would change the 
 # date in line 57 to yesterday's date and then run the following two lines:
 # dbWriteTable(conn = sdb, name = "statcast_data", value = df, overwrite = F,
 #              append = T)

# When done writing or using our database, we disconnect from it.
dbDisconnect(sdb)
