####################################
### SPRING TRAINING SUPERLATIVES ###
####################################

library(tidyverse)
library(readr)
library(janitor)
library(mlbplotR)
library(gt)
library(gtExtras)
library(baseballr)
library(emoji)
library(readxl)

# As a first step, pull the superlatives.xlsx file (saved to github)
# Statcast data was populated manually in excel after pulling 2023 data
 # from a local SQL database
# Link: https://github.com/MetruvianMan/Baseball

superlatives %>% 
  # join headshots and logos from mlbplotR
  left_join(mlbplotR::load_headshots() %>% select(savant_id, espn_headshot), 
            by = c("Player_Id" = "savant_id")) %>% 
  left_join(mlbplotR::load_mlb_teams() %>% select(team_name, team_logo_espn),
            by = c("Team" = "team_name")) %>%
  select(-Team, -Player_Id) %>% 
  relocate(espn_headshot, .before = 1) %>%
  relocate(team_logo_espn, .before = 1)

# Manually populate headshots that were not present in mlbplotR
superlatives[3,2] = 'https://img.mlbstatic.com/mlb-photos/image/upload/c_fill,g_auto/w_360/v1/people/695549/headshot/milb/current'
superlatives[5,2] = 'https://img.mlbstatic.com/mlb-photos/image/upload/c_fill,g_auto/w_360/v1/people/801639/headshot/milb/current'
superlatives[6,2] = 'https://images.sidearmdev.com/resize?url=https%3a%2f%2fdxbhsrqyrr690.cloudfront.net%2fsidearm.nextgen.sites%2fgopoly.com%2fimages%2f2023%2f1%2f16%2fBryce_Warrecker_-_01809_copy.jpg&width=300&type=jpeg'
superlatives[8,2] = 'https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/4722599.png&w=350&h=254'
superlatives[9,2] = 'https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/3960910.png&w=350&h=254'

# Manually populate emojis using emoji package
superlatives[1,5] = emoji::emoji('world_map')
superlatives[2,5] = emoji::emoji('rocket')
superlatives[3,5] = emoji::emoji('fire')
superlatives[4,5] = emoji::emoji('rainbow')
superlatives[5,5] = emoji::emoji('muscle')
superlatives[6,5] = emoji::emoji('gloves')
superlatives[7,5] = emoji::emoji('cyclone')
superlatives[8,5] = emoji::emoji('fuelpump')
superlatives[9,5] = emoji::emoji('playground_slide')
superlatives[10,5] = emoji::emoji('giraffe')
superlatives[11,5] = emoji::emoji('ant')
superlatives[12,5] = emoji::emoji('airplane')
superlatives[13,5] = emoji::emoji('statue_of_liberty')

# Concatenate emoji and superlative text
superlatives <- mutate(superlatives, Superlative = paste(Emoji,Superlative)) %>% 
  select(-Emoji, -Date)

# Construct gt table
superlatives %>% 
  gt() %>% 
  gt_theme_espn() %>%  
  gt_img_rows(team_logo_espn, height = 30) %>% #add headshots to two columns
  gt_img_rows(espn_headshot, height = 30) %>%
  cols_label(team_logo_espn = md(""),
             espn_headshot = md(""),
             Notes_Honorable_Mentions = md("Notes / Honorable Mention")
  ) %>%
  cols_align(
    align = "center",
    columns = c(2)) %>% 
  tab_header(title = 'Spring Training Statcast Superlatives',
             subtitle = 'Single Pitch or Batted Ball Events through 3/18/24') %>% 
  tab_source_note(source_note = 'Data: Baseball Savant via baseballr | by: @Metruvian_Man') # %>% 
  gtsave("Spring_Training_Superlatives.png", expand = 50)
