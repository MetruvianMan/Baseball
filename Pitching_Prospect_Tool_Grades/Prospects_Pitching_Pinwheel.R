######################################################
#### TOP PROSPECTS AND TOOL GRADES PINWHEEL CHART ####
######################################################

# Github-friendly code
# SOURCE: The code for gathering the data (approx first 100 lines) is taken from
# Robert Frey's tutorial here: https://www.youtube.com/watch?v=54RKtA9yquM'

library(tidyverse)
library(janitor)
library(grid)
library(png)
library(reshape2)
library(forcats)
library(mlbplotR)
library(cropcircles)
library(ggimage)

## Since this plot relies primary on FanGraphs Stuff+ data, we first have to
 # export this data from "The Board" here: 
 # Link: https://www.fangraphs.com/prospects/the-board/2024-prospect-list

 # Therefore, the first step is retrieving and collating the right FG data
 # There are four separate files correspond to four tabs on the FG site:
     # 1) Summary
     # 2) Scouting - Position
     # 3) Scouting - Pitching
     # 4) Physical Attributes

# On my mac, I place these four .csv files in a folder labeled "TheBoardDataApril"
# and name the four files:
   # fangraphs-summary-the-board.csv
   # fangraphs-scouting-position-the-board-2.csv
   # fangraphs-scouting-pitching-the-board-3.csv
   # fangraphs-phys-attributes-the-board-4.csv

getwd()

# establish your working directory as the folder with the four above export files
setwd("/Users/username/Desktop/TheBoardDataApril")
files <- list.files(path = getwd(), pattern = ".csv")
temp <- lapply(files, read_csv)

#Pitch Data - We separate current and future ratings into distinct columns
pitch_data <- temp[[2]] %>% select(-Pos,-Org,-`Top 100`,-Age,-FV,-`Org Rk`) %>%
  rename(pitcher_name = Name) %>% 
  drop_na('FB Type') %>%
  separate(col = FB, into = c("Current_FB","Future_FB"), sep = "\\/") %>%
  separate(col = SL, into = c("Current_SL","Future_SL"), sep = "\\/") %>%
  separate(col = CB, into = c("Current_CB","Future_CB"), sep = "\\/") %>%
  separate(col = CH, into = c("Current_CH","Future_CH"), sep = "\\/") %>%
  separate(col = CMD, into = c("Current_CMD","Future_CMD"), sep = "\\/") %>%
  separate(col = Sits, into = c("Min_FB_Range","Max_FB_Range"), sep = "\\-") %>%
  mutate(across(everything(), str_squish)) %>%
  mutate_at(vars(Current_FB:Tops),as.numeric) %>%
  dplyr::filter(!is.na(PlayerId))

#Hit Data - Once again we separate current and future ratings
hit_data <- temp[[3]] %>% select(-Pos,-Org,-`Top 100`,-Age,-FV,-`Org Rk`) %>%
  rename(batter_name = Name) %>% 
  drop_na('Hit') %>%
  separate(col = Hit, into = c("Current_Hit","Future_Hit"), sep = "\\/") %>%
  separate(col = Game, into = c("Current_Game_Power","Future_Game_Power"), sep = "\\/") %>%
  separate(col = Raw, into = c("Current_Raw_Power","Future_Raw_Power"), sep = "\\/") %>%
  separate(col = Spd, into = c("Current_Spd","Future_Spd"), sep = "\\/") %>%
  separate(col = Fld, into = c("Current_Fld","Future_Fld"), sep = "\\/") %>%
  rename(Hard_Hit_Pct = `Hard Hit%`, Max_EV = `Max EV`, Avg_EV = `Avg EV`,
         Bat_Ctrl = `Bat Ctrl`, Pitch_Sel = `Pitch Sel`) %>%
  mutate(across(everything(), str_squish)) %>%
  mutate_at(vars(Current_Hit:Max_EV),as.numeric) %>%
  dplyr::filter(!is.na(PlayerId))

# load attribute export file
attribute_data <- temp[[1]] %>% select(-Name,-Pos,-Age)

# load summary export file
total_data <- temp[[4]] %>% dplyr::rename(Current_Level = `Current Level`,
                                          Org_Rk = `Org Rk`,
                                          Sign_Yr = `Sign Yr`,
                                          Sign_Mkt = `Sign Mkt`,
                                          Sign_Org = `Sign Org`,
                                          Signed_From = `Signed From`) %>%
# this line, which pulls video links of the featured players, is optional  
  dplyr::mutate(video = ifelse(is.na(Video),NA,paste0("https://www.youtube.com/watch?v=",Video))) %>%                            
  dplyr::filter(!is.na(PlayerId))

hitting_data_all <- left_join(total_data,hit_data,by="PlayerId") %>% 
  dplyr::filter(!is.na(batter_name)) %>%  
  left_join(attribute_data,by="PlayerId")

# We won't end up plotting pitching data, but it's still useful to bring it in
 # in case we want to work with it later or highlight any two-way players
 # such as Jac Caglianone

# We conduct a number of left joins to consolidate data across the four files
pitching_data_all <- left_join(total_data,pitch_data,by="PlayerId") %>% 
  dplyr::filter(!is.na(pitcher_name)) %>% 
  left_join(attribute_data,by="PlayerId")

all_data <- left_join(total_data,hit_data,by="PlayerId") %>% 
  left_join(pitch_data,by="PlayerId") %>% 
  left_join(attribute_data,by="PlayerId") %>% 
  janitor::clean_names()

# Now we read in Baseball Prospectus df with MLB id's. There may be duplicate 
# names which can throw off our later joins. We handle this later in the code
# by removing older players with same name, e.g. James Wood. We could also
# manage this by filtering the BP database

player_ids <- read.csv(url("https://legacy.baseballprospectus.com/sortable/playerids/playerid_list.csv"))

# Read in prospects dataset (assembled in code above and saved locally)
prospects <- all_data %>% 

# Take first 8 position players
  # We could pull in more players but it tends to make the plot less legible
  # and I recommend a max of 8 featured players

# Since this is a hitting-centric plot, we remove pitchers
filter(pos != 'SP') %>% 
  head(8) %>%  
  select(name, current_hit, future_hit, current_raw_power, future_raw_power,
         current_game_power, future_game_power,current_spd, future_spd,
         current_fld, future_fld, bat_ctrl, pitch_sel) %>% 
  dplyr::rename(current_bat_ctrl = bat_ctrl, current_pitch_sel = pitch_sel) %>% 
  
# Bat control and pitch selection tools don't have current and future components
# so for symmetry we create two separate grades that are equivalent to each other
  
  mutate(future_bat_ctrl = current_bat_ctrl,
         future_pitch_sel = current_pitch_sel)

# Now we use pivot_longer and pivot_wider to recast the data. Note the use of 
# regular expressionss proposed by a Stack Overflow poster here:
# Link: https://stackoverflow.com/questions/78078577/how-do-i-use-pivot-longer-and-or-pivot-wider-to-restructure-my-data/78078621#78078621

output <- prospects %>% 
  pivot_longer(-name, names_to = "names") %>% 
  mutate(tmp = sub("(\\w)_.*", "\\1", names),
         names = sub("current_|future_", "", names)) %>% 
  pivot_wider(names_from = tmp, values_from = value) %>%
  dplyr::rename(player = name)

# Use melt to further manipulate data

my_data_long <- reshape2::melt(output, id.vars = c("player","names")) %>% 
  
# Bring back some columns from prospects dataset now that data is wrangled
# There is a probably a way to retain this data without having to left_join it
# at this stage but I preferred to simplify the dataset earlier in the process
# and restore of the additional complexity later in the code
  
left_join(all_data %>% select(name, org, fv_number = fv, pos, top_100) %>% 
              mutate(org = clean_team_abbrs(org)), by = c("player" = "name")) %>% 
  
# Join on player_id dataset to get MLB ID's which are not in FanGraphs dataset
  
  left_join(player_ids %>% select(FIRSTNAME, LASTNAME, MLBCODE) %>% 
              mutate(player = paste(FIRSTNAME, LASTNAME)) %>% 
              select(-c(FIRSTNAME, LASTNAME)) %>% 
              rename(mlb_code = MLBCODE)) %>% 
  
# Left join to team logos in load_mlb_teams functions
left_join(mlbplotR::load_mlb_teams() %>% select(team_abbr, team_logo_espn),
            by = c("org" = "team_abbr")) %>% 
  
# Filter out older James Wood, leaving just the prospect James Wood
  
filter(mlb_code != c(595467)) %>% 
  
# Create column to populate headshot link from milb.com
  
mutate(headshot = paste0("https://img.mlbstatic.com/mlb-photos/image/upload/c_fill,g_auto/w_360/v1/people/",
                           mlb_code,"/headshot/milb/current")) %>% 
mutate(names = case_when(names == "pitch_sel" ~ "Pitch Selection",
                           names == "hit" ~ "Hit Tool",
                           names == "game_power" ~ "Game Power",
                           names == "raw_power" ~ "Raw Power",
                           names == "spd" ~ "Speed",
                           names == "fld" ~ "Field",
                           names == "bat_ctrl" ~ "BatCtrl")) %>% 
  
# Factor tools by specified order  
mutate(names = fct_relevel(names, c("Hit Tool","BatCtrl","Pitch Selection",
                                    "Raw Power","Game Power","Field","Speed")
                           )
       ) %>% 
  
# Factor players by their appearance on the Top 100 Prospects list, e.g. by FV value  
mutate(player = fct_reorder(paste0(player," (",org," ", pos, ", FV= ",fv_number,")"), 
                            top_100)
       )

# Create dataframe for player headshots. Use cropcircles package to clean up rectangular
# rectangular headshots and make them circular

images <- my_data_long |>
  distinct(player, headshot) |>
  mutate(
    img_circle = circle_crop(headshot, border_size = 12, 
                             border_colour = "black")
        )

# Create logo df
logos <- my_data_long |>
  distinct(player, team_logo_espn)

# Create emoji df for the seven tools
emoji_set <- my_data_long |> distinct(player) %>%  data.frame() %>% 
  mutate(muscle_image = "https://emojigraph.org/media/apple/flexed-biceps_1f4aa.png",
        glove_image = "https://emojigraph.org/media/apple/gloves_1f9e4.png",
        speed_image = "https://emojigraph.org/media/apple/person-running_1f3c3.png",
        hit_image = "https://emojigraph.org/media/apple/wood_1fab5.png",
        target_image = "https://emojigraph.org/media/apple/direct-hit_1f3af.png",
        game_power_image = "https://emojigraph.org/media/apple/man-lifting-weights_1f3cb-fe0f-200d-2642-fe0f.png",
        eye_image = "https://emojigraph.org/media/apple/eye_1f441-fe0f.png")

# Start ggplot
ggplot(data = my_data_long, aes(x = names, y = value, fill = variable,
                                color = variable, alpha = variable
                                )
      ) +
  
# Create custom geom_lines for concentric rings
geom_hline(aes(yintercept = y), data.frame(y = c(4:8) * 10),color = "lightgrey") +
geom_bar(stat = "identity", position = "identity") +
  
# Place headshots using images df
geom_image(
    data = images, aes(image = img_circle), x = 4, y = -40, size = .2,
    inherit.aes = FALSE
          ) +
  
# Place muscle emoji for raw power
geom_image(
    data = emoji_set, aes(image = muscle_image), x = 4, y = 15, size = .07,
    inherit.aes = FALSE
          ) +
  
# Place weightlifter emoji for game power
geom_image(
    data = emoji_set, aes(image = game_power_image), x = 5, y = 15, size = .07,
    inherit.aes = FALSE
            ) +
  
# Place glove emoji for fielding
geom_image(
    data = emoji_set, aes(image = glove_image), x = 6, y = 15, size = .07,
    inherit.aes = FALSE
  ) +
  
# Place runner emoji for speed
geom_image(
    data = emoji_set, aes(image = speed_image), x = 7, y = 15, size = .07,
    inherit.aes = FALSE
  ) +
  
# Place lumber emoji for hit tool
geom_image(
    data = emoji_set, aes(image = hit_image), x = 1, y = 15, size = .07,
    inherit.aes = FALSE
  ) +
  
# Place target emoji for bat control
geom_image(
    data = emoji_set, aes(image = target_image), x = 2, y = 15, size = .07,
    inherit.aes = FALSE
          ) +
  
# Place eye emoji for pitch selection
geom_image(
    data = emoji_set, aes(image = eye_image), x = 3, y = 15, size = .07,
    inherit.aes = FALSE
          ) +
  
# Place team logos in upper left hand corner 
geom_image(
    data = logos, aes(image = team_logo_espn), x = 6.63, y = 144, size = .11,
    inherit.aes = FALSE
          ) +
  
scale_colour_manual(values = c("lightblue4", "red")) +
scale_fill_manual(values = c("lightblue", "pink")) +
scale_alpha_manual(values = c(.8, .3)) +
scale_y_continuous(
    limits = c(-40, 80),
    expand = c(0, 0),
    breaks = c(40, 50, 60, 70, 80)
                  ) +
theme_bw() +
  
# Facet_wrap plot and specify four columns for best presentation  
facet_wrap(~player, ncol = 4) +
coord_polar() +
geom_text(
    data = data.frame(x = 4, y = c(40, 80)),
    aes(x = x, y = y, label = y),
    color = "gray12",
    family = "Trebuchet MS",
    inherit.aes = FALSE
          ) +
  
# Annotate 40 and 80 values on the plot 
annotate(geom = "text", x = 7.5, y = 40, label = "40", color = "gray12", family = "Trebuchet MS") +
annotate(geom = "text", x = 7.5, y = 80, label = "80", color = "gray12", family = "Trebuchet MS") +  
labs(
    title = "Top MLB Prospects and Tool Grades",
    subtitle = "20-80 Scouting Scale | Ranked by Overall Future Value (FV)",
    caption = "Data: FanGraphs | by: @Metruvian_Man"
  ) +
  
# Customize general theme
theme(
    text = element_text(color = "gray12", family = "Trebuchet MS"),
    # Customize the text in the title, subtitle, and caption
    plot.title = element_text(face = "bold", hjust = 0.0, size = 20),
    plot.subtitle = element_text(hjust = 0.0, size = 14),
    plot.caption = element_text(size = 10),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    
# Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
    )
