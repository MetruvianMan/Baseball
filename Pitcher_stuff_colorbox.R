## PITCHER GT TABLE

library(tidyverse)
library(gt)
library(gtExtras)
library(mlbplotR)
library(janitor)
library(ggsci)
library(RCurl)

# Grab raw data from github (originally retrieved from FanGraphs on 4/19/24)
x <- getURL("https://raw.githubusercontent.com/MetruvianMan/Baseball/main/fangraphs-leaderboards-current-qualified.csv")

stuff_plus <- read.csv(text = x) %>% 
  janitor::clean_names() %>% 
  mutate(team = mlbplotR::clean_team_abbrs(team)) %>%
  rename(name = i_name) %>% 

# Left join to team logos in load_mlb_teams functions
  left_join(mlbplotR::load_mlb_teams() %>% select(team_abbr, team_logo_espn),
            by = c("team" = "team_abbr")) %>% 
# Bring in headshots from mlbplotR
   left_join(mlbplotR::load_headshots() %>% select(savant_id, espn_headshot),
            by = c("mlbamid" = "savant_id")
            ) %>% 
# Collapse curveball/knuckleball into same column
  mutate(stf_cu = if_else(!stf_kc %in% c(NA,0),
                          stf_kc,
                          stf_cu),
# Collapse forkball/splitter into same column
         stf_fs = if_else(!stf_fo %in% c(NA,0),
                          stf_fo,
                          stf_fs),
# Some headshots aren't available from mlbplotR so we manually add them
espn_headshot = case_when(mlbamid == 683003 & name == 'Jared Jones' ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/4918156.png&w=350&h=254",
                                   name == 'Keaton Winn' ~ "https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/5116843.png&w=350&h=254",
                                   .default = espn_headshot)
  ) %>% 
  select(team_logo_espn, espn_headshot,name, ip, stf_fa, stf_si, stf_fc, stf_fs,
         stf_sl, stf_cu, stf_ch, stuff
  ) %>% 
# rename columns for better readability in final table
  rename(logo = team_logo_espn,
         headshot = espn_headshot,
         fastball = stf_fa,
         sinker = stf_si,
         cutter = stf_fc,
         splitter = stf_fs,
         slider = stf_sl,
         curve = stf_cu,
         changeup = stf_ch) %>% 
# round stuff+ numbers to one decimal
  mutate(fastball = round(fastball,1),
         sinker = round(sinker,1),
         cutter = round(cutter,1),
         splitter = round(splitter,1),
         slider = round(slider,1),
         curve = round(curve,1),
         changeup = round(changeup,1),
         stuff = round(stuff,1)
  ) %>% 
# sort by stuff descending and slice the top 15 pitchers
  arrange(desc(stuff)) %>% 
  head(15)

# Construct gt table
stuff_plus %>% 
  gt() %>% 
  gt_theme_espn() %>%  
  gt_img_rows(logo, height = 25) %>% #add logos
  gt_img_rows(headshot, height = 25) %>% #add headshots
  cols_label(logo = md(""),
             headshot = md(""),
             stuff = md("Stuff+")
  ) %>%
  cols_align(
    align = "center",
    columns = c(5:11)) %>% 
# use color boxes for each pitch. Set domains manually for a legible gradient
  gt_color_box(columns = fastball, domain = 40:200, palette = "ggsci::blue_material") %>%
  gt_color_box(columns = sinker, domain = 10:200, palette = "ggsci::red_material") %>%
  gt_color_box(columns = cutter, domain = 80:150, palette = "ggsci::pink_material") %>%
  gt_color_box(columns = splitter, domain = 10:200, palette = "ggsci::green_material", na.color = "#FFFFFF") %>%
  gt_color_box(columns = slider, domain = 80:150, palette = "ggsci::teal_material") %>%
  gt_color_box(columns = curve, domain = 40:200, palette = "ggsci::purple_material") %>%
  gt_color_box(columns = changeup, domain = 10:160, palette = "ggsci::orange_material") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = stuff
    )
  ) %>% 
  tab_header(title = 'Top 15 Qualified Starters by Stuff+',
             subtitle = '2024 Regular Season (Thru 4/18)') %>% 
  tab_source_note(source_note = 'Data: FanGraphs | by: @Metruvian_Man')
