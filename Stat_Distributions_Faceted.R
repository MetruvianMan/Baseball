# FACETED LEAGUE-WIDE STAT PLOTS

library(tidyverse)
library(baseballr)
library(ggtext)

# BATTING STATISTICAL CURVES (WITH DIFFERENT COLORED FACETS)

# Read in primary data with contact and swing decisions info
hitting <- read.csv(url("https://baseballsavant.mlb.com/leaderboard/custom?year=2023&type=batter&filter=&min=q&selections=pa%2Ck_percent%2Cbb_percent%2Cbatting_avg%2Cslg_percent%2Con_base_percent%2Con_base_plus_slg%2Cisolated_power%2Cwoba%2Cxwoba%2Csweet_spot_percent%2Cbarrel_batted_rate%2Chard_hit_percent%2Cavg_best_speed%2Cavg_hyper_speed%2Cz_swing_percent%2Coz_swing_percent%2Ciz_contact_percent%2Cwhiff_percent%2Cswing_percent%2Cgroundballs_percent%2Csprint_speed&chart=false&x=pa&y=pa&r=no&chartType=beeswarm&sort=xwoba&sortDir=desc&csv=TRUE"))

data <- hitting %>%
  select(last_name..first_name, player_id, k_percent, bb_percent, batting_avg, 
         whiff_percent, iz_contact_percent, xwoba, groundballs_percent,
         slg_percent, barrel_batted_rate, on_base_plus_slg,
         isolated_power, oz_swing_percent) %>% 
  mutate(full_name = sub("(^.*),\\s(.*$)","\\2 \\1", last_name..first_name), 
         .before = 1) %>% select(-last_name..first_name) 

# create data frame for median geom_vlines
data_vline <- data.frame(stat_name = c("AVG",
                                       "Barrel%",
                                       "BB%",
                                       "Groundball%",
                                       "ISO",
                                       "K%",
                                       "Chase%",
                                       "OPS",
                                       "SLG",
                                       "Whiff%",
                                       "XWOBA",
                                       "ZContact%"),
                         median = c(median(data$batting_avg),
                                   median(data$barrel_batted_rate),
                                   median(data$bb_percent),
                                   median(data$groundballs_percent),
                                   median(data$isolated_power),
                                   median(data$k_percent),
                                   median(data$oz_swing_percent),
                                   median(data$on_base_plus_slg),
                                   median(data$slg_percent),
                                   median(data$whiff_percent),
                                   median(data$xwoba),
                                   median(data$iz_contact_percent)
                                  ))
                         
# Swing% by pitch type
data %>%
  pivot_longer(
    k_percent:oz_swing_percent,
    names_to = "stat_name", # name column that will hold the names of the columns to stat_name
    values_to = "stat_value" # name column that will hold the values of the stat columns to stat_value
  ) %>%
  mutate(
    stat_name = case_match( # make stat names more presentable
      stat_name,
      "k_percent" ~ "K%",
      "bb_percent" ~ "BB%",
      "batting_avg" ~ "AVG",
      "whiff_percent" ~ "Whiff%",
      "xwoba" ~ "XWOBA",
      "iz_contact_percent" ~ "ZContact%",
      "groundballs_percent" ~ "Groundball%",
      "slg_percent" ~ "SLG",
      "barrel_batted_rate" ~ "Barrel%",
      "on_base_plus_slg" ~ "OPS",
      "oz_swing_percent" ~ "Chase%",
      "isolated_power" ~ "ISO"
    )) %>% 

  ggplot(aes(x = stat_value, fill = factor(stat_name))) +
  geom_density() +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~stat_name, scales = "free") + # scales = "free" lets the x/y axes vary for each facet
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # center plot title
    plot.subtitle = element_text(hjust = 0.5, face = "italic"), # and subtitle
    text = element_text(family = "Trebuchet MS")
  ) +
  labs(
    x = "Stat Value",
    y = "density",
    title = "2023 Batting Statistic Curves",
    subtitle = "Density Plots for all Qualified Hitters",
    caption = "\nData: Baseball Savant | By: @Metruvian_Man"
  ) + 
  theme(legend.position="none") +

geom_vline(data = data_vline,
                 aes(xintercept = median, color = "Median"
                 ),
                 color = "black",
                 size = 1,
                 linetype = "longdash"
          ) 


#####
# PITCHING STATISTICAL CURVES (WITH DIFFERENT COLORED FACETS)
#####

# Read in primary data with contact and swing decisions info
pitching <- read.csv(url("https://baseballsavant.mlb.com/leaderboard/custom?year=2023&type=pitcher&filter=&min=q&selections=pa%2Ck_percent%2Cbb_percent%2Cwoba%2Cxwoba%2Csweet_spot_percent%2Cbarrel_batted_rate%2Chard_hit_percent%2Cavg_best_speed%2Cavg_hyper_speed%2Coz_swing_percent%2Cwhiff_percent%2Cgroundballs_percent%2Cff_avg_speed%2Cff_avg_spin%2Cff_avg_break_x%2Cff_avg_break_z%2Cff_avg_break%2Cff_range_speed%2Cn_sl_formatted%2Csl_avg_speed%2Csl_avg_spin%2Csl_avg_break_x%2Csl_avg_break_z%2Csl_avg_break%2Cn_cukc_formatted%2Ccu_avg_speed%2Ccu_avg_spin%2Ccu_avg_break_x%2Ccu_avg_break_z%2Ccu_avg_break%2Ccu_range_speed%2Cfastball_avg_speed%2Cfastball_avg_spin%2Cfastball_avg_break_x%2Cfastball_avg_break_z%2Cfastball_avg_break%2Cfastball_range_speed&chart=false&x=pa&y=pa&r=no&chartType=beeswarm&sort=xwoba&sortDir=desc&csv=TRUE")) 

data_pitch <- pitching %>%
  select(last_name..first_name, player_id, k_percent, bb_percent, 
         whiff_percent, fastball_avg_speed, fastball_avg_spin, 
         groundballs_percent, barrel_batted_rate, oz_swing_percent,
         cu_avg_speed,cu_avg_spin, sl_avg_speed, sl_avg_spin) %>% 
  mutate(full_name = sub("(^.*),\\s(.*$)","\\2 \\1", last_name..first_name), 
         .before = 1) %>% select(-last_name..first_name) 

# create data frame for median geom_vlines
data_vline <- data.frame(stat_name = c("Avg. Curveball Spin", 
                                       "Avg. Curveball Velo",
                                       "Avg. Fastball Spin",
                                       "Avg. Fastball Velo",
                                       "Avg. Slider Spin",
                                       "Avg. Slider Velo",
                                       "Barrel%",
                                       "BB%",
                                       "Chase%",
                                       "Groundball%",
                                       "K%",
                                       "Whiff%"
                                        ),
                   median = c(median(data_pitch$cu_avg_spin, na.rm = TRUE),
                              median(data_pitch$cu_avg_speed, na.rm = TRUE),
                              median(data_pitch$fastball_avg_spin, na.rm = TRUE),
                              median(data_pitch$fastball_avg_speed, na.rm = TRUE),
                              median(data_pitch$sl_avg_spin, na.rm = TRUE),
                              median(data_pitch$sl_avg_speed, na.rm = TRUE),
                              median(data_pitch$barrel_batted_rate, na.rm = TRUE),
                              median(data_pitch$bb_percent, na.rm = TRUE),
                              median(data_pitch$oz_swing_percent, na.rm = TRUE),
                              median(data_pitch$groundballs_percent, na.rm = TRUE),
                              median(data_pitch$k_percent, na.rm = TRUE),
                              median(data_pitch$whiff_percent, na.rm = TRUE)
                              )
                             )

data_pitch %>%
  pivot_longer(
    k_percent:sl_avg_spin,
    names_to = "stat_name", # name column that will hold the names of the columns to stat_name
    values_to = "stat_value" # name column that will hold the values of the stat columns to stat_value
  ) %>%
  mutate(
    stat_name = case_match( # make stat names more presentable
      stat_name,
      "k_percent" ~ "K%",
      "bb_percent" ~ "BB%",
      "whiff_percent" ~ "Whiff%",
      "fastball_avg_speed" ~ "Avg. Fastball Velo",
      "fastball_avg_spin" ~ "Avg. Fastball Spin",
      "cu_avg_speed" ~ "Avg. Curveball Velo",
      "cu_avg_spin" ~ "Avg. Curveball Spin",
      "sl_avg_speed" ~ "Avg. Slider Velo",
      "sl_avg_spin" ~ "Avg. Slider Spin",
      "groundballs_percent" ~ "Groundball%",
      "barrel_batted_rate" ~ "Barrel%",
      "oz_swing_percent" ~ "Chase%",
    )) %>% 
  
  ggplot(aes(x = stat_value, fill = factor(stat_name))) +
  geom_density() +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~stat_name, scales = "free") + # scales = "free" lets the x/y axes vary for each facet
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # center plot title
    plot.subtitle = element_text(hjust = 0.5, face = "italic"), # and subtitle
    text = element_text(family = "Trebuchet MS")
  ) +
  labs(
    x = "Stat Value",
    y = "density",
    title = "2023 Pitching Statistic Curves",
    subtitle = "Density Plots for all Qualified Pitchers",
    caption = "\nData: Baseball Savant | By: @Metruvian_Man"
  ) + 
  theme(legend.position="none") +
  # we can trim the space below and beside axes but I prefer not to
  # scale_y_continuous(expand = c(0, 0)) # +
  # scale_x_continuous(expand = c(0, 0))
  
  geom_vline(data = data_vline,
             aes(xintercept = median, color = "Median"
             ),
             color = "black",
             size = 1,
             linetype = "longdash"
  )
