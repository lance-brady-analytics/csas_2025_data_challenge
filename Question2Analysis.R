# Question2Analysis.R
# Summary: This script seeks to explain Question 2: 
# On a day-to-day basis, does having a consistent or varied approach correlate with daily offensive output?

# First, let's read in our pitches dataframe and load our necessary libraries
library(tidyverse)
library(lme4)
library(car)

# Read in data frame
pitches = read.csv("statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv")

# Add a unique Plate Appearance ID to every row and add a batter_team column
pitches <- pitches %>%
  mutate(
    PA_id = paste(game_pk, at_bat_number, sep="_"),
    batter_team = ifelse(inning_topbot == "Bot", home_team, away_team)
  )

# Count the number of Plate Appearances for each batter in each game
player_game_allPA <- pitches %>%
  group_by(game_pk, batter, batter_team) %>%
  summarise(
    total_pa_game = n_distinct(PA_id),    # all PAs in that game for that batter
    .groups = "drop"
  )

# Find each player's mean and median bat speed and swing length for each game
# Subset to actual swings
batter_game_swings <- pitches %>%
  filter(!is.na(bat_speed), !is.na(swing_length)) %>%
  group_by(game_pk, batter, batter_team) %>%
  summarise(
    # mean & median for the swings that occurred
    game_mean_bat_speed      = mean(bat_speed, na.rm=TRUE),
    game_median_bat_speed    = median(bat_speed, na.rm=TRUE),
    game_mean_swing_length   = mean(swing_length, na.rm=TRUE),
    game_median_swing_length = median(swing_length, na.rm=TRUE),
    .groups="drop"
  )

# Add the total PA for each game to the batter_game_swings dataframe
batter_game_stats <- player_game_allPA %>%
  left_join(batter_game_swings,
            by = c("game_pk", "batter", "batter_team"))

# Weighted SD function
weighted_sd <- function(x, w) {
  w_mean <- sum(x*w, na.rm=TRUE)/sum(w, na.rm=TRUE)
  w_var  <- sum(w*(x - w_mean)^2, na.rm=TRUE)/sum(w, na.rm=TRUE)
  sqrt(w_var)
}

# Weighted Mean function
weighted_mean <- function(x, w) {
  sum(x*w, na.rm=TRUE)/sum(w, na.rm=TRUE)
}

# Create a dataframe with the game-level diversity statistics
team_game_diversity <- batter_game_stats %>%
  group_by(game_pk, batter_team) %>%
  summarise(
    # Weighted Mean of each player's mean (bat_speed)
    game_mean_mean_bat_speed = weighted_mean(game_mean_bat_speed, total_pa_game),
    # Weighted Mean of each player's median (bat_speed)
    game_mean_median_bat_speed = weighted_mean(game_median_bat_speed, total_pa_game),
    
    # Weighted Mean of each player's mean (swing_length)
    game_mean_mean_swing_length = weighted_mean(game_mean_swing_length, total_pa_game),
    # Weighted Mean of each player's median (swing_length)
    game_mean_median_swing_length = weighted_mean(game_median_swing_length, total_pa_game),
    
    # Weighted SD of each player's mean (bat_speed)
    game_sd_mean_bat_speed = weighted_sd(game_mean_bat_speed, total_pa_game),
    # Weighted SD of each player's median (bat_speed)
    game_sd_median_bat_speed = weighted_sd(game_median_bat_speed, total_pa_game),
    
    # Weighted SD of each player's mean (swing_length)
    game_sd_mean_swing_length = weighted_sd(game_mean_swing_length, total_pa_game),
    # Weighted SD of each player's median (swing_length)
    game_sd_median_swing_length = weighted_sd(game_median_swing_length, total_pa_game),
    
    .groups = "drop"
  )

# Now we need to find team-level wOBA and xwOBA for each game
# We'll define "final pitch" as the last pitch_number in that (game_pk, at_bat_number)
final_pitches <- pitches %>%
  group_by(game_pk, at_bat_number) %>%
  slice_max(order_by = pitch_number, n = 1) %>%
  ungroup()

# Clean up the final_pitches dataframe
final_pitches_clean <- final_pitches %>%
  filter(!is.na(woba_value), !is.na(woba_denom))

# Create a new dataframe with wOBA values
game_woba <- final_pitches_clean %>%
  group_by(game_pk, batter_team) %>%
  summarise(
    total_woba_val   = sum(woba_value, na.rm=TRUE),
    total_woba_denom = sum(woba_denom, na.rm=TRUE),
    game_wOBA = ifelse(total_woba_denom == 0, NA,
                       total_woba_val / total_woba_denom),
    .groups="drop"
  ) %>%
  select(game_pk, batter_team, game_wOBA)

# Create a new dataframe with xwOBA values
game_xwoba <- final_pitches_xwoba %>%
  group_by(game_pk, batter_team) %>%
  summarise(
    total_xwoba_value   = sum(xWOBA_value, na.rm=TRUE),
    total_xwoba_denom = sum(xWOBA_denom, na.rm=TRUE),
    game_xwOBA = ifelse(total_xwoba_denom == 0, NA,
                        total_xwoba_value / total_xwoba_denom),
    .groups="drop"
  ) %>%
  select(game_pk, batter_team, game_xwOBA)

# Now, combine into an analysis dataframe:
analysis_game_df <- team_game_diversity %>%
  left_join(game_woba, by=c("game_pk", "batter_team"))

analysis_game_df <- analysis_game_df %>%
  left_join(game_xwoba, by=c("game_pk","batter_team"))

# Remove rows with NA values
analysis_game_df <- analysis_game_df %>%
  filter(game_sd_mean_bat_speed != 0, game_sd_mean_swing_length != 0)

# Graphs
ggplot(analysis_game_df, aes(x=game_sd_median_swing_length, y=game_xwOBA)) +
  geom_point(aes(color=batter_team)) +
  scale_color_manual(
    values = c(
      "ATL" = "magenta",
      "PHI" = "red",
      "MIA" = "green",
      "WSH" = "blue",
      "NYM" = "orange"
    ),
    breaks = c("ATL", "PHI", "MIA", "WSH", "NYM"),
    guide = guide_legend(title = "Team")
  ) +
  labs(
    title="Team Swing Length Diversity vs. Team xwOBA",
    x="Game SD of Median Swing Length",
    y="Game xwOBA"
  )+
  theme_pub()+
  geom_smooth(method="lm", se=TRUE)

ggplot(analysis_game_df, aes(x=game_sd_median_bat_speed, y=game_xwOBA)) +
  geom_point(aes(color=batter_team)) +
  scale_color_manual(
    values = c(
      "ATL" = "magenta",
      "PHI" = "red",
      "MIA" = "green",
      "WSH" = "blue",
      "NYM" = "orange"
    ),
    breaks = c("ATL", "PHI", "MIA", "WSH", "NYM"),
    guide = guide_legend(title = "Team")
  ) +
  labs(
    title = "Team Swing Speed Diversity vs. Team xwOBA",
    x = "Game SD of Median Bat Speed",
    y = "Game xwOBA"
  ) +
  theme_pub() +
  geom_smooth(method="lm", se=TRUE, color="black")

# Make simple regression models of these graphs
# Swing Length
model_game_swing_length <- lm(game_xwOBA ~ game_mean_median_swing_length + game_sd_median_swing_length, data=analysis_game_df)
summary(model_game_swing_length)

# Bat Speed
model_game_bat_speed <- lm(game_xwOBA ~ game_mean_median_bat_speed + game_sd_median_bat_speed, data=analysis_game_df)
summary(model_game_bat_speed)

# Check correlations between our 4 median variables before we make a multivariate model
cor(analysis_game_df[c("game_sd_median_bat_speed", "game_sd_median_swing_length", "game_mean_median_swing_length", "game_mean_median_bat_speed")])

# Correlations are not extremely high, so we can proceed with a multivariate model.

# SIMPLE MULTIVARIATE MODEL
model_game_simple <- lm(game_xwOBA ~ game_sd_median_bat_speed + game_sd_median_swing_length + game_mean_median_bat_speed + game_mean_median_swing_length,
                        data=analysis_game_df)
summary(model_game_simple)

vif(model_game_simple)

# Our VIF are all below 2, so there is no reason to suspect multicollinearity
# or combine predictors

# MIXED MODEL
model_game_lme <- lmer(
  game_xwOBA ~ game_sd_median_bat_speed + game_sd_median_swing_length + game_mean_median_bat_speed + game_mean_median_swing_length + (1|batter_team),
  data = analysis_game_df
)
summary(model_game_lme)















