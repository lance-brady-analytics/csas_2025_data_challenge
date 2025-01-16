# Question1Analysis.R
# Summary: This script seeks to explain Question 1: 
# Is consistency associated with higher personal performance once I factor out average skill?

# First, let's read in our pitches dataframe and load our necessary libraries
library(tidyverse)
library(car)

pitches = read.csv("statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv")

# Add a unique Plate Appearance ID to every row and add a batter_team column
pitches <- pitches %>%
  mutate(
    PA_id = paste(game_pk, at_bat_number, sep="_"),
    batter_team = ifelse(inning_topbot == "Bot", home_team, away_team)
  )

# Get season-long swing tendencies for each player
player_swing_consistency <- pitches %>%
  filter(!is.na(swing_length), !is.na(bat_speed)) %>%
  group_by(batter) %>%
  summarise(
    # Season-long means
    mean_swing_length = mean(swing_length, na.rm=TRUE),
    mean_bat_speed    = mean(bat_speed, na.rm=TRUE),
    # Season-long standard deviations
    sd_swing_length = sd(swing_length, na.rm=TRUE),
    sd_bat_speed    = sd(bat_speed, na.rm=TRUE),
    .groups = "drop"
  )

## Take out players with NA standard deviation
player_swing_consistency <- player_swing_consistency %>%
  filter(!is.na(sd_swing_length), !is.na(sd_bat_speed))

# Now, let's calculate wOBA and xwOBA for each player
# We'll define "final pitch" as the last pitch_number in that (game_pk, at_bat_number)
final_pitches <- pitches %>%
  group_by(game_pk, at_bat_number) %>%
  slice_max(order_by = pitch_number, n = 1) %>%
  ungroup()

final_pitches_xwoba <- final_pitches %>%
  filter(!is.na(estimated_woba_using_speedangle)) %>%
  mutate(
    xWOBA_value = estimated_woba_using_speedangle,
    xWOBA_denom = 1
  )

player_xwoba <- final_pitches_xwoba %>%
  group_by(batter) %>%
  summarise(
    total_xwoba_value = sum(xWOBA_value, na.rm=TRUE),
    total_xwoba_denom = sum(xWOBA_denom, na.rm=TRUE),
    player_xwOBA = ifelse(total_xwoba_denom == 0, NA,
                          total_xwoba_value / total_xwoba_denom),
    .groups="drop"
  ) %>%
  select(batter, player_xwOBA)


# Now, combine into an analysis dataframe:
player_analysis_df <- player_swing_consistency %>%
  left_join(player_xwoba, by="batter")

# Let's graph these scenarios, using the median variation values and xWOBA:
ggplot(player_analysis_df, aes(x=sd_swing_length, y=player_xwOBA)) +
  geom_point() +
  labs(
    title="Player Swing Length Consistency vs. Player xwOBA",
    x="Player SD of Swing Length",
    y="Player xwOBA"
  )+
  theme_pub()+
  geom_smooth(method="lm", se=TRUE)

ggplot(player_analysis_df, aes(x=sd_bat_speed, y=player_xwOBA)) +
  geom_point() +
  labs(
    title="Player Bat Speed Consistency vs. Player xwOBA",
    x="Player SD of Bat Speed",
    y="Player xwOBA"
  )+
  theme_pub()+
  geom_smooth(method="lm", se=TRUE)

# Create linear models to study the effect of each variable when accounting for others
model_sd_swing_length <- lm(player_xwOBA ~ mean_swing_length + sd_swing_length, data=player_analysis_df)
summary(model_sd_swing_length)

model_sd_bat_speed <- lm(player_xwOBA ~ mean_bat_speed + sd_bat_speed, data=player_analysis_df)
summary(model_sd_bat_speed)

# Let's also try out a multivariate model.
mod_multi <- lm(player_xwOBA ~ mean_swing_length + mean_bat_speed + sd_swing_length + sd_bat_speed,
                data = player_analysis_df)
summary(mod_multi)
vif(mod_multi)


# Show collinearity between predictors
cor(player_analysis_df$sd_swing_length, player_analysis_df$sd_bat_speed)
cor(player_analysis_df$mean_swing_length, player_analysis_df$mean_bat_speed)
cor(player_analysis_df$mean_swing_length, player_analysis_df$sd_swing_length)
cor(player_analysis_df$mean_bat_speed, player_analysis_df$sd_bat_speed)

# Although our VIF values are all less than 5, suggesting that multicollinearity 
# is likely not a problem and just a result of using real imperfect data, we can 
# try to create a "mechanical variability" combined component using Principal 
# Components Analysis to combine the two predictors into one. We have some 
# collinearity between predictors, especially between our variability variables.

# Standardize variables
player_analysis_df <- player_analysis_df %>%
  mutate(
    sd_swing_length_scaled = scale(sd_swing_length),
    sd_bat_speed_scaled = scale(sd_bat_speed)
  )

# Perform PCA
pca_model <- prcomp(player_analysis_df[, c("sd_swing_length_scaled", "sd_bat_speed_scaled")],
                    center = TRUE, scale. = TRUE)

# Add first principal component to the dataset
player_analysis_df <- player_analysis_df %>%
  mutate(mechanical_variability = pca_model$x[, 1])

# Check PCA results
summary(pca_model)

# Our PCA results say that we have explained over 90% of the variance with our first
# component. This is a good sign that we have a good combined variable.

# Now, let's try our multivariate model with our new combined variable.
model <- lm(player_xwOBA ~ mean_swing_length + mean_bat_speed + mechanical_variability,
data = player_analysis_df)
summary(model)

vif(model)

# Our VIFs are again under 5, signalling that this is a good model. Our model has
# a lower Adjusted R-Squared than our model with the two separate variables, suggesting
# that they are more predictive than our combined variable. However, our combined variable
# is still significant and has a negative coefficient, suggesting that players with more
# mechanical variability over a season have lower xwOBA values.

# This could be for a variety of reasons, but it suggests that players who are more
# consistent in their swing length and bat speed have higher xwOBA values. This could
# be due to the fact that they have gotten closer to an optimal swing for their
# body type or that a consistency in mechanics leads to better results.

# Let's visualize this relationship
ggplot(player_analysis_df, aes(x=mechanical_variability, y=player_xwOBA)) +
  geom_point() +
  labs(
    title="Player Mechanical Variability vs. Player xwOBA",
    x="Player Mechanical Variability",
    y="Player xwOBA"
  )+
  theme_pub()+
  geom_smooth(method="lm", se=TRUE)

# Our graph shows a negative relationship between mechanical variability and xwOBA,
# reaffirming that players with more mechanical variability have lower xwOBA values.








