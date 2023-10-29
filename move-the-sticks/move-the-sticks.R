library(nflfastR)
library(tidyverse)
library(ggplot2)

df <- load_pbp(2022)

df_series <- calculate_series_conversion_rates(df, weekly=TRUE)

week_cutoff = 8
df_series <- df_series %>%
  mutate(
    off_scr_1st_cumulative = off_scr_1st,
    off_scr_2nd_cumulative = off_scr_1st_cumulative + off_scr_2nd,
    off_scr_3rd_cumulative = off_scr_2nd_cumulative + off_scr_3rd,
    off_scr_4th_cumulative = off_scr_3rd_cumulative + off_scr_4th,
    is_pre = if_else(week <= week_cutoff, 1, 0)
      ) %>%
  group_by(is_pre, team) %>%
  summarize(
    off_scr_1st_mean = mean(off_scr_1st),
    off_scr_2nd_mean = mean(off_scr_2nd),
    off_scr_3rd_mean = mean(off_scr_3rd),
    off_scr_4th_mean = mean(off_scr_4th),
    off_scr_1st_cumulative_mean = mean(off_scr_1st_cumulative),
    off_scr_2nd_cumulative_mean = mean(off_scr_2nd_cumulative),
    off_scr_3rd_cumulative_mean = mean(off_scr_3rd_cumulative),
    off_scr_4th_cumulative_mean = mean(off_scr_4th_cumulative)
  ) %>%
  pivot_wider(
    names_from=is_pre,
    values_from=c(
      off_scr_1st_mean,
      off_scr_2nd_mean,
      off_scr_3rd_mean,
      off_scr_4th_mean,
      off_scr_1st_cumulative_mean,
      off_scr_2nd_cumulative_mean,
      off_scr_3rd_cumulative_mean,
      off_scr_4th_cumulative_mean
      )
    )

# cumulative 1st --> cumulative 1st
# adj R2: 0.3842
y = df_series$off_scr_1st_cumulative_mean_1
x = df_series$off_scr_1st_cumulative_mean_0

fit <- lm(y ~ x, df_series)
linreg <- summary(fit)
linreg

ggplot(data=df_series, aes(x, y)) +
  geom_point() + 
  geom_abline(slope=linreg$coefficients[2], intercept=linreg$coefficients[1], color='red')

# cumulative 1st --> cumulative 4th
# adj R2: 0.2955
y = df_series$off_scr_4th_cumulative_mean_1
x = df_series$off_scr_1st_cumulative_mean_0

fit <- lm(y ~ x, df_series)
linreg <- summary(fit)
linreg

ggplot(data=df_series, aes(x, y)) +
  geom_point() + 
  geom_abline(slope=linreg$coefficients[2], intercept=linreg$coefficients[1], color='red')

# cumulative 2nd --> cumulative 4th
# adj R2: 0.2558
y = df_series$off_scr_4th_cumulative_mean_1
x = df_series$off_scr_2nd_cumulative_mean_0

fit <- lm(y ~ x, df_series)
linreg <- summary(fit)
linreg

ggplot(data=df_series, aes(x, y)) +
  geom_point() + 
  geom_abline(slope=linreg$coefficients[2], intercept=linreg$coefficients[1], color='red')


# cumulative 3rd --> cumulative 4th
# adj R2: 0.3154
y = df_series$off_scr_4th_cumulative_mean_1
x = df_series$off_scr_3rd_cumulative_mean_0

fit <- lm(y ~ x, df_series)
linreg <- summary(fit)
linreg

# cumulative 4th --> cumulative 4th
# adj R2: 0.3294
y = df_series$off_scr_4th_cumulative_mean_1
x = df_series$off_scr_4th_cumulative_mean_0

fit <- lm(y ~ x, df_series)
linreg <- summary(fit)
linreg

# cumulative 3rd --> cumulative 3rd
# adj R2: 0.2238
y = df_series$off_scr_3rd_cumulative_mean_1
x = df_series$off_scr_3rd_cumulative_mean_0

fit <- lm(y ~ x, df_series)
linreg <- summary(fit)
linreg

ggplot(data=df_series, aes(x, y)) +
  geom_point() + 
  geom_abline(slope=linreg$coefficients[2], intercept=linreg$coefficients[1], color='red')

# 3rd --> 3rd
# adj R2: 0.06213
y = df_series$off_scr_3rd_mean_1
x = df_series$off_scr_3rd_mean_0

fit <- lm(y ~ x, df_series)
linreg <- summary(fit)
linreg

ggplot(data=df_series, aes(x, y)) +
  geom_point() + 
  geom_abline(slope=linreg$coefficients[2], intercept=linreg$coefficients[1], color='red')

# cumulative 2nd --> 3rd
# adj R2: -0.03237
y = df_series$off_scr_3rd_mean_1
x = df_series$off_scr_2nd_cumulative_mean_0

fit <- lm(y ~ x, df_series)
linreg <- summary(fit)
linreg

ggplot(data=df_series, aes(x, y)) +
  geom_point() + 
  geom_abline(slope=linreg$coefficients[2], intercept=linreg$coefficients[1], color='red')

# cumulative 4th --> cumulative 4th
# adj R2: 0.2188
fit <- lm(off_scr_4th_cumulative_mean_1 ~ off_scr_4th_cumulative_mean_0, df_series)
linreg <- summary(fit)
linreg

ggplot(data=df_series, aes(off_scr_4th_cumulative_mean_0, off_scr_4th_cumulative_mean_1)) +
  geom_point() + 
  geom_abline(slope=linreg$coefficients[2], intercept=linreg$coefficients[1], color='red')

#  1st + 2nd + 3rd + 4th --> 3rd
# adj R2: 0.1236, no slope significant
y = df_series$off_scr_3rd_mean_1
x1 = df_series$off_scr_1st_mean_0
x2 = df_series$off_scr_2nd_mean_0
x3 = df_series$off_scr_3rd_mean_0
x4 = df_series$off_scr_4th_mean_0

fit <- lm(y ~ x1 + x2 + x3 + x4, df_series)
linreg <- summary(fit)
linreg

#  1st + 2nd + 3rd + 4th --> cumulative 4th
# adj R2: 0.3778
y = df_series$off_scr_4th_cumulative_mean_1
first_down = df_series$off_scr_1st_mean_0
second_down = df_series$off_scr_2nd_mean_0
third_down = df_series$off_scr_3rd_mean_0
fourth_down = df_series$off_scr_4th_mean_0

fit <- lm(y ~ first_down + second_down + third_down + fourth_down, df_series)
linreg <- summary(fit)
linreg


#  cumul 1st + cumul 2nd + cumul 3rd + cumul 4th --> cumulative 4th
# adj R2: 0.3778
y = df_series$off_scr_4th_cumulative_mean_1
after_first_down = df_series$off_scr_1st_cumulative_mean_0
after_second_down = df_series$off_scr_2nd_cumulative_mean_0
after_third_down = df_series$off_scr_3rd_cumulative_mean_0
after_fourth_down = df_series$off_scr_4th_cumulative_mean_0

fit <- lm(y ~ after_first_down + after_second_down + after_third_down + after_fourth_down, df_series)
linreg <- summary(fit)
linreg


#####################
ggplot(data=df_series, aes(x1, y)) +
  geom_point() + 
  geom_abline(slope=linreg$coefficients[2], intercept=linreg$coefficients[1], color='red')
linreg$coefficients[2]
