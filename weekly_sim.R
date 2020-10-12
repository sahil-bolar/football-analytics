library(ggplot2)
library(stringr)

# game win probabilities, as given by 538 following week n
# games that have already been played are given a 0 (loss) or 1 (win)
DAL_cutoffs_3 <- c(0, 1, 0, .62, .79, .63, .7, .57, .45, .6, .78, .33, .69, .47, .67, .7)
DAL_cutoffs_4 <- c(0, 1, 0, 0, .78, .62, .67, .49, .45, .53, .76, .29, .63, .46, .6, .69)
DAL_cutoffs_5 <- c(0, 1, 0, 0, 1, .39, .51, .32, .25, .34, .61, .15, .46, .37, .43, .51)

# define pre and post
week <- 5
DAL_pre_cutoffs = DAL_cutoffs_4
DAL_post_cutoffs = DAL_cutoffs_5

# reset vectors
DAL_pre_games_won <- numeric(0)
DAL_post_games_won <- numeric(0)

# simulate 100,000 outcomes with week 3 probabilities
for (i in 1:100000){
  probs <- runif(16)
  count = 0
  for (j in 1:16){
    prob = probs[j]
    cutoff = DAL_pre_cutoffs[j]
    if (prob < cutoff){
      count = count + 1
    } else{
    }
  }
  DAL_pre_games_won <- c(DAL_pre_games_won, count)
}

# simulate 100,000 outcomes with week 4 probabilities
for (i in 1:100000){
  probs <- runif(16)
  count = 0
  for (j in 1:16){
    prob = probs[j]
    cutoff = DAL_post_cutoffs[j]
    if (prob < cutoff){
      count = count + 1
    } else{
    }
  }
  DAL_post_games_won <- c(DAL_post_games_won, count)
}

# plot overlaid histograms of frequency distributions
library(ggplot2)
df_pre <- data.frame(xx = c(DAL_pre_games_won), yy = rep(c('pre'), each = 100000))
df_post <- data.frame(xx = c(DAL_post_games_won), yy = rep(c('post'), each = 100000))
df <- rbind(df_pre, df_post)
ggplot(df, aes(x=xx)) +
  geom_histogram(data=subset(df, group = 'pre'), aes(fill=group), alpha=0.4, binwidth=1, position='identity') +
  geom_histogram(data=subset(df, group = 'post'), aes(fill=group), alpha=0.5, binwidth=1, position='identity') +
  scale_fill_manual(name='', values=c('#869397', '#041E42'), labels = c(str_glue('Before week {week}'), str_glue('After week {week}'))) +
  labs(title = str_glue('Distribution of 2020 season wins for DAL, before and after week {week}'), 
       subtitle = 'Simulated 100,000 times using fivethirtyeight.com probabilities',
       caption = 'Analysis by @sahil_bolar') + 
  scale_x_continuous(name = 'Games projected to win', breaks = seq(0, 16, 1)) +
  scale_y_continuous(name = 'Relative Frequency', labels = seq(0, 0.25, 0.05)) +
  geom_vline(xintercept = mean(DAL_pre_games_won), color = '#869397') +
  geom_vline(xintercept = mean(DAL_post_games_won), color = '#041E42')


