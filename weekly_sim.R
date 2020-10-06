# game win probabilities, as given by 538 following week n
# games that have already been played are given a 0 (loss) or 1 (win)
DAL_cutoffs_3 <- c(0, 1, 0, .62, .79, .63, .7, .57, .45, .6, .78, .33, .69, .47, .67, .7)
DAL_cutoffs_4 <- c(0, 1, 0, 0, .78, .62, .67, .49, .45, .53, .76, .29, .63, .46, .6, .69)

# reset vectors
DAL_games_won_3 <- numeric(0)
DAL_games_won_4 <- numeric(0)


# simulate 100,000 outcomes with week 3 probabilities
for (i in 1:100000){
  probs <- runif(16)
  count = 0
  for (j in 1:16){
    prob = probs[j]
    cutoff = DAL_cutoffs_3[j]
    if (prob < cutoff){
      count = count + 1
    } else{
    }
  }
  DAL_games_won_3 <- c(DAL_games_won_3, count)
}

# simulate 100,000 outcomes with week 4 probabilities
for (i in 1:100000){
  probs <- runif(16)
  count = 0
  for (j in 1:16){
    prob = probs[j]
    cutoff = DAL_cutoffs_4[j]
    if (prob < cutoff){
      count = count + 1
    } else{
    }
  }
  DAL_games_won_4 <- c(DAL_games_won_4, count)
}

# plot overlaid histograms of frequency distributions
library(ggplot2)
df_3 <- data.frame(xx = c(DAL_games_won_3), yy = rep(c('3'), each = 100000))
df_4 <- data.frame(xx = c(DAL_games_won_4), yy = rep(c('4'), each = 100000))
df <- data.frame(xx = c(DAL_games_won_3, DAL_games_won_4), group = rep(c('3', '4'), each = 100000))
ggplot(df, aes(x=xx)) +
  geom_histogram(data=subset(df, group = '3'), aes(fill=group), alpha=0.4, binwidth=1, position='identity') +
  geom_histogram(data=subset(df, group = '4'), aes(fill=group), alpha=0.5, binwidth=1, position='identity') +
  scale_fill_manual(name='', values=c('#869397', '#041E42'), labels = c('Before week 3', 'After week 3')) +
  labs(title ='Distribution of 2020 season wins for DAL, before and after week 3', 
       subtitle = 'Simulated 100,000 times using fivethirtyeight.com probabilities',
       caption = 'Analysis by @sahil_bolar') + 
  xlab('2020 season wins') +
  geom_vline(xintercept = mean(DAL_games_won_3), color = '#869397') +
  geom_vline(xintercept = mean(DAL_games_won_4), color = '#041E42')


