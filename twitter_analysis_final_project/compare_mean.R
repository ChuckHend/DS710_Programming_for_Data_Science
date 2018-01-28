# Adam Hendel
# DS710 Final Project
# Compare mean sentiment amongst Optum and UHC tweets

library(ggplot2)
library(reshape2)
# Read in the data ####
d <- read.csv('optum_uhc_sentiments.csv')

num_tweets <- nrow(d)

polarity <- melt(d[, c('optumTweets_pol', 'uhcTweets_pol')])

subjectivity <- melt(d[, c('optumTweets_sub', 'uhcTweets_sub')])

ggplot(polarity,aes(x=value, fill=variable)) + 
  geom_density(alpha=0.25) +
  ggtitle(label = paste('UnitedHealth Group Subsidiary Tweet Polarity', sep = ' '),
          subtitle = paste('Sample of', num_tweets, 'tweets', sep=' ')) +
  scale_fill_manual(name = 'Business',
                    labels= c('Optum', 'UHC'), 
                    values = c('#F2B411', '#0066F5')) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

ggplot(subjectivity,aes(x=value, fill=variable)) + 
  geom_density(alpha=0.25) +
  ggtitle(label = paste('UnitedHealth Group Subsidiary Tweet Subjectivity', sep = ' '),
          subtitle = paste('Sample of', num_tweets, 'tweets', sep=' ')) +
  scale_fill_manual(name = 'Business',
                    labels= c('Optum', 'UHC'), 
                    values = c('#F2B411', '#0066F5')) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

t.test(d$optumTweets_pol, d$uhcTweets_pol, alternative = 'two')
# Welch Two Sample t-test
# 
# data:  d$optumTweets_pol and d$uhcTweets_pol
# t = -3.1217, df = 1640.4, p-value = 0.001829
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.05966358 -0.01361910
# sample estimates:
#   mean of x  mean of y 
# 0.07074475 0.10738609

t.test(d$optumTweets_sub, d$uhcTweets_sub, alternative = 'two')
# Welch Two Sample t-test
# 
# data:  d$optumTweets_sub and d$uhcTweets_sub
# t = -8.7279, df = 1823, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.14435306 -0.09138079
# sample estimates:
#   mean of x mean of y 
# 0.1667359 0.2846028 

optum_pol_mean <- mean(d$optumTweets_pol)
optum_pol_se <- sd(d$optumTweets_pol)/sqrt(length(d$optumTweets_pol))
optum_sub_mean <- mean(d$optumTweets_sub)
optum_sub_se <- sd(d$optumTweets_sub)/sqrt(length(d$optumTweets_sub))

uhc_pol_mean   <- mean(d$uhcTweets_pol)
uhc_pol_se <- sd(d$uhcTweets_pol)/sqrt(length(d$uhcTweets_pol))
uhc_sub_mean   <- mean(d$uhcTweets_sub)
uhc_sub_se <- sd(d$uhcTweets_sub)/sqrt(length(d$uhcTweets_sub))

results <- data.frame(Polarity_Mean=c(optum_pol_mean, uhc_pol_mean),
                      Polarity_SE=c(optum_pol_se, uhc_pol_se),
                      Subjectivity_Mean=c(optum_sub_mean, uhc_sub_mean),
                      Subjectivity_SE=c(optum_sub_se, uhc_sub_se),
                      row.names = c('Optum', 'UHC'))

results <- round(results, 3)

# visualize results
# POLARITY
barCenters <- barplot(height = c(results$Polarity_Mean),
        names.arg = c('Optum', 'UHC'),
        ylab = 'Polarity',
        col = c('#F2B411', '#0066F5'),
        ylim = c(-1, 1),
        main = 'Polarity')
segments(barCenters, results$Polarity_Mean + results$Polarity_SE,
         barCenters, results$Polarity_Mean - results$Polarity_SE)
arrows(barCenters, results$Polarity_Mean + results$Polarity_SE,
       barCenters, results$Polarity_Mean - results$Polarity_SE,
       angle = 90, code = 3)
text(x=0.7, y=optum_pol_mean/2, round(optum_pol_mean,2))
text(x=1.9, y=uhc_pol_mean/2, round(uhc_pol_mean,2),col='white')

# SUBJECTIVITY
barCenters <- barplot(height = c(results$Subjectivity_Mean),
                      names.arg = c('Optum', 'UHC'),
                      ylab = 'Subjectivity',
                      col = c('#F2B411', '#0066F5'),
                      ylim = c(0, 1),
                      main = 'Subjectivity')
segments(barCenters, results$Subjectivity_Mean + results$Subjectivity_SE,
         barCenters, results$Subjectivity_Mean - results$Subjectivity_SE)
arrows(barCenters, results$Subjectivity_Mean + results$Subjectivity_SE,
       barCenters, results$Subjectivity_Mean - results$Subjectivity_SE,
       angle = 90, code = 3)
text(x=0.7, y=optum_sub_mean/2, round(optum_sub_mean,2))
text(x=1.9, y=uhc_sub_mean/2, round(uhc_sub_mean,2),col='white')

write.csv(results, 'sent_results.csv', row.names = T)


# Emoji Proportions ####
# due to extremely small number of emojis present in the sample, this 
# analysis is omitted from the results and executive summary
emoji <- read.csv('emoji_results.csv')
# need to reshape the dataframe so rows are observations
emoji <- t(emoji[,2:3])
# assign col names
colnames(emoji) <- c('num_emojis', 'num_tweets')
emoji

prop.test(x=emoji[,1], n=emoji[,2], alternative = 'two')
# 2-sample test for equality of proportions with continuity correction
# 
# data:  emoji[, 1] out of emoji[, 2]
# X-squared = 5.2465, df = 1, p-value = 0.02199
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.008010121 -0.000989879
# sample estimates:
#   prop 1 prop 2 
# 0.0000 0.0045 