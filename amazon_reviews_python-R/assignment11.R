# Adam Hendel
# DS710 Assignment 11

d<-scan('/Users/ahendel1/Documents/Academics/ds710fall2017assignment11/finefoods_cleaned.csv',
     sep = ',', what='list', skip = 1)


# convert to matrix
# known rows based on analysis in python
d <- matrix(d, nrow=568454, ncol = 7, byrow = TRUE)

# assign column names
cols <- scan('/Users/ahendel1/Documents/Academics/ds710fall2017assignment11/finefoods_cleaned.csv',
             sep = ',', what='list', n=7)
colnames(d) <- cols

# check data types of certain columns
for(x in c('NumHelp', 'review_num_chars', 'exclams', 'frac_helpful')){
  print(paste(x, typeof(d[,x]), sep = ': '))
}

votes <- as.numeric(d[, 'NumHelp'])
review.length <- as.numeric(d[, 'review_num_chars'])
num.exclams <- as.numeric(d[, 'exclams'])
frac.helpful <- as.numeric(d[, 'frac_helpful'])

# examine fraction of helpful votes
summary(frac.helpful)

which(frac.helpful>1)
frac.helpful[which(frac.helpful>1)]

d[which(frac.helpful>1), c('NumHelp', 'NumReviews', 'frac_helpful')] <- NA

# reassign vars
votes <- as.numeric(d[, 'NumHelp'])
review.length <- as.numeric(d[, 'review_num_chars'])
num.exclams <- as.numeric(d[, 'exclams'])
frac.helpful <- as.numeric(d[, 'frac_helpful'])

helpful.reviews.len <- review.length[which(frac.helpful > 0.5)]
other.reviews.len <- review.length[which(frac.helpful <= 0.5)]
# compare helpful reviews vs. non helpful ones
par(mfrow=c(1,2))
hist(helpful.reviews.len)
hist(other.reviews.len)

# log transform right skew
log.helpful.len <- log(helpful.reviews.len)
log.other.len <- log(other.reviews.len)
hist(log.helpful.len)
hist(log.other.len)

library(ggplot2)
#Plot.
ggplot() + 
  geom_density(aes(x=log.helpful.len), fill = 'blue', alpha = 0.35) +
  geom_density(aes(x=log.other.len), fill = 'red', alpha =  0.35) +
  xlab('log.length')

t.test(log.helpful.len, log.other.len, mu=0, alternative = 'greater')


# max votes recieved by any of the reviews for a product
max.votes <- tapply(votes, d[,'ProductID'], FUN = max)
max.votes
length(unique(d[,'ProductID'])) == length(max.votes)

# count number of reviews for each product
num.reviews <- tapply(as.numeric(d[,'NumReviews']), d[,'ProductID'], FUN = sum, na.rm = T)
num.reviews
sum(as.numeric(d[,'NumReviews']), na.rm = T) == sum(as.vector(num.reviews), na.rm = T)

# the the data
plot(num.reviews, max.votes)
mod<-lm(max.votes~num.reviews)
abline(mod, col='red', lwd=2)
summary(mod)

# subset variables to exclude zeros
sub.max.votes <- max.votes[which(max.votes>0 & num.reviews>0)]
sub.num.reviews <- num.reviews[which(max.votes>0 & num.reviews>0)]

# the the subsetdata
plot(log(sub.num.reviews), log(sub.max.votes))
