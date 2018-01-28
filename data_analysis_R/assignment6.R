# Adam Hendel
# DS710 Assignment 6
setwd('/Users/ahendel1/documents/academics/ds710fall2017assignment6')

# 1a) Read in the data
d <- read.csv('allRev.csv', header=FALSE)



# 1b
# add month column
library(ggplot2)
d$month <- seq(1, 24, by=1)
d$category <- c(replicate(12, 'old'), replicate(12, 'new'))
names(d) <- c('Revenue', 'Month', 'Campaign')

ggplot(d, aes(x=Month, y=Revenue, col=Campaign)) +
  geom_point(size=2.5) +
  geom_vline(xintercept=13, col='black', size=1) +
  ggtitle('TableFarm Revenue') +
  theme(plot.title = element_text(hjust = 0.5))


# 2c
ggplot(d, aes(x=Campaign, y=Revenue)) +
  geom_boxplot()

lm(d$rev)

t.test(d$Revenue ~ d$Campaign, alternative='greater')


# 2) Chocolate consumption vs nobel prize winners

# read data from assignment 4
nob <- read.csv('nobel_choco.csv', header=FALSE)
# c  represents chocolate consumption in kg/year/person and  
# n  represents the number of Nobel laureates per 10 million population.
names(nob) <- c('c', 'n')

# fit a linear model
mod <- lm(nob$n~nob$c)

# scatter plot, prize winners vs choco consumption
ggplot(nob, aes(x=c, y=n)) +
  geom_point() +
  scale_x_continuous(name='Chocolate Consumed \n(kg/year/person)') +
  scale_y_continuous(name='Number of Laureates \nper 10 mil population') +
  geom_abline(slope=mod$coefficients[2], intercept = mod$coefficients[1], col = 'red')


par(mfrow=c(2,2))
plot(mod)


# part3

encA <- read.csv("encryptedA_freq.csv", header=F)
names(encA) <- c('key', 'freq')
attach(encA)
library(dplyr)
# generate a vec with the index of the freq in ascending order
encA_order = order(freq) 
# plot the data, but select it according to the order we just generated
#barplot(freq[encA_order], names.arg = key[encA_order])



letterFreq <- read.csv('Letter Frequencies.csv', header=T)
ltrF <- letterFreq[,c('Letter', 'English')]
names(ltrF) <-c('key', 'freq') 
ltrF$type <- 'english'
encA$type <- 'encrypted'
#encA <- encA[order(encA$freq),'freq']
#ltrF <- ltrF[order(ltrF$freq),'freq']

#par(mfrow=c(2,1))
#barplot(encA$freq, names.arg=encA$key, main='Encrypted')
#barplot(ltrF$freq, names.arg=ltrF$key, main='English')




encB <- read.csv("encryptedB_freq.csv", header=F)
names(encB) <- c('key', 'freq')
encB_order = order(freq) 

letterFreq <- read.csv('Letter Frequencies.csv', header=T)
ltrF <- letterFreq[,c('Letter', 'English')]
names(ltrF) <-c('key', 'freq') 
ltrF$type <- 'english'
encA$type <- 'encrypted'

letterFreq <- read.csv('Letter Frequencies.csv', header=T)
ltrF <- letterFreq[,c('Letter', 'English')]
names(ltrF) <-c('key', 'freq') 
ltrF$type <- 'english'
encB$type <- 'encrypted'
encVec <- encB[order(encB$freq),'freq']
engVec <- ltrF[order(ltrF$freq),'freq']

length(engVec) == length(encVec)

expectedCounts<-sum(encVec)*engVec

actualcounts <- c(sum(encVec[1:7]), encVec[8:26])
expectedCounts <- c(sum(expectedCounts[1:7]), expectedCounts[8:26])
expectedDist <- c(sum(engVec[1:7]), engVec[8:26])

chisq.test(actualcounts, p=expectedDist)
