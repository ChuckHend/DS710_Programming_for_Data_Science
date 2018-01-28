# Adam Hendel
# DS710 Assignment 3

#### PART 1: ANALYZING USED CAR PRICES ####

# 1a. read on the data and attach
cars <- read.csv('Cars 2005.csv')
attach(cars)

# 1b. make a histogram and describe the shape
hist(Price)

# 1c. what proportion of the cars cost between $10k and $20k?
# total cars in set
tot.cars <- length(cars$Price)

# index of prices in the range
boolVec <- Price > 10000 & Price < 20000

# total cars in this range
tot.in.range <- sum(boolVec)

# proportion is ratio of the subset to the whole
proportion <- tot.in.range / tot.cars
print(proportion)
# 0.5621891


# About 56.2 % are within 10k and 20k

# 1d. Find the mean and median price. Which is larger, why does this make sense?

# mean price
mean(Price)
# 21343.14

# median price
median(Price)
# 18025

# the mean is higher. This make sense by visual inspection of the histogram because
# we see that the frequency of prices is skewed right, so the median will tend towards the higher
# density, which is a lower value (due to skewness).

# 1e. Add a vertical line to the histogram at the mean price. Also add a legend
hist(Price)
abline(v=mean(Price), col = 'red', lwd = 5)
# text(x=mean(Price)+7000,y=150, labels='Mean Price ')
legend(x=60000, y=150, legend = 'Mean Price', col = 'red', pch = 15)


# 1f. Transform price to reduce its skew, make a histogram of the transformed price. 
# fit a normal distribution to new price, graph the density curve on the same plot as histogram
# how well does a normal distribution fit the transformed data?

# transform price
log.Price <- log(Price)
# plot histogram of transformed price
hist(log.Price)
# fit normal distribution to transformed price


# 1g Make a scatterplot of transformed price versus engine size, in liters. Describe relationship
# between these two variables
plot(Price, Liter, pch = 16, cex = 0.7)
# As price increases so does the engine size, to a point. Around $50k the engine size levels off.

# 1h Find correlation between transformed price and engine size in liters. Explain.
cor(log.Price, Liter)
# a correlation of 0.59, indicates there is a slight positive relationship between these two variables
# As one variable increases, so does the other variable, but not perfectly.

# 1i. Modify the scatterplot in g to use one color of plotting symbol for cars with leather
# and a differet color for cars without leather interiors, and add a legend.
labs <- levels(factor(Leather))
# inspect labs 0 no leather, 1 is leather
labs <- c('No Leather', 'Leather')
plot(Price, Liter, pch = 16, cex = 1, col = c('black', 'red'))
legend(x = 60000, y = 3, legend = labs, col = c('red', 'black'), 
       pch = 16, title = 'Interior')
# cars[Price == max(Price),] check that the plot makes sense

# 1j. Make a barplot of the types
# ggplot has far more functionality, switching to this package over R base graphics
library(ggplot2)

base <- ggplot(cars, aes(x=Type))
base + geom_bar(stat='count')

# 1k. Make a barplot of the types of cars and whether they have interior leather. Add a legend
base +
  geom_bar(stat='count', aes(fill = factor(Leather))) +
  scale_fill_brewer(palette = 'Spectral', name = 'Interior', 
                    labels = c('0'='No Leather', '1' = 'Leather'))

# 1l. Make a boxplot of (untransformed) price by type of car. In words, summarize what it shows.
boxBase <- ggplot(cars)
boxBase +
  geom_boxplot(aes(Type, Price))

# The box plots show the distribution of price across the types of cars. We see that hatchbacks
# The horizontal line inside the box represents the median while the top and bottom boundaries of the 
# box represents the third and 1st quartile, respectively (IQR). The whiskers here are values outside the IQR,
# and could potentially be outliers. We see that hatchbacks price are consistently below 20k, while convertables
# are generally above $30k, but there are several observations of convertibles well over $60k.

# 1m. b.	Create two different histograms in a vertical stack that allow comparison of (untransformed)
# price according to whether the car has a leather interior. Use the same horizontal axis for each to 
# enable comparison, and use informative labels for each graph and the x-axis.

# going to modify the Leather column for ease of use
cars.hw <- cars
cars.hw$Leather[cars.hw$Leather == 0] <- 'Not Leather'
cars.hw$Leather[cars.hw$Leather == 1] <- 'Leather'

ggplot(cars.hw) +
  geom_histogram(aes(x=Price), binwidth = 1000) +
  facet_grid(Leather ~ .) + 
  scale_x_continuous(name = 'Price of Vehicle (USD)') +
  scale_y_continuous(name = 'Number of Vehicles')

# 1n. Create a single histogram with side-by-side bars to allow the same comparison in part m.
  # add a legend
  
ggplot(cars.hw) +
  geom_histogram(aes(x=Price, fill = Leather), 
                 binwidth = 2000, position = 'dodge') +
  scale_x_continuous(name = 'Price of Vehicle (USD)') +
  scale_y_continuous(name = 'Number of Vehicles')
  
#### PART 2: ANALYSING RUNNING SPEED OF MAMMALS ####
# 2a load data
install.packages("quantreg")
data(Mammals, package="quantreg")

# 2b Decide whether either of the quantitative variables should be transformed.
# justift the decision using plots and descriptive statistics

# inspect data
str(Mammals)

# histogram for weight
qplot(Mammals$weight, binwidth = 50)
boxplot(Mammals$weight)
summary(Mammals$weight)

# histogram for speed
qplot(Mammals$speed, binwidth = 2)
boxplot(Mammals$speed)
summary(Mammals$weight)

# weight is heavily skewed right. 
qplot(log(Mammals$weight), binwidth = 1)

# the weights of these animals are not normally distributed--there is a heavy right skew.
# we can see this by visually inspecting the histogram, but also taking note of several larege
# observations with weights >1000, which are many many signma away from the mean

# 2c Use appropriate graphs and/or descriptive statistics to describe the relationship between 
# maximum land speed and body weight. Does it matter whether the animal is a “hopper” (such as a kangaroo)?
# Explain why you chose the graphs and/or statistics that you chose.

