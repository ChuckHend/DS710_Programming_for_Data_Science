## ADAM HENDEL
## DS700 Assignment 9 - DATA CLEANING


# read in the data we cleaned in python
d <- read.csv('D:/Projects/ds710fall2017assignment9/usnews_clean.csv')

str(d)
head(d)

summary(d)


# visually inspect state abbreviations
unique(d$State)

# visually inspect min/max of SAT (200-800 for each) and ACT (1-36) for expected ranges
summary(d)

# Pct.of.faculty.with.PhDs has 105 percent value somewhere in the variable
min(d$Pct.of.faculty.with.PhDs, na.rm = T)
max(d$Pct.of.faculty.with.PhDs, na.rm = T)

# find all records with erroneous values in this column
d[which(d$Pct.of.faculty.with.PhDs> 100 | d$Pct.of.faculty.with.PhDs<0),]

d$Pct.of.faculty.with.PhDs[which(d$Pct.of.faculty.with.PhDs> 100 | d$Pct.of.faculty.with.PhDs<0)] <- NA

# Graduation.rate has value > 100 percent
max(d$Graduation.rate, na.rm = T)

# set values > 100 to NA
d[which(d$Graduation.rate> 100 | d$Graduation.rate<0),]
d$Graduation.rate[which(d$Graduation.rate> 100 | d$Graduation.rate<0)] <- NA

names(d)

d[which(d$First.quartile...Math.SAT > d$Third.quartile...Math.SAT | d$First.quartile...Verbal.SAT == d$Third.quartile...Verbal.SAT),]


d[,c("First.quartile...Math.SAT", "Third.quartile...Math.SAT", "First.quartile...Verbal.SAT",  "Third.quartile...Verbal.SAT")]


# Westfield College
d[462, c("First.quartile...Verbal.SAT",  "Third.quartile...Verbal.SAT", 'iqrVERB')] <- NA

# Pembroke
d[674, c("First.quartile...Math.SAT", "Third.quartile...Math.SAT", 'iqrMATH')] <- NA

prvDonate <- d$Pct.alumni.who.donate[d$pub_prv=='Private']
pubDonate <- d$Pct.alumni.who.donate[d$pub_prv=='Public']
mean(prvDonate, na.rm = T)
mean(pubDonate, na.rm = T)

summary(prvDonate)
summary(pubDonate)

length(prvDonate[!is.na(prvDonate)])
length(pubDonate[!is.na(pubDonate)])

sd(prvDonate, na.rm = T)
sd(pubDonate, na.rm = T)

welch.t.test <- function(mean1,mean2,sd1,sd2,n1,n2){
  # standard error
  se<- sqrt( sd1^2/n1 + sd2^2/n2 )
  
  # test statistic
  t<- (mean1 - mean2)/se
  
  # degrees of freedom
  df <-  ( sd1^2/n1 + sd2^2/n2 )^2 / ( (sd1^2/n1)^2/(n1-1) + (sd2^2/n2)^2/(n2-1) ) 
  
  # p-value
  p <- 2 * pt(-abs(t), df)
  
  # output data
  dframe<-data.frame(test.statistic=t,
                     DOF=df,
                     P.Value=p)
  return(dframe) 
}

prvMean <- mean(prvDonate, na.rm = T)
pubMean <- mean(pubDonate, na.rm = T)
prvSD   <- sd(prvDonate, na.rm = T)
pubSD   <- sd(pubDonate, na.rm = T)
prvLen  <- length(prvDonate[!is.na(prvDonate)])
pubLen  <- length(pubDonate[!is.na(pubDonate)])