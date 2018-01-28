setwd('D:/Downloads')

d <- read.csv('babyboom.csv', header=T)
attach(d)
detach(d)
head(d)

unique(d$Sex)

sextoFM <- function(x){
  y <- rep.int(NA, length(x))
  male <- x=='boy' | x=='Male' | x=='M'
  female <- x=='girl' | x=='female' | x=='F'
  y[male] <- 'M'
  y[female] <- 'F'
  return(y)
}

d$Sex <- sextoFM(d$Sex)



female = c("F", "female", "girl")
d$Sex %in% female

sextoFM2 <- function(x){
  y <- rep.int(NA, length(x))
  male <- c("M", "Male", "boy")
  female <- c("F", "female", "girl")
  y[d$Sex %in% male] <- 'M'
  y[d$Sex %in% female] <- 'F'
  return(y)
}

d$Sex == sextoFM2(d$Sex)


d$Time



Time2 = as.numeric(d$Time)
Time2
Time2 = as.numeric(levels(d$Time))[d$Time]
Time2 = as.numeric(as.character(Time)) 

cbind(as.character(Time), Time2)
d$Time = Time2

summary(d$Time)
summary(Time2)

d[which(d$Time %% 100 > 59),]

d$Time[14] <- NA

d$Time[d$Time > 2400] <- NA
which( is.na( d$Time ) )

summary(d$Time)

sd(d$Time, na.rm = T)




which(d$Weight > 5000 | d$Weight < 1000)
d$Weight[which(d$Weight > 5000 | d$Weight < 1000)] <- NA

mydata2 = data.frame(d$Time,d$Sex,d$Weight)


complete.cases(mydata2)
mydata3<-mydata2[complete.cases(mydata2),]
nrow(mydata3)

mydata3[order(mydata3$d.Time),]
