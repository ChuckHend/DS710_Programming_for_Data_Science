# Adam Hendel
# DS710 Assignment 2: Control Flow

setwd('/Users/ahendel1/documents/academics/MSDS/ds710fall2017assignment2')
# 1 Airport Statistics

# a. read in the data, and inspect it
d <- read.csv('airport.csv')
str(d)
head(d)

# b. Use control flow to print a list of airports at which the number of scheduled 
#    departures was less than the number of departures performed.
for (i in 1:nrow(d)){
  # slect first airport
  d.1 <- d[i,]
  if(d.1$Scheduled.Departures < d.1$Performed.Departures){
    print(as.character(d.1$Airport))
  }
}
# d$Airport[d$Scheduled.Departures < d$Performed.Departures]

# c. Use control flow to find the average (mean) number of passengers on 
#    flights from the airports in part b.
totPass <- 0
numFli <- 0
for (i in 1:nrow(d)){
  # select first airport
  d.1 <- d[i,]
  if(d.1$Scheduled.Departures < d.1$Performed.Departures){
    totPass <- totPass + d.1$Passengers
    numFli <- numFli + 1
  }
}
avg <- totPass / numFli
print(avg)

# mean(d[d$Scheduled.Departures < d$Performed.Departures,]$Passengers)
