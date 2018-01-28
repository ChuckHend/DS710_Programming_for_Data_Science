# Adam Hendel DS710
# Assignment 10

d <- read.csv('/Users/ahendel1/documents/Academics/ds710fall2017assignment9/usnews_clean.csv')
attach(d)
n.1 = length(which(Instructional.expenditure.per.student > Out.of.state.tuition))

n=0
for(row in 1:nrow(d)){
  d.1<-d[row,]
  if(is.na(d.1$Instructional.expenditure.per.student) || is.na(d.1$Out.of.state.tuition)){
    
  }else if(d.1$Instructional.expenditure.per.student > d.1$Out.of.state.tuition){
    n = n + 1
  }
}
n==n.1

system.time(length(which(Instructional.expenditure.per.student > Out.of.state.tuition)))

system.time(for(row in 1:nrow(d)){
  d.1<-d[row,]
  if(is.na(d.1$Instructional.expenditure.per.student) || is.na(d.1$Out.of.state.tuition)){
    
  }else if(d.1$Instructional.expenditure.per.student > d.1$Out.of.state.tuition){
    n = n + 1
  }
})


# drop college, state, pub_prv (string values)
d<-d[,-c(2,3, 36)]

# option 1
opt1 <- lapply(d, mean, na.rm=T)

# option 2
mymean <- function(x){
  sum <- sum(x, na.rm = T)
  len <- length(which(!is.na(x)))
  return(sum/len)
}
opt2 <- lapply(d, mymean)

# option 3
results <- c()
z=1
for(col in names(d)){
  x<- d[,col]
  sum=0
  n=0
  for(i in 1:length(x)){
    if(!is.na(x[i])){
      sum = sum + x[i]
      n = n+1
    }
  }
  results[z] <- sum/n
  z = z + 1
}
opt3 <- results

opt1 <- as.vector(unlist(opt1))
opt2 <- as.vector(unlist(opt2))
opt3 <- as.vector(opt3)

opt1==opt2
opt1==opt3
opt2==opt3

opt1[which(opt1 != opt2)]
opt2[which(opt1 != opt2)]
opt3[which(opt2 != opt3)]
     
opt1[30]
opt2[30]
opt1[30]== opt2[30]

library(microbenchmark)
microbenchmark(lapply(d, mean, na.rm=T))
microbenchmark(lapply(d, mymean))
microbenchmark(for(col in names(d)){
  x<- d[,col]
  sum=0
  n=0
  for(i in 1:length(x)){
    if(!is.na(x[i])){
      sum = sum + x[i]
      n = n+1
    }
  }
  results[z] <- sum/n
  z = z + 1
})
