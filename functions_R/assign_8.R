# ADAM HENDEL
# DS710 - Assignment 8

# explore dataset
d <- read.csv('Best Cities.csv')
str(d)

pleasantness <- function(dataVec){
  trav <- dataVec[2]/20
  if(dataVec[12]<10000){
    pop <- 3
  }else{
    pop <- 0
  }
  bike <- dataVec[16]/10
  tmax <- max((dataVec[17]-80)/10, 0)
  p_score <- trav + pop + bike - tmax
  return(p_score)
}

pleasantness(d$Madison.city..Wisconsin)

apply(d[,2:length(d)], 2, pleasantness)

d<- read.csv('AmesHousing.csv')
str(d)
tapply(d$SalePrice, d$Land.Slope, median)

PearsonSkew <- function(x, remove_na=T){
  return(3*(mean(x, na.rm=remove_na) - median(x, na.rm=remove_na)) / sd(x, na.rm=remove_na))
}
y = c( 1, 1, 2, 10 )

PearsonSkew(y)

apply(ames2, 2, PearsonSkew)

names(ames2)
score <- function(x){
  chars<-names(x)
  ft <- as.numeric(x[chars=='Gr.Liv.Area'])/1000
  if(x[chars=='SalePrice']<300000){
    sale<- 3
    if(as.numeric(x[chars=='SalePrice'])<200000){
      sale <- sale +2
    }
  }else{
    sale<- 0
  }
  if(x[chars=='Neighborhood']=='NoRidge'){
    loc<-3
  }else if(x[chars=='Neighborhood']=='NridgHt'){
    loc<-2
  }else{
    loc<-0
  }
  return(ft+sale+loc)
}

which.max(apply(d, 1, score))

x=d[1,]

mourner <- c(3, 7, 8, 3, 7, 3, 3, 6, 2, 3, 3, 2, 3, 4, 3, 8, 10, 
             2, 3, 3, 7, 4, 2, 10, 6, 3, 4, 9, 3, 6, 4, 2, 4, 2, 
             6, 4, 3, 8, 5, 2, 5, 4, 8, 11, 2, 6, 4, 4, 3, 3, 7, 
             2, 7, 3, 4, 2, 11, 2, 6, 5, 4, 8, 2, 3, 7, 2, 4, 6, 
             4, 3, 5, 6, 2, 3, 5, 10, 5, 6, 5, 4, 8, 8, 8, 2, 3, 
             8, 7, 2, 3, 6, 3, 6, 2, 3, 9, 3, 6, 4, 3, 3, 7, 3, 
             5, 2, 9, 3, 8, 8, 2, 6, 4, 3, 4, 5, 2, 3, 3, 4, 2, 
             7, 5, 6, 8, 4, 3, 7, 6, 6, 5, 2, 3, 6, 12, 7, 6, 2, 
             5, 5, 5, 6, 2, 5, 2, 3, 1, 7, 6, 3, 5, 4, 4, 1, 6, 3, 1, 7)
sd(mourner)
mean(mourner)

t.test(mourner, mu=4.69, alternative = 'two.sided')

sd(mourner)


t.test.Modified <- function(mean1,mean2,sd1,sd2,n1,n2){
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

mean.mourner=mean(mourner)
sd.mourner=sd(mourner)
len.mourner=length(mourner)

t.test.Modified(mean1 = 4.69, mean2 = mean.mourner, 
                sd1 = 2.6, sd2 =sd.mourner,
                n1 = 121, n2 = len.moruner)

x<-rnorm(n = 3000, mean = 100, sd = 50)
y<-rnorm(n = 3000, mean = 100, sd = 50)

t.test.Modified(mean1=3000, mean2=3000, sd1=50, sd2=50, n1=100, n2=100)
t.test(x, y)

