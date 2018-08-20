###This code is the first step in replacing missing standard deviation(sd) 
###values for meta-analysis. At present the approach is very clunky and needs
###refinement. 
#setwd("C:/Users/Matt/Documents/R folder")
#set seed
set.seed(10101)
###set up dataframe
df <- data.frame(m = runif(500,0,100), n = round(runif(500,1,100)),sd=runif(500,1,25))
head(df)
df$sd<-as.data.frame(lapply(df[3], function(cc) cc[sample(c(TRUE, NA), prob = c(0.85, 0.15), size = length(cc), replace = TRUE) ]))
df


####Assess if the SD in the data are missing
NaS<-which(is.na(df), arr.ind=TRUE)[,1]

#get the mean values from the df where the sd is missing
xm<-df$m[NaS]
#xm
  
#get the n values from the df where the sd are missing
xn<-df$n[NaS]
#xn
#make this a dataframe
Simdf<-data.frame(xm,xn)
#simulate the sd by using xm and xn in the rnorm function (rnorm assumes a Sd of 1 when it is not provided in the equation) 
#we could use other distributions rpois etc. Runif is different and needs a max and a min value (runif(n,min,max))
Simdf$sim.sd<-apply(Simdf, 1, function(x) sd(rnorm(n = x[2], mean = x[1])))
head(Simdf)
#replace missing values in the dataframe with the simulated variables
df$sd[is.na(df$sd) ]<- Simdf$sim.sd
df

###trying to put this in to a function - Not sure if this works!
imp_SD<-function(x){
  NaS<-which(is.na(x), arr.ind=TRUE)[,1]
#get the mean values from the df where the sd is missing
xm<-x$m[NaS]
#get the n values from the df where the sd are missing
xn<-x$n[NaS]
#make this a dataframe
Simdf<-data.frame(xm,xn)
#simulate the sd by using xm and xn in the rnorm function (rnorm assumes a Sd of 1 when it is not provided in the equation) 
#we could use other distributions rpois etc. Runif is different and needs a max and a min value (runif(n,min,max))
Simdf$sim.sd<-apply(Simdf, 1, function(x) sd(rnorm(n = x[2], mean = x[1])))
#replace missing values in the dataframe with the simulated variables
x$sd[is.na(x$sd) ]<- Simdf$sim.sd
}
#test
df2<-imp_SD(df)
df2
