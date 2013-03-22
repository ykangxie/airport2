##############################################
# PART I. Size Exploration
##############################################
files = gsub(".*([0-9]{4}.csv).*", "\\1", list.files()[grep(".*([0-9]{4}.csv).*", list.files())])
year = gsub(".*([0-9]{4}).csv.*", "\\1", files[grep(".*([0-9]{4}.csv).*", files)])

## (1)Explore size
cmd<-sprintf("wc -l %s",files)
system.time(counts<-sapply(cmd,function(cmd)system(cmd,intern=TRUE)));rm(cmd)
#user  system elapsed #13.130   4.645  25.024
popinfo<-as.data.frame(do.call(rbind,strsplit(counts," "))[,-1]);rm(counts)
colnames(popinfo)<-c("obs.counts","year")
popinfo[,1]<-as.integer(as.character(popinfo[,1]))
summary(popinfo[,1])
options(scipen=10)
par()$mar;par(mar=c(2,2,1,1))
dotchart(as.numeric(popinfo[,1]),labels=year,main="Counts of observations (1987-2008)")
abline(v=mean(popinfo[,1]),lty=2)
par(mar=c(5.1,4.1,4.1,2.1))

## (2) Decide proportion of sampling
prop<-round(100000/mean(popinfo[,1]),3)  #around 0.018
popinfo$sampling.counts<-sapply(popinfo[,1],function(x) ceiling(x*round(100000/mean(popinfo[,1]),3)))
#sample size for each year
popinfo[,-2]
#sample size for all years
sum(popinfo[,3]) #2204376

##############################################
# PART II. Sampling from Population
##############################################

####################
# Method (i)  perl #
####################
#user  system elapsed  #40.592  15.245  49.399.  #only 50 seconds to do the sampling for all years
system.time(system("cat [12]*.csv | perl -n -e 'print if (rand() < .018)' > airport_perl_sampling_allyear.csv"))
#sample size for all years
system("cat airport_perl_sampling_allyear.csv | wc -l") #2204629

###################
# Method (ii). R  #
###################
rsample.cat<-function(B,prop,con){
  if(prop*B<1) stop ("sampling size is too small for each reading block")
  B=as.integer(B)
  index=1:B
  #data constructing
  bag=c()
  #reading blocks
  while(TRUE){
    txt=readLines(con,n=B)
    if (length(txt)==0)
      break
    #do random sampling
    sample.index<-sample(index,prop*B)
    bag<-rbind(bag,txt[sample.index])
    rm(txt,sample.index)
    gc(verbose=FALSE)
  }
  cat(bag,file="airport_R.out",sep="\n")
}
con<-pipe("cat 2008.csv","r")
#con<-pipe("cat [12]*.csv","r")
system.time(rsample.cat(100000,0.018,con))
#user  system elapsed 
#101.364   0.488 102.629   for only 2008.csv; 
#we can expect that it would take 40 mins to take the sampling for all years
#we can expect that it would take 10 mins if we parallel the sampling in R
#perl is still much faster than R for only 50 seconds 

##############################################
# PART III. Data Construction
##############################################
#constructing
varname<-strsplit(readLines("2008.csv",n=1L),",")[[1]]
vartype<-c(rep("integer",4),rep("numeric",4),"factor","numeric","character",rep("numeric",5),rep("factor",2),"numeric",rep(NA,10))

#read in data
system.time(airport<-read.table("airport_perl_sampling_allyear.csv",sep=",",col.names=varname,colClasses=vartype,encoding = "UTF-8"))
#user  system elapsed 
#42.224   1.584  43.921 

#overview of sampling size
cc<-cbind(popinfo[,1],table(airport$Year),structure(popinfo[,3],names=year))
cc<-cbind(cc,cc[,3]-cc[,2],cc[,3]/cc[,1])
colnames(cc)=c("Population","Expectexd_Obs","Real_Obs","Diff","Proportion")
popinfo<-cc;rm(cc)

#save ~.RData
save(airport,popinfo,file="airport.rda")
rm(list = ls())

##############################################
# PART IV. Data Exploration
##############################################
load("./data/airport.rda")
names(airport)

###############
# 1. ArrDelay
###############
#responsive variable (rm NA)
table(airport$ArrDelay,useNA="ifany")
table(airport$Diverted)
table(airport$Cancelled)

#remove NA value
#NAs in ArrDelay are either diverted or cancelled
airport<-airport[airport$Diverted ==0 & airport$Cancelled ==0 , ]

#a glance of ArrDelay
hist(airport$ArrDelay,xlim=c(-40,40),breaks=1000,main="Histogram of Arrival Delay",xlab="Arrival Delay (-30,30)")

#Category Generation
qt<-quantile(airport$ArrDelay,probs=seq(0,1,0.2))
airport$ArrLabel<-with(airport, cut(ArrDelay, breaks=qt,
                  label=c("Very Early","Early","On Time","Late","Very Late") ,include.lowest=TRUE))

###################
# 2. NA Detection
###################
#exclude the five causes of delay
airport<-airport[,-c(20:29)]

table(complete.cases(airport))
# NAinfo<-lapply(airport,function(x)table(complete.cases(x)))
# $TailNum
# FALSE    TRUE 
# 660474 1497942 
# $AirTime
# FALSE    TRUE 
# 660475 1497941 
# $Distance       ####
# FALSE    TRUE 
# 3591 2154825

check<-airport[is.na(airport$Distance),] #3591 NA in distance
head(check);tail(check)
table(check$Year) #found that most of the NA in distance has also NA in tail num
table(check[!(is.na(check$TailNum)),]$Year) #only 92 NA in distance with complete info in tailnum
rm(check,NAinfo)

airport<-airport[!(is.na(airport$Distance)),]

#################
# 3. Predictors
#################
# Timing: Month, DayOfWeek,CRSDepTime
# Operation: UniqueCarrier 
# Travel: Distance
# Airport: Origin, Dest

#Airport Hub Info:
con=pipe("egrep '([A-Z][A-Z][A-Z])' list_of_hub","r")
#http://en.wikipedia.org/wiki/List_of_hub_airports#United_States
tt=readLines(con)
tt
hub = gsub(".*([A-Z][A-Z][A-Z]).*", "\\1", tt)
close(con);rm(con)

#hub or not
airport$Origin.Hub[airport$Origin %in% hub]<-"Hub"
airport$Origin.Hub[!(airport$Origin %in% hub)]<-"Non-Hub"
airport$Dest.Hub[airport$Dest %in% hub]<-"Hub"
airport$Dest.Hub[!(airport$Dest %in% hub)]<-"Non-Hub"
airport$Origin.Hub<-as.factor(airport$Origin.Hub)
airport$Dest.Hub<-as.factor(airport$Dest.Hub)

attach(airport)
table(Origin.Hub,Dest.Hub)
detach(airport)
#             Dest.Hub
# Origin.Hub    Hub  Non-Hub
# Hub         845316   567523
# Non-Hub     568767   223023


##############################################
# PART V. (1) randomForest-pre processing
##############################################
#formular for randomForest
names(airport)
y = c("ArrLabel")
x = c("Year","Month","DayOfWeek", "CRSDepTime", "UniqueCarrier", "Distance", "Origin.Hub", "Dest.Hub")
table(complete.cases(df.test[,c(y,x)]))

#traning vs. test dataset
set.seed(242)
index.test<-sample(1:nrow(airport),1/3*nrow(airport))
df<-airport[!(1:nrow(airport) %in% index.test),] #training subset
df.test<-airport[index.test,] #test subset
rm(index.test,airport)


#delete variables to save system memory and data Transfer
df<-df[,c(x,y)]
df.test<-df.test[,c(x,y)]
gc()
#variables from 22 to 9..1/3 saving. 
#save(df,file="./data/airportTrain.rda") #38.6M to 10.3M 
#save(df.test,file="./data/airportTest.rda") #24.3M to 7.9M
#in ram. is like 1G to 500M... saving 500M
rm(df.test)

##############################################
# PART V. (2) randomForest-classifing
##############################################
load('./data/airportTrain.rda')
library(randomForest)
library(parallel)

parRandomForest <- function(xx, ..., ntree = 300, mc = (detectCores()-1))
{ cl <- makeCluster(mc,type = "FORK")
  clusterEvalQ(cl, library(randomForest))
  rfwrap <- function(ntree, xx, ...) randomForest(x=xx, ntree=ntree, ...)
  rfpar <- parLapply(cl, rep(ceiling(ntree/mc), mc), rfwrap, xx=xx,...)
  stopCluster(cl)
  do.call(combine, rfpar)
}

y = c("ArrLabel")
x = c("Year","Month","DayOfWeek", "CRSDepTime", "UniqueCarrier", "Distance", "Origin.Hub", "Dest.Hub")


##############
#Test:ntree=99
##############
system.time(forest.nonpar<-randomForest(df[,x],df[,y], ntree=99,sampsize=50000,replace=FALSE))
gc()
#user  system elapsed 
#303.976   1.450 305.388 

detectCores() #we will assign detectCores()-1 worker notes
system.time(forest.par <- parRandomForest(df[,x], df[,y], mc=2, ntree=99,sampsize=50000,replace=FALSE))
#user  system elapsed 
#10.084   3.802 192.898 
gc()
system.time(forest.par.bagging <- parRandomForest(df[,x], df[,y], mc=2, ntree=99,sampsize=50000,replace=FALSE,mtry=length(x)))
#user  system elapsed 
#13.829   3.650 571.164 
gc()
system.time(forest.par.halfVars <- parRandomForest(df[,x], df[,y], mc=2, ntree=99,sampsize=50000,replace=FALSE,mtry=ceiling(length(x)/2)))
#user  system elapsed 
#14.760   3.585 350.399 
gc()

#load('forest.rda')
#save(forest.nonpar,forest.par,forest.par.bagging,forest.par.halfVars,file='forest.rda')

################
#Test:ntree=300
################
system.time(forest.nonpar<-randomForest(df[,x],df[,y], ntree=300,sampsize=50000,replace=FALSE))
#user   system  elapsed 
#850.285    3.617 1842.200 

system.time(forest.par <- parRandomForest(df[,x], df[,y], mc=2, ntree=300,sampsize=50000,replace=FALSE))
#user  system elapsed 
#10.084   3.802 1011.312 

system.time(forest.par.bagging <- parRandomForest(df[,x], df[,y], mc=2, ntree=300,sampsize=50000,replace=FALSE,mtry=length(x)))
#user   system  elapsed 
#18.145    8.919 1681.871 

system.time(forest.par.halfVars <- parRandomForest(df[,x], df[,y], mc=2, ntree=300,sampsize=50000,replace=FALSE,mtry=ceiling(length(x)/2)))
#user   system  elapsed 
#19.026    9.006 1035.875 

rm(df,x,y)
save(forest.nonpar,forest.par.halfVars,forest.par.bagging,file='forest_300.rda')

################
#Test:ntree=30000
################
system.time(forest.par <- parRandomForest(df[,x], df[,y], mc=3, ntree=30000,sampsize=9999,replace=FALSE))
#user  system elapsed 
#10.084   3.802 1011.312 

system.time(forest.par.bagging <- parRandomForest(df[,x], df[,y], mc=2, ntree=30000,sampsize=9999,replace=FALSE,mtry=length(x)))
#user   system  elapsed 
#18.145    8.919 1681.871 

system.time(forest.par.halfVars <- parRandomForest(df[,x], df[,y], mc=2, ntree=30000,sampsize=9999,replace=FALSE,mtry=ceiling(length(x)/2)))
#user   system  elapsed 
#19.026    9.006 1035.875 

#rm(df,x,y)
#save(forest.nonpar,forest.par.halfVars,forest.par.bagging,file='forest_300.rda')


################
#Prediction
################
load('./data/forest_300.rda')
load('./data/airportTest.rda')
y = c("ArrLabel")
x = c("Year","Month","DayOfWeek", "CRSDepTime", "UniqueCarrier", "Distance", "Origin.Hub", "Dest.Hub")

#prediction---default
pred.label<-predict(forest.nonpar, df.test[,x])
test.label = df.test[,y]
#confusion matrix
cmat<-table(test.label, pred.label)
cmat
#misclassification rate
1-sum(diag(cmat))/sum(cmat) ##mis.class 69.68%

#prediction---halfVars
pred.label<-predict(forest.par.halfVars, df.test[,x])
test.label = df.test[,y]
#confusion matrix
cmat<-table(test.label, pred.label)
cmat
#misclassification rate
1-sum(diag(cmat))/sum(cmat) ##mis.class 69.52%

#prediction---bagging
pred.label<-predict(forest.par.bagging, df.test[,x])
test.label = df.test[,y]
#confusion matrix
cmat<-table(test.label, pred.label)
cmat
#misclassification rate
1-sum(diag(cmat))/sum(cmat) ##mis.class 69.55%
