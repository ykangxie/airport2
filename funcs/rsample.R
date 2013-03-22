rsample<-function(B,prop,con,varname,vartype){
  if(prop*B<1) stop ("sampling size is too small for each reading block")
  B=as.integer(B)
  index=1:B
  #data constructing
  bag=c()
  #reading blocks
  while(TRUE){
    txt<-read.table(con,sep=",",col.names=varname,colClasses=vartype,nrows=B)
    if (length(txt)==0)
      break
  #do sampling
  sample.index<-sample(index,prop*B)
    if (length(txt) < B) {
      sample.index<-sample(seq(1,length(txt)),prop*length(txt))
    }
  bag<-rbind(bag,txt[sample.index,])
  rm(txt,sample.index)
  gc(verbose=FALSE)
  }
  bag
}




#constructing
varname<-strsplit(readLines("2008.csv",n=1L),",")[[1]]
vartype<-c(rep("integer",4),rep("numeric",4),"factor","numeric","character",rep("numeric",5),rep("character",2),"numeric",rep(NA,10))

#sampling lines# 
con<-pipe("cat tmp.out","r")
tmp<-readLines(con)
tmp2<-matrix(scan(con, character(0), sep = ","),ncol=29)
tmp<-read.table(con,sep=",",col.names=varname,colClasses=vartype)


#sampling lines# 
con<-pipe("cat 2008.csv | head -n 210000","r")
Rprof("sample.out")
system.time(tmp<-rsample(100000,prop,con,varname,vartype))
Rprof(NULL)
