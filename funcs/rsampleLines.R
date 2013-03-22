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