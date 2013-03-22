parRandomForest <- function(xx, ..., ntree = 600, mc = (detectCores()-1))
{ cl <- makeCluster(mc,type = "FORK")
  clusterEvalQ(cl, library(randomForest))
  rfwrap <- function(ntree, xx, ...) randomForest(x=xx, ntree=ntree, ...)
  rfpar <- parLapply(cl, rep(ceiling(ntree/mc), mc), rfwrap, xx=xx,...)
  stopCluster(cl)
  do.call(combine, rfpar)
}

y = c("ArrLabel")
x = c("Year","Month","DayOfWeek", "CRSDepTime", "UniqueCarrier", "Distance", "Origin.Hub", "Dest.Hub")
