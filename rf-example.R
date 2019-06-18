rm(list = ls())

Sys.setenv(HADOOP_HOME='/usr/lib/hadoop-0.20-mapreduce')
Sys.setenv(HADOOP_CMD='/usr/bin/hadoop')
Sys.setenv(HADOOP_STREAMING='/usr/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming-2.0.0-mr1-cdh4.7.0.jar')

#data <- read.table("file3.txt", sep="")
#data <- read.table("file8.txt", sep="")
#data <- read.csv("hardwood_carpet.csv", head=FALSE)
#data <- read.csv("nslkdd.csv", head=FALSE)

data <- read.csv("shuffledYogurt.csv", head=TRUE)


kee <- dim(data)[2]
vee <- kee-1

library(rmr2)
library(rhdfs)

hdfs.init()
data.content <- to.dfs(data)

library(randomForest)

# MAPPER
poisson.subsample <- function(k, v) {
  #key <- v[,kee]
  mm <-mean(v[,32])
  key <- ifelse(v[,32] < mm, "less", "greater")
  val <- v #c(scale(v[,1]),scale(v[,2]))
  #rmr.str(val)
  keyval(key,val)
}


# REDUCE function
fit.trees <- function(k, v) {
  
  #tmp <- k * (v[,1]/v[,1])
  #rmr.str(length(k))
  rmr.str(v[,65])
  rf <- randomForest(y=as.factor(v[,65]),
                     x=v[,1:64],
                     ntree=1000,
                     importance=TRUE)
  keyval(k, list(forest=rf))
}

classify <- mapreduce(input=data.content,
                      map=poisson.subsample,
                      reduce=fit.trees)

aa = from.dfs(classify)[["val"]]
aa

my_combine <- function (...)
{
  pad0 <- function(x, len) c(x, rep(0, len - length(x)))
  padm0 <- function(x, len) rbind(x, matrix(0, nrow = len -
                                              nrow(x), ncol = ncol(x)))
  rflist <- list(...)
  areForest <- sapply(rflist, function(x) inherits(x, "randomForest"))
  if (any(!areForest))
    stop("Argument must be a list of randomForest objects")
  rf <- rflist[[1]]
  classRF <- rf$type == "classification"
  trees <- sapply(rflist, function(x) x$ntree)
  ntree <- sum(trees)
  rf$ntree <- ntree
  nforest <- length(rflist)
  haveTest <- !any(sapply(rflist, function(x) is.null(x$test)))
  vlist <- lapply(rflist, function(x) rownames(importance(x)))
  numvars <- sapply(vlist, length)
  if (!all(numvars[1] == numvars[-1]))
    stop("Unequal number of predictor variables in the randomForest objects.")
  for (i in seq_along(vlist)) {
    if (!all(vlist[[i]] == vlist[[1]]))
      stop("Predictor variables are different in the randomForest objects.")
  }
  haveForest <- sapply(rflist, function(x) !is.null(x$forest))
  if (all(haveForest)) {
    nrnodes <- max(sapply(rflist, function(x) x$forest$nrnodes))
    rf$forest$nrnodes <- nrnodes
    rf$forest$ndbigtree <- unlist(sapply(rflist, function(x)
      x$forest$ndbigtree))
    rf$forest$nodestatus <- do.call("cbind", lapply(rflist,
                                                    function(x)
                                                      padm0(x$forest$nodestatus, nrnodes)))
    rf$forest$bestvar <- do.call("cbind", lapply(rflist,
                                                 function(x)
                                                   padm0(x$forest$bestvar, nrnodes)))
    rf$forest$xbestsplit <- do.call("cbind", lapply(rflist,
                                                    function(x)
                                                      padm0(x$forest$xbestsplit, nrnodes)))
    rf$forest$nodepred <- do.call("cbind", lapply(rflist,
                                                  function(x)
                                                    padm0(x$forest$nodepred, nrnodes)))
    tree.dim <- dim(rf$forest$treemap)
    if (classRF) {
      rf$forest$treemap <- array(unlist(lapply(rflist,
                                               function(x)
                                                 apply(x$forest$treemap, 2:3, pad0,
                                                       
                                                       nrnodes))), c(nrnodes, 2, ntree))
    }
    else {
      rf$forest$leftDaughter <- do.call("cbind", lapply(rflist,
                                                        function(x)
                                                          padm0(x$forest$leftDaughter, nrnodes)))
      rf$forest$rightDaughter <- do.call("cbind", lapply(rflist,
                                                         function(x)
                                                           padm0(x$forest$rightDaughter, nrnodes)))
    }
    rf$forest$ntree <- ntree
    if (classRF)
      rf$forest$cutoff <- rflist[[1]]$forest$cutoff
  }
  else {
    rf$forest <- NULL
  }
  #
  #Tons of stuff removed here...
  #
  if (classRF) {
    rf$confusion <- NULL
    rf$err.rate <- NULL
    if (haveTest) {
      rf$test$confusion <- NULL
      rf$err.rate <- NULL
    }
  }
  else {
    rf$mse <- rf$rsq <- NULL
    if (haveTest)
      rf$test$mse <- rf$test$rsq <- NULL
  }
  rf
}

#######################################################################################

#forest <- do.call(combine, aa)
#summary(forest)

forest <- do.call(my_combine, aa)
summary(forest)

forest$y

forest$oob.times

forest$forest
