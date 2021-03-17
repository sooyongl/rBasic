#################################################################
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

oper <- foreach(i=1:10, .combine='comb', .multicombine=TRUE,
                .init=list(list(), list())) %dopar% {
                  list(i+2, i+3)
                }

oper1 <- oper[[1]]
oper2 <- oper[[2]]

#################################################################3
library(foreach)
library(doParallel)

# Create class which holds multiple results for each loop iteration.
# Each loop iteration populates two properties: $result1 and $result2.
# For a great tutorial on S3 classes, see: 
# http://www.cyclismo.org/tutorial/R/s3Classes.html#creating-an-s3-class
multiResultClass <- function(result1=NULL,result2=NULL)
{
  me <- list(
    result1 = result1,
    result2 = result2
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"multiResultClass")
  return(me)
}

cl <- makeCluster(3)
registerDoParallel(cl)
oper <- foreach(i=1:10) %dopar% {
  result <- multiResultClass()
  result$result1 <- i+1
  result$result2 <- i+2
  return(result)
}
stopCluster(cl)

oper1 <- oper[[1]]$result1
oper2 <- oper[[1]]$result2

######################################################################
library(foreach)
library(doParallel)

# Create class which holds multiple results for each loop iteration.
# Each loop iteration populates two properties: $result1 and $result2.
# For a great tutorial on S3 classes, see: 
# http://www.cyclismo.org/tutorial/R/s3Classes.html#creating-an-s3-class
DMST_results <-
  setClass(
    "DMST_results",
    
    slots = c(
      res1 = "numeric", 
      res2 = "numeric")
  )

cl <- makeCluster(2)
registerDoParallel(cl)
oper <- foreach(i=1:10) %dopar% { # i = 1
  result <- DMST_results()
  result@res1 <- i+1
  result@res2 <- i+2
  return(result)
}
stopCluster(cl)

oper1 <- oper[[1]]$result1
oper2 <- oper[[1]]$result2

################################################################
comb <- function(...) {
  mapply('cbind', ..., SIMPLIFY=FALSE)
}

result <- foreach(i=1:100, .combine='comb', .multicombine=TRUE) %dopar% {
  vec1 <- rep(i, 10)
  vec2 <- rep(2*i, 10)
  list(vec1, vec2)
}

################################################################
cl <- makeCluster(2)
registerDoParallel(cl)
results2 <- 
  foreach(i = 1:10, 
          .packages=c('doParallel','foreach','base','abind')) %dopar% {
            
            i = 1
            
            a1 = array(data=sample(rnorm(100, mean=i, sd=1),100),dim=c(3,3,100))
            res1 <- aperm(a1,c(2,1,3))
            
            sum1 <- apply(res1, 3, sum)
            
            a2 = array(data=sample(rnorm(100, mean=sum1[1], sd=1),100),dim=c(3,3,100))
            res2 <- aperm(a2,c(2,1,3))
            sum2 <- apply(res2, 3, sum)
            
            a1 <- c(sum1[1],sum2[2])
            a2 <- c(sum1[2],sum2[3])
            a3 <- c(sum1[6],sum2[4])
            
            list(data = a1, sum = res1, mean = list(a1, a2, a3))
          }

library(purrr)
final <- list(data = do.call(acomb, map(results2, "data")), 
              sum  = do.call(c, map(results2, "sum")), 
              mean = do.call(c, map(results2, "mean")))



rbenchmark::benchmark(
  
  "for1" = {
    set.seed(16)
    list1 <- vector("list", 1000)        
    
    for (i in 1:1000) { 
      list1[[i]] = rnorm(5, 0, 1) 
    }
    list1
  },
  
  "for2" = {
    
    set.seed(16)
    list1 <- list()        
    for (i in 1:1000) { 
      list1[[i]] = rnorm(5, 0, 1) 
    }
    list1
  },
  
  "replicate" = {set.seed(16)
    replicate(n = 1000, rnorm(5, 0, 1), simplify = FALSE )
  },
  
  "rerun" = {
    set.seed(16)
    purrr::rerun( .n=1000, rnorm(5,0,1) )
  },
  
  "foreach" = {
    set.seed(16)
    foreach(i = 1:1000) %do% {
      rnorm(5, 0, 1)
      
    }
  }
  
)



