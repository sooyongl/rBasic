data <- list( x <- 10, y <- x, z <- x + y)

ys <- substitute(y)
yq <- quote(y)

deparse(yq)


eval(substitute(z, data))
eval(quote(z), data)

ne <- new.env()
ne$x <- 20

eval(substitute(x, ne))
eval(substitute(x))

eval(quote(x), data)
eval(quote(x), ne)

sample_df <- data.frame(a = 1:10, b = 21:30, d = 0)
ne$sample_df <- sample_df
substitute(a, sample_df)
eval(quote(a > 2), sample_df)

subset2 <- function(x, condition) {
  
  condition_call <- substitute(condition)
  r <- eval(condition_call, x)
  x[r,]
}

subset2(sample_df, a >= 4)

condition_call <- 4
condition <- 4

subset2(sample_df, a == condition)

subset2 <- function(x, condition) {
  
  condition_call <- substitute(condition)
  r <- eval(condition_call, x, 
            #enclos = ne
            enclos = parent.frame()
  )
  x[r,]
}
subset2(sample_df, a == condition)

subset2 <- function(x, condition) {
  
  condition_call <- substitute(condition)
  env <- list2env(x, parent = parent.frame())
  r <- eval(condition_call, env)
  x[r,]
}
subset2(sample_df, a == condition)