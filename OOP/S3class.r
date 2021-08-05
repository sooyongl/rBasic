# class append ---------------
a <- 1
class(a)
class(a) <- "new"
class(a)

attr(a, "class") <- "numeric"
class(a)

a <- structure(a, class = "new")
class(a)

class(a) <- append(class(a), "numeric")
attr(a, "class")

inherits(a, "new")

# generic function ----------------

anyGenericFunction <- function(x, ...) {
  UseMethod("anyGenericFunction", x)
}


anyGenericFunction.default <- function(x) x

anyGenericFunction.new <- function(x, y) { 
  paste0("This is ", x, "and ", y)
}

anyGenericFunction.new1 <- function(x, ...) { 
  inputs <- list(...)
  paste0("This is ", x, "and ", inputs[["y"]])
}

anyGenericFunction(a, y = 1)

methods("anyGenericFunction")

# internal generics -----------------------
# internal C functions like length(), rep(), ...


# Inheritance ---------------------------
#' NextMethod() : dispathing based on the second (subsequent or next ) class
# call the next method

baz <- function(x) UseMethod("baz", x)
baz.A <- function(x) "A"
baz.B <- function(x) "B"

ab <- structure(1, class = c("A", "B"))
ba <- structure(1, class = c("B", "A"))
baz(ab)
baz(ba)


baz.C <- function(x) c("C", NextMethod())
ca <- structure(1, class = c("C", "A"))
cb <- structure(1, class = c("C", "B"))
baz(ca)
baz(cb)

# Turn object into class A - doesn't work!
baz.D <- function(x) {
  class(x) <- "A"
  NextMethod()
}
da <- structure(1, class = c("D", "A"))
db <- structure(1, class = c("D", "B"))
da <- baz(da)
class(da)
baz(db)

#
x <- 1
attr(x,'class') <- c('first','second')
Cate <- function(x,...)UseMethod('Cate')

Cate.first <- function(x,...){
  print(match.call())
  print(paste('first:',x))
  print('---------------------')
  NextMethod()## This will call Cate.second
}

Cate.second <- function(x,y){
  print(match.call())
  print(paste('second:',x,y))
}

Cate(x,1:3)

















