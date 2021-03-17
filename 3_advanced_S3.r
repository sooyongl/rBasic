# generate Class
## 클래스를 부여해주는 함수
mkclass <- function(class, ...){
  obj <- list(...)
  class(obj) <- ifelse(missing(class), "default", class)
  return(obj)
}

## 메쏘드를 사용하는 함수
area <- function(obj) {
  UseMethod("area", obj)
}
## 해당 메쏘드 안에 있는 실제 실행함수
area.default <- function(obj){
  res <- sum(unlist(obj))
  return(res)
}
area.rectangle <- function(obj){
  return(obj[["x"]] * obj[["y"]])
}
area.circle <- function(obj){
  radi <- obj[["radius"]]
  res <- pi*radi^2
  return(res)
}

## Test
r <- mkclass(x = 10, y = 20, z = 10, w = -100)
a <- area(r)
#
r <- mkclass(class = 'rectangle',
             x = 10, y = 20)
a <- area(r)
##
r <- mkclass(class = 'circle',
             radius = 10)
a <- area(r)