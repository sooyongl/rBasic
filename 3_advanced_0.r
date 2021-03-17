library(lobstr); library(tidyverse)

mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")

x <- c(1, 2, 3)
cat(tracemem(x), "\n")

e1 <- rlang::env(a = 1, b = 2, c = 3)
e2 <- e1

# When creating it: 
x <- c(a = 1, b = 2, c = 3)

# By assigning a character vector to names()
x <- 1:3
names(x) <- c("a", "b", "c")

# Inline, with setNames():
x <- setNames(1:3, c("a", "b", "c"))

df1 <- data.frame(
  x = 1:3,
  y = c("a", "b", "c"),
  stringsAsFactors = FALSE
)

a1 <-
  tibble(
    x = 1:3, 
    y = list(1:2, 1:3, 1:4)
  )

a1$y

c(1,1,1,1)

x <- outer(1:4, 1:4, FUN = "*")
x[upper.tri(x)]


grades <- c(1, 2, 2, 3, 1)

info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)


id <- match(grades, info$grade)

df[setdiff(names(df), "z")]

x_option <- function(x) {
  switch(x,
         a = "option 1",
         b = "option 2",
         c = "option 3",
         stop("Invalid `x` value")
  )
}

for (i in 1:10) {
  if (i >= 5)
    break
  
  if (i < 3) 
    next
  
  print(i)
  
  
}

try()
supressMessages()

f2 <- function(x) {
  try(log(x))
}
f2("a")

f3 <- function(x) {
  tryCatch(
    error = function(cnd) NA,
    log(x)
  )
}

tryCatch(
  error = function(cnd) {
    paste0("--", conditionMessage(cnd), "--")
  },
  stop("This is an error")
)

library(Rcpp)