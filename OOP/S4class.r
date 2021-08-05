# S4 is much stricter
# Class name had better start with capital letter.

# validity
check_person <- function(object) {
  errors <- character()
  length_age <- length(object@age)
  if (length_age != 1) {
    msg <- paste("Age is length ", length_age, ".  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  
  length_name <- length(object@name)
  if (length_name != 1) {
    msg <- paste("Name is length ", length_name, ".  Should be 1", sep = "")
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) TRUE else errors
}


setClass("Information",
         slots = c(
           name = "character",
           age = "numeric",
           grade = "list"
         ),
         prototype = list(
           name = NA_character_,
           age = NA_real_,
           grade = list()
         ),
         
         # sealed = T,
         
         validity = check_person
         
         #   function(object) {
         #   if (!is.character(object@name)) {
         #     stop("@name must be a type of character")
         #   }
         #   return(TRUE)
         # }
)

user1 <- new("Information", name = "Lee", age = 30)
slot(user1, "name") # same as [[ ]]
slot(user1, "name") <- "soo"

getSlots("Information")
validObject(user1)

# removeClass("Information")


# Generic -----------------------------------
setClass("Shape")
setClass("Polygon", 
         slots = c(
           sides = "numeric"
         ), 
         contains = "Shape")
setClass("Triangle", contains = "Polygon")
setClass("Square", contains = "Polygon")
setClass("Circle", contains = "Shape")

setGeneric("sides", function(object) {
  standardGeneric("sides")
})

setMethod("sides", 
          signature(object = "Polygon"), 
          function(object) {
            object@sides^2
          })

poly <- new("Polygon", sides = 10)

sides(poly)

showMethods("sides")
showMethods(class = "Polygon")
# removeMethod()


# Method dispatch --------------------------


# S3 used with setOldClass() ---------------
# see also dotsMethods()
foo <- structure(list(x = 1), class = "foo")
type(foo)

setOldClass("foo")
setMethod("type", signature("foo"), function(x) "foo")

type(foo)

setMethod("+", signature(e1 = "foo", e2 = "numeric"), function(e1, e2) {
  structure(list(x = e1$x + e2), class = "foo")
})
foo + 3

is(foo)
# Inheritance
# callNextMethod()


a1 <- lubridate::period()
class(a1)
getSlots("Period")


# refer to https://advanced-r-solutions.rbind.io/s4.html



