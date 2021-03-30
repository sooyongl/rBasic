# rm(list = ls())
# install.packages("R6")
library(R6); library(tidyverse)

# variable name must be the same as classname
student <- R6::R6Class(
  classname = "student",

  # inherit = other class, 
  
  private = list( # this is like read-only.
  
    name = NULL,
    age = NULL
  ), 
  
  # active = list(),  # useful with private fields. 
  active = list(
    private_fields = function(value) {
      if (missing(value)) {
        c(private$age)
      } else {
        stop("`$age` is read only", call. = FALSE)
      }
    }
  ),
    
  public = list(
    
    # Define variable ( fields );
      gender = NULL, # when character
      midterm = NA, # when numeric
      final = NA,
      
      
    # Define member function (methods );
    
      ## initialization;
      initialize = function(name, age, gender, midterm, final) {
        
        # Validate initial values like below 
        # stopifnot(is.character(name), length(name) == 1)
        
         private$name <- name
         private$age <- age
         self$gender <- gender
         self$midterm <- midterm
         self$final <- final
      },
    
      ## public_fields() and list_attributes() are helper functions
      ## to get names and values of fields;
      public_fields = function(){
        return(names(get(class(self))$public_fields))
      },
                        
      list_attributes = function(){
        values <- purrr::map(self$public_fields(), ~.subset2(self, .x))
        names(values) <- self$public_fields()
        return(values)
      },

    # print() another helper function
    print = function(...){
      cat("Student: \n")
      cat(glue::glue("
                  Name  : {self$name}
                  age: {self$age}
                  gender: {self$gender}
                  Midterm Score : {self$midterm}
                  Final Score: {self$final}
              "))
      # invisible(self)
    }
    
  ) # list
) # Class

someone <- student$new(name = "Peter", age=12, gender="M", midterm = 0, final = 0)

someone$public_fields()

someone$private_fields

someone$list_attributes()
someone$public_fields()
someone$print()


# Add new thing
student$set("public", "total", NA)
student$set("public", "calculate_total", function(){
  self$total <- self$midterm + self$final
  invisible(self)
})

someone <- student$new(name = "Peter", age=12, gender="M", midterm = 0, final = 10)

someone$calculate_total()
someone$total


