title: "Programming Assignment 2: Lexical Scoping"
author: "MCurcio"
date: "12/7/2020"

# Purpose:

# This exercise discusses the concept of Lexical Scoping.

# Definition:
# Lexical Scoping in R programming means that the values of the free variables
# are searched for in the environment in which the function was defined.
# An environment is a collection of symbols, value, and pair, every environment
# has a parent environment it is possible for an environment to have multiple
# children but the only environment without the parent is the empty environment.
# If the value of the symbol is not found in the environment in which the
# function was defined then the search is continued in the parent environment.
# In R, the free variable bindings are resolve by first looking in the
# environment in which the function was created.
# [Lexical Scoping](https://www.geeksforgeeks.org/lexical-scoping-in-r-programming/)

# One resource that I found very helpful was from the Coursera mentor, Leonard Greski.
# I would recommend it to people taking this course.
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

# The `makeCacheMatrix` function & description

# The `makeCacheMatrix` is a function which contains five steps.

# 1. The `set` func. clears/deletes any previously held value of matrix,m and its inverse.
# 2. The `get` function returns a value of a matrix,m stored.
# 3. `setsolve` determines the inverse matrix of a matrix held in `m`.
# 4. `getsolve` returns a value of the inverse matrix (A') stored or calculated.
# 5. Although this last line is not a function, perse, it stores both values
#    of the matrix and its inverse into a list.


makeCacheMatrix <- function(x = matrix()) {
      # `x = matrix` above opens a variable,x that is a matrix
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x # returns the matrix, x
      setsolve <- function(solve) m <<- solve # calculates the A'
      getsolve <- function() m # returns the solved value, A'.
      list(set = set, # gives the name 'set' to set() function defined above
           get = get, # gives the name 'get' to get() function defined above
           setsolve = setsolve, # " " name 'setsolve' to setsolve() function defined above
           getsolve = getsolve) # " " name 'getsolve' to getsolve() function defined above
}


##  The cacheSolve function & description

# The `cacheSolve` is a function which contains five steps.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}

# TEST

x = matrix(c(3,-7,5,2), nrow = 2, ncol = 2)

Inverse_matrix <- makeCacheMatrix(x)
Inverse_matrix$get()       # retrieve the value of x
Inverse_matrix$getsolve()  # retrieve the value of matrix(m) which should be NULL
Inverse_matrix$set(x)      # reset value with inverse matrix
cacheSolve(Inverse_matrix) # notice the inverse matrix
Inverse_matrix$getsolve()

