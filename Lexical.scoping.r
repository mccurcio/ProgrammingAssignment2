# FROM: https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, # gives the name 'set' to set() function defined above
           get = get, # gives the name 'get' to get() function defined above
           setmean = setmean, # " " name 'setmean' to setmean() function defined above
           getmean = getmean) # " " name 'getmean' to getmean() function defined above
}


# Here it's important to note that the cachemean() function
# REQUIRES an input argument of type makeVector().

cachemean <- function(x, ...) {
      m <- x$getmean()
      if (!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}


# Putting the Pieces Together: How the functions work at runtime

aVector <- makeVector(1:10)
aVector$get()               # retrieve the value of x
aVector$getmean()           # retrieve the value of m, which should be NULL
aVector$set(30:50)          # reset value with a new vector
cachemean(aVector)          # notice mean calculated is mean of 30:50, not 1:10
aVector$getmean()           # retrieve it directly, now that it has been cached













