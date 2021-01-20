## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates an object with a special "matrix" object which is able to cache its inverse.
  
  ## Initializing the inverse variable
  ivr <- NULL
  
  ## Creating a method to set the matrix
  set <- function(m) {
    x <<- m
    ivr <<- NULL
  }
  
  ## Creating a method to get the matrix
  get <- function() x
  
  ## Creating a method to set the inverse of the matrix
  setInverse <- function(inverse) ivr <<- inverse
  
  ## Creating a method to get the inverse of the matrix
  getInverse <- function() ivr
  
  ## At the end, the function is returning a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## This function returns the inverse matrix of X
  
  ## Creating a variable to return the inverse of the matrix of 'x'
  ivr <- x$getInverse()
  
  ## Creating a method to only return the inverse of the matrix if its already set
  if (!is.null(ivr)) {
    message("getting cached data")
    return(ivr)
  }
  
  ## Getting the matrix
  data <- x$get()
  
  ## Calculating the inverse using solve function
  ivr <- solve(data, ...)
  
  ## Seting the inverse matrix into the object created in the previous function
  x$setInverse(ivr)
  
  ## Returning the inverse matrix
  ivr
}



