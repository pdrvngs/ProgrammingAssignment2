# Disclaimer: The code in my solution is heavily based on the example given 
# in the assignment description. The explanations of what they do are not. 

## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix and cacheSolve take a matrix, turn it into a cacheable object,
## invert it and return the inverse. Will return the cached inverse if available.

## Write a short comment describing this function
# makeCacheMatrix takes a matrix and outputs a list of 4 functions that give 
# us the ability to set and store the input and inverse inside the "state" of the functions.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  get <- function() x 
  setsolved <- function(solve) invert <<- solve
  getsolved <- function() invert
  list(set = set, get =get, setsolved = setsolved, getsolved=getsolved)
}


## Write a short comment describing this function
# This function is the one that takes the cacheable object and operates on it 
# to give us the inverted matrix and then use the special properties of our
# cacheable matrix to "save" the changes into the state of the "makeCacheMatrix"
# function. Once saved, this function is also able to check with the state of
# makeCacheMatrix and recall the cached result for additional use. 

cacheSolve <- function(x, ...) {
  invert <- x$getsolved()
  if(!is.null(invert)){
    message("Fetching data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setsolved(invert)
  invert
        ## Return a matrix that is the inverse of 'x'
}
