## Put comments here that give an overall description of what your
## functions do
## There will be 2 functions that will help reduce the time taken to  
## compute the inverse of matrices. The functions help to cache the 
## inverse of a matrix so that if the inverse of the same matrix 
## has to be calculated later on, we can retrieve this cached value.

## Write a short comment describing this function
## This function creates a matrix and caches its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## the cacheSolve function checks whether the inverse of
## the matrix created by makeCacheMatrix has been computed. If the 
## inverse has already been cached, then it will retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  input_matrix <- x$get()
  inv <- solve(input_matrix, ...)
  x$setInverse(inv)
  inv
}

