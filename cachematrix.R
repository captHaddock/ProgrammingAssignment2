## This script contains two functions that together can help to reduce time-
## consuming calculation of a matrix, for example in loops, by using the
## cashe in R.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    ## Use "<<-" and not "<-" to be able to use it in different environments
    x <<- y  
    m <<- NULL
  }
  get <- function() x 
  ## the function solve will be used to calcultate the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  ## Create a list of the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() # get the inverse matrix
  
  ## If the inverse of the matrix is already calculated, just return it from
  ## the cashe.  
  if(!is.null(m)) {
    message("getting cached data")
    return(m) # give back the inverse matrix
  }
  
  ## if it is not already calucated, calculate the inverse and store it in 
  ## the cashe. 
  data <- x$get() # get the matrix 
  m <- solve(data, ...) # calculate the inverse of the matrix
  x$setinverse(m) # store the inverse matrix in cache
  m # give back the inverse matrix
}


## Testing the functions with an example:

## Create a test 3x3 matrix
# testmatrix <- matrix(c(1, -5, 0, 0, 1, 0, 0, 0, 1), 3, 3)
# testmatrix
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]   -5    1    0
# [3,]    0    0    1

## Calculate the inverse using the functions, looks OK!
# cacheSolve(makeCacheMatrix(testmatrix))
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    5    1    0
# [3,]    0    0    1

