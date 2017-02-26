## combining the 2 functions provided in this files, will give you the possiblity to cache the inverse of a matrix 
# so you do not need to compute it each time.

## creates a "cacheMatrix" from an ordinary matrix  

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(matrix) {
    matrix <<- matrix
    inverse <<- NULL
  }
  get <- function() matrix
  setInverse <- function(inverse) inverse <<- inverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# cache the matrix in the matrix expected a "CacheMatrix"
cacheInverse <- function(matrix, ...) {
  inverse <- matrix$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- matrix$get()
  
  # calulate the inverce of  a matrix
  inverse <- solve(data, ...)
  matrix$setInverse(inverse)
  inverse
}


# E.g

# matrix2 <- matrix(c(4,2,7,6), 2,2)
# cm2 <- makeCacheMatrix(matrix2)
# cacheInverse(cm2)
# # This will output a message that you are retriving cached result
# cacheInverse(cm2)


