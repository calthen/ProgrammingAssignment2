## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  #Store matrix to be used
  #Clear previous cache
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Return the Matrix that is stored for use in other operations
  get <- function() x
  
  #Cache value for the inverse of the matrix
  setInverse <- function(Inverse) i <<- Inverse
  
  #Return currenty cached value
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Obtain current value of cache (or Null if it has not bee set yet)
  i <- x$getInverse()
  
  #Test for Null, 
  #If cache has been set (isnot null), return the vaue of cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #if cache hasn't been set then perform the calculation and cache the result
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
