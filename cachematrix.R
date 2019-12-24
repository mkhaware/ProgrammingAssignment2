## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #This function takes a matrix and creates a cache
  cacheMatrix <- NULL
  
  #define setMatrix method 
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  #define getMatrix method that returns matrix 'x'
  getMatrix <- function() x
  
  #define setCache method that updates cacheMatrix with inverse
  setCache <- function(inverse) cacheMatrix <- inverse
  
  #define getCache method that returns the inverse of matrix 'x'
  getCache <- function() cacheMatrix
  
  # list methods exposed through this function
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheMatrix <- x$getCache()
  
  #if cacheMatrix is not null then return cacheMatrix
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix")
    return(cacheMatrix)
  }
  #if it is null then use createCacheMatrix function to set the value and then return
  else {
    myMatrix <- x$getMatrix()
    cacheMatrix <- solve(myMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
}
