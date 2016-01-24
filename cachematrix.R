#Functions to cache the inverse of a matrix. 

#function to create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  
  #set funtion
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  #get function
  get <- function() x
  
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#funtion to return the cached inverse of matrix (if cached), 
#else compute the inverse and return the same.
cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if (!is.null(inver)) {
    #print message to indicate that data is being retrieved from cache
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver
}
