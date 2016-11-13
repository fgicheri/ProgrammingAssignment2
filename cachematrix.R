## Put comments here that give an overall description of what your
## functions do
## The role of these two functions is to create a cache to enhance the speed of performing the inverse of a matrix. 
## Write a short comment describing this function
## This function creates a special matrix which caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
	mx <- NULL
	set <- function(y) {
    x <<- y
    mx <<- NULL
  }
	get <- function () x
	setinverse <- function(solve) mx <<- solve
	getinverse <- function() mx
	list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix created by the function above. If the inverse had previously 
## been calculated for the same matrix, the function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		mx <- x$getinverse()
		if(!is.null(mx)) {
		message("getting cached data")
		return(mx)
  }
  data <- x$get()
  mx <- solve(data, ...)
  x$setinverse(mx)
  mx
}
