## These functions are used for Programming Assignment 2 of rprog-034
## The purpose of this assignment is to make 2 functions to cache inverse of a special matrix

## This function takes matrix x to be converted into a special matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(mat) {
		mat <<- x
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inv_inp) inv <<- inv_inp
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes a special matrix, calculates the inverse of the matrix and saves it into cache
## If the inverse of the matrix has been calculated before, it will return the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
