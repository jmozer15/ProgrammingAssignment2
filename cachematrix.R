## This is a sequence of two functions that work within the in tandem to cache a potentially large data matrix and calculate the inverse of that matrix.

## the first function is creating a matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y  ## assigning the value of y to x.
		m <<- NULL ##assigning the value of NULL to m.
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	List(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## this function computes the inverse of the matrix function returned by the first function "makeCacheMatrix".

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
        ## Return a matrix that is the inverse of 'x'
}
