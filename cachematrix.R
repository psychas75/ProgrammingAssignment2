## The functions defined below are used to create a matrix object
## and caches its inverse.

## The makeCacheMatrix function creates a matrix and returns a
## list of functions that will set and get the value of the matrix
## and set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {

	s <- NULL
	set <- function(y, nrows, ncols) {
		x <<- matrix(y, nrows, ncols)
		s <<- NULL
	}
	
	get <- function() x

	setInversion <- function(inversion) s <<- inversion

	getInversion <- function() s

	list(
		set = set, 
		get = get, 
		setInversion = setInversion,
		getInversion = getInversion
	)
}


## The cacheSolve function returns the inverse of the matrix returned by
## by the makeCacheMatrix function. It will first check if the inverse 
## is cached. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
	s <- x$getInversion()

	if(!is.null(s)) {
		message("Retrieving cached data...")
		return(s)
	}

	data <- x$get()
	s <- solve(data)
	x$setInversion(s)
	return(s)
}












