## Functions to efficiently deal with an invertible matrix and its
## inverse.
##
## The invertible matrix is wrapped using makeCacheMatrix(x). The wrapper
## can then be passed to cacheSolve(x) to produce the inverse. 
## Subsequent calls to cacheSolve(x) with the same wrapper will 
## continue to return the inverse without incurring the repeated
## expense of computing the inverse.
##
## Example:
##  m <- matrix(c(2, 0, 0, 2), 2, 2)
##  wrapper <- makeCacheMatrix(m)    # Wrap the matrix
##  inverse <- cacheSolve(wrapper)   # Get the inverse of m
##
## The wrapper supports functions to get and set the matrix and the
## cached inverse:
##  wrapper$setMatrix(m)   # Replace the matrix and clear the cached inverse
##  wrapper$getMatrix()    # Get the current matrix
##  wrapper$setInverse(i)  # Install the given matrix as the cached inverse
##  wrapper$getInverse()   # Get the currently cached inverse matrix; NULL if not set


## Return a wrapper that holds an invertible matrix 'x' and its inverse
makeCacheMatrix <- function(x = matrix()) {
	# The cached inverse of 'x'; NULL if not yet computed
	matrixInverse <- NULL

	# Functions to get and set the matrix and its inverse
	setMatrix <- function(mtrx) {
		x <<- mtrx
		matrixInverse <<- NULL
	}
	getMatrix <- function() x
	setInverse <- function(inverse) matrixInverse <<- inverse
	getInverse <- function() matrixInverse

	# Package the functions for this wrapper into a list
	list(setMatrix = setMatrix, 
	     getMatrix = getMatrix, 
	     setInverse = setInverse, 
	     getInverse = getInverse)
}


## Return a matrix that is the inverse of the matrix wrapped in 'x'
cacheSolve <- function(x, ...) {
	# If the inverse has already been computed and cached, return it
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		return(inverse)
	}

	# The inverse was not cached, update the cache and return the inverse
	inverse <- solve(x$getMatrix(), ...)
	x$setInverse(inverse)
	inverse
}
