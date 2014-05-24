## Programming Assignment 2: 
## Caching Function Results in lexical scoping
## The overall aim is to allow the results of a function to be stored for
## later retrieval rather than having to be recalculated every time it
## is called with the same arguments

## makeCacheMatrix - Creates a list of functions that get and set 
## the value of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	mat <- NULL
	set <- function(y){
		x <<- y
		mat <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) mat <<-inverse
	getinverse <- function() mat
	list(set = set, get = get, setinverse = setinverse, 
	getinverse = getinverse)
}


## cacheSolve - calculates the inverse of the matrix created using makeCacheMatrix
## First it checks whether the result has already been calculated 
## If it has been calculated it skips the calculation and gets the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	mat <- x$getinverse()

	if(!is.null(mat)){
		message("getting cached data")
		return(mat)
	}
	data <- x$get()
	mat <- solve(data, ...)
	x$setmean(mat)
	mat
}
