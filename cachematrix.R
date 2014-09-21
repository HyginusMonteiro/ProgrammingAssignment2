## Program Name: cachematrix.R
## Created Date: 21-Sep-2014
## Overview: This program creates 2 functions that cache the inverse of a matrix for better performance.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		setmat <- function(y) {
				x <<- y
				i <<- NULL	
		}
		getmat <- function() x
		setmatinv<- function(solve) i <<-solve
		getmatinv <- function() i
		list(setmat = setmat, getmat = getmat,setmatinv = setmatinv,getmatinv = getmatinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
## Return a inverse matrix
## Check if present in cache
		i <- x$getmatinv()
		if (!is.null(i)) {
			message("Getting data from cache")
			return(i)
		} 
## Use solve function if inverse not present in cache
		else {
			i <- solve(x$getmat())
			message("Computing data(not from cache)")
			x$setmatinv(i)
			return(i)
		}
}