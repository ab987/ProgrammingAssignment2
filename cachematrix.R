## Cache the results of inverting a matrix.
## Then on subsequent calls, return the cached value rather than
## recomputing it.

## Create a special matrix, which is actually a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) {
		inv <<- inverse
	}
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## If the inverse has already been computed, then return it.
## Otherwise, compute it, save it, and then return it.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
	
