## The makeCacheMatrix() function helps to cache
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setinv <- function(inversed) inv <<- inversed
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv = getinv)
}


## The followinf function cacheSolve() returns
## Inverse of a matrix. If the input matrix is same
## as immediate previous then it retrieves the result
## from cache otherwise it calculates afresh

cacheSolve <- function(x, ...) {
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
