## makeCacheMatrix
##      This function caches a matrix (and it's inverse)
## cacheSolve
##      This function computes the inverse of a given matrix.
##      If the inverse matrix has been computed and the matrix
##      has not changed, this function returns a cached matrix


## This function creates a matrix that can be cached. It can also
## cache the inverse of the matrix which offers better performance
## (for large matricies)
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
		        x <<- y
		        m <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) m <<- inverse
		getinverse <- function() m
		list(set = set, get = get,
		     setinverse = setinverse,
		     getinverse = getinverse)
}


## This function returns the inverse of a given matrix.
## It checks to see if the inverse of the matrix
## has already been solved.  If so, it returns the
## cached data.  Otherwise it solves the inverse

cacheSolve <- function(x, ...) {
        ## get the cached inverse of the matrix
        m <- x$getinverse()
        
        ## check that we have actually calculated the inverse previously
        if(!is.null(m)) {
          
            n <- x$get()
            ## check that the matrix has not changed      
            if( is.matrix(x$get()) && is.matrix(n) && dim(x$get()) == dim(n) && all(x$get() == n) ) {
                    ## print message that we are returning cached data
                    message("getting cached data")
                    ## return cached inverse
                    return(x$getinverse())
            }
        }
        
        ## store matrix in temp variable
        data <- x$get()
        ## solve the inverse of the matrix
        m <- solve(data, ...)
        ## cache the inverse matrix
        x$setinverse(m)

        ## Return a matrix that is the inverse of 'x'
        m
}