## These functions offer a way to store the expensive "matrix inverse" calculation for reuse. 

## The makeCacheMatrix is a function that creates a list of parameters on the user supplied matrix object. It allows you to "set"
## the object and subsequently get the object as well as set its inverse and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        
}


## The cacheSolve function will provide the inverse matrix of user inputted matrix from makeCacheMatrix. If an inverse
## for the matrix has already been cached, it will return the cached matrix. If no cached inverse exists
## it will calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Returns the inverse of the supplied matrix        
        
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
        
        
}
