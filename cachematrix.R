## caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        q <- NULL
        set <- function(y) {
                x <<- y
                q <<- NULL
        }
        get <- function() x
        setsolve <- function(inver) q <<- inver
        getsolve <- function() q
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        q <- x$getsolve()
        if(!is.null(q)) {
                message("getting cached data")
                return(q)
        }
        data <- x$get()
        q <- solve(data, ...)
        x$setsolve(q)
        q
}
