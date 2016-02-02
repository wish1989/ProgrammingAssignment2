## caching the inverse of a matrix

## Write a short comment describing this function

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


## Write a short comment describing this function

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
