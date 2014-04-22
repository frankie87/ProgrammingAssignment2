## Function implements matrix inversion and caching
## as well as get and set this stuff

## make inverse matrix and create getters and setters

makeCacheMatrix <- function(x = matrix()) {
s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## If there is a cached copy of inversed matrix - get it, otherwise create new
## and add it to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
			 s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
