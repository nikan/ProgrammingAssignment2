## Two functions for caching and retrieving the inverse 
## of an inversible matrix. 
## makeCacheMatrix is the function that acts as the cache
## cacheSolve is producing the inverse of a matrix or, if exists, 
## retrieves it from the cache 

## Creates a special list which contains four functions: 
## get, set, getsolve,setsolve
## set sets the value of the matrix
## get retrieves the value of the matrix
## setsolve sets the value of the inverse matrix
## getsolve retrieves the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Computes the inverse of a matrix, or retrieves it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
