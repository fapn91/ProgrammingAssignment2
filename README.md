## makeCacheMatrix and cacheSolve creates a matrix and find its inverse. Both functions assume that the matrix is always 
## a square matrix

## makeCacheMatrix is a function which gets and sets the square matrix, and sets and gets its inverse.
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


## cacheSolve is a function that computes the inverse of the matrix taken from makeCacheMatrix. If the inverse has been 
## calculated previously by makeCacheMatrix, then it avoids the computation and returns the original inverse matrix.
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolve(s)
        s
}