## Sets and gets the solved inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function (y) {
        x <<- y
        m <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(inverse) {
        m <<- inverse
    }

    getInverse <- function() {
        m
    }

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Finds the inverse of an invertible matrix.
## Takes the return value of makeCacheMatrix as an argument
## and uses it to retrieve a cached solution for the inverse
## of a matrix, if present. Otherwise, solves for the inverse
## and caches the solution.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    if (!is.null(m)) {
        message('Getting cached data')
        return(m)
    }

    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)

    m
}
