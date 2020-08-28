## This functions cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_i <- function(inverse) i <<- inverse
    get_i <- function() i
    list(set = set, get = get,
         set_i = set_i,
         get_i = get_i)
}

## Computes the inverse of a matrix

cacheSolve <- function(x, ...) {
    i <- x$get_i()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_i(i)
    i
    ## Return a matrix that is the inverse of 'x'
}
