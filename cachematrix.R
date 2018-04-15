## makeCacheMatrix creates a list that contains the matrix as well as the 
## inverse matrix
## cacheSolve is a function that computes the inverse of the matrix contained 
## in the makeCacheMatrix function. If the inverse has already been calculated,
## it will return the cached results otherwise calculate by the solve function.

## create list that contains the matrix as well as functions to set and get
## get returns the original matrix
## set accepts a matrix(object) and will reset inv to null
## setinv will set the inv global variable to the provided parameter
## getinv will return the current value of inv

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve will return the invese of the matrix
## cacheSolve accepts an makeCacheMatrix object as x
## if the object inv is not null then it will return that value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
