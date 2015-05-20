## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list object for its matrix argument that allows for caching of its inverse
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

## Write a short comment describing this function
## This function examines the matrix argument for caching of its inverse
## If cached, the cached result is returned, otherwise it is calculated, cached and returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    orig <- x$get() # get original data
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <-solve(orig) # inv is inverse of original matrix
    x$setinv(inv) # write this to the list object
    inv
}