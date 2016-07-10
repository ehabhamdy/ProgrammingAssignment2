## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The following Function (makeCacheMatrix) is used to create a new matrix
# and return four functions to set, get the matrix or set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
##The main purpose of the following function is to compute the inverse of a 
# give matrix with solve() function and cache it value so it will calculated one
# and it returns its value from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

