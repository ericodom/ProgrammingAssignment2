## These functions help increase performance of calculating the 
## inverse of a matrix by caching the inverse value for future use

## This function takes a matrix and creates a list of functions
## that cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function will check to see if the inverse of the 
## matrix has already been calculated and stored, if not
## then it will calculate the inverse and cache for future use
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
