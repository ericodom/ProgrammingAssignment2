## Author: Eric Odom
## Date: 1/16/2014
## Filename: cachematrix.R
##
## These functions help increase performance by checking if the
## inverse of the matrix has already been calculated and cached.


## This function takes a matrix and creates a list of functions
## that manage the storage and retrieval of the inverse of a
## matrix.  If there is no cached inverse value, NULL will be returned.
## Usage: testMatrix <- makeCacheMatrix(myMatrix)
makeCacheMatrix <- function(x = matrix()) {

    ## Create empty matrix variable
    m <- NULL
    
    ## Function to store the original matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Function to get the original matrix
    get <- function() x
    
    ## Function to store the inverse of the matrix in cache
    setinverse <- function(inverse) m <<- inverse
    
    ## Function to get the inverse of the matix from cache
    getinverse <- function() m
    
    ## List operations to call functions above
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function will check to see if the inverse of the 
## matrix has already been calculated and stored, if not
## then it will calculate the inverse and cache for future use
## Usage: cacheSolve(testMatrix)
cacheSolve <- function(x, ...) {
    
    ## Get the inverse of the matrix from the cached value
    m <- x$getinverse()
    
    ## Check to see if the cached value is null. 
    ## If the cached value is not null, then return cached inverse value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Load the original matrix into data variable
    data <- x$get()
    
    ## Calculate the inverse of the matrix
    m <- solve(data, ...)
    
    ## Store the inverse of the matrix for future use
    x$setinverse(m)
    
    ## Return the inverse value of the matrix
    m
}
