## This pair of functions calculate the inverse of a matrix and cache its value
## so that subsequently the inverse can be returned without having to calculate 
## it again

## this function takes a matrix as a parameter and returns a list of 
## 4 functions that get and set the matrix and get and set its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function returns the inverse of a matrix 
## if the inverse has not already been calculated for the input matrix
## it is calculated, cached and returned
## if it has already been calculated the cached value is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
