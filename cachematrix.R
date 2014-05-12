## These functions provide the capability to get the inverse of a square
## matrix and to cache the result. If a request is made for the inverse 
## of a matrix that has already been computed, the cached inverse is returned.


## This function defines a "class" that creates a cache-able matrix.
## It provides functions to get/set the matrix and to
## obtain its inverse (with solve function) and retrieve the inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  
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


## This function defines the sub-functions that will call the function
## to perform the inverse and will cache and retrieve an inverse that 
## has been previously computed.

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

## Test the cache matrix capability.
testCacheMatrix <- function() {
        # Create a little matrix and make it a CacheMatrix.
        seq1 <-c(1,2,1,3,4,3,1,1,4)
        mat1<-matrix(seq1, 3)
        a <- makeCacheMatrix(mat1)
        print("Call cacheSolve for matrix 1 first time")
        ainv<- cacheSolve(a) # Inverse
        print(ainv)

        # Get cached inverse of first matrix
        print("Call cacheSolve for matrix 1 second time -- get from cache")
        a2inv<-cacheSolve(a)
        print(a2inv)
}
