## Below are two functions that are used to create a special object that stores a
#matrix and cache the inverse of that matrix.

## The first function creates a special matrix object that can catche its inverse,
#by means of using a list of functions to (1) set de value of the matrix, (2) get the 
#value of the matrix, (3) set the value of the inverse, and (4) get the value of the 
#inverse:

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix returned by the previous 
#function (CacheMatrix). If the inverse has already been calculated the function retrives
#the inverse from the cache and skips the computation. Otherwise it calculates the inverse 
#of the data matrix and sets its value in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse() 
        if(!is.null(i) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i           
}
