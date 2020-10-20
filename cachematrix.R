## Creating a pair of functions that cache the inverse of a matrix 

## Fistly, I am creating the function 'makeCacheMatrix'. 
## This function creates a special "matrix x" that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y){
                x <<- y
                i <<- NULL        
        }
        get <- function()x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Now, I am creating the function 'cacheSolve' to compute the inverse 
## of "x" returned by the function 'makeCasheMatrix'above.
## However, if the inverse has aleady been calculated, 
## then'cacheSolve' function will get the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)       
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
