## A pair of functions that cache computation of inverse of a matrix

## creates special CacheMatrix
makeCacheMatrix <- function(M = matrix()) {
    i <- NULL;
    set <- function(newM){
        M <<- newM
        i <<- NULL
    }
    get <- function() M
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list( 
        set = set, 
        get = get, 
        setInverse = setInverse, 
        getInverse = getInverse
    )
}


## Computes and returns cached inverse of a CacheMatrix 
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()  
    i <- solve(data, ...)  
    x$setInverse(i)  
    i
}

## test run
m <- replicate(10, rnorm(10))
ms <- makeCacheMatrix(m)
cacheSolve(ms)
cacheSolve(ms)
m2 <- replicate(10, rnorm(10))
ms$set(m2);
cacheSolve(ms)
cacheSolve(ms)

