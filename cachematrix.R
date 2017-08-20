## The below functions calculate the inverse of a matrix and cache the value once it has been calculted. 
## If the cache already has a value in, it returns this value without calcuting inverse again for the same matrix. 

## The makeCacheMatrix function sets and gets the value of the matrix, 
## and then sets and gets the value of the matrix's inverse.


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

## The cacheSolve function calculates the inverse of the matrix created with the initial function and sets the value in the cache. . 
## If the inverse has already been calculated, it gets it from the cache and skips the calculation. 

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
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
        