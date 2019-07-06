## Functions that uses solve function to get an inverse of matrix
## First function create the matrix with two encapsulation functions (get,set)
## Second function realizes the calculate.

##Function for crete the object matrix con 4 encapsulation functions.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function that can solve the inverse of a matrix and storage its result in 
## a variable in other enviroment, so we can cached id.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}