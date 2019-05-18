#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## It returns a matrix that is the inverse of x
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Cached data:")
                return(inv)
        }
        
        get_x <- x$get()
        inv <- solve(get_x, ...)
        x$setInverse(inv)
        inv
}
  
© 2019 GitHub, Inc.
