
## Special Matrix Object that can be used to cache matrix inv


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)
    {   x <<- y
    inv <<- NULL        
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks if the matrix inverse is cached or not
## if its not then inv is calculated.
## This function returns inverse of matrix


cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    ## Return a matrix that is the inverse of 'x'
    if(!is.null(inv))
    {
        message("Found Cached Data -  Retrieving")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
