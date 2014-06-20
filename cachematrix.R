## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will create a special matrix object, and then cacheSolve will
## calculate the inverse of the matrix.
## If the matrix inverse has already been calculated, the function will  
## find it in the cache and return it, and not calculate it again. If it is not
## calculated then the funtion will calculate the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL  ## to store the cached inverse matrix
        
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        ## return the matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The function cacheSolve returns the inverse of a matrix created with
## the makeCacheMatrix function. If the cached inverse is available,
## cacheSolve retrieves it, if not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        # If the inverse is already calculated, the function will return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # When the inverse is not yet calculated, the function calculate it
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the inverse
        x$setinv(inv)
        
        inv
}
