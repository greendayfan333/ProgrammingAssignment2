## These functions will invert your matrix and store the result so
## you do not have to keep inverting your matrix

## Run this function with your matrix first.  This function sets
## up the cache.  Set this to a variable to go into the next function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #Sets the matrix
    set <- function(y) {
        x <<- y
        inv <<-NULL
    }
    #Gets the matrix
    get <- function() x
    #Sets the inverse of the matrix
    setinv <- function(solveinv) inv <<- solveinv
    #Gets the inverse of the matrix
    getinv <- function() inv
    #Returns the above functions as a list
    list(set = set,get=get, setinv = setinv, getinv = getinv) 
}


## Use the variable created in the first function as the input in to this one.
## This function finds the inverse of the matrix if it hasn't been calculated
## before, or calls the inverse if it has.

cacheSolve <- function(x, ...) {
    #Gets the inverse of the matrix from the cache
    inv <- x$getinv()
    #If the cache is not Null, returns the cached inverse (with a message 
    #informing the user that this is what it has done)
    if(!is.null(inv)) {
        message("getting cached matrix inversion")
        return(inv)
    }
    #Gets the matrix
    data <- x$get()
    #Finds the inverse
    inv <- solve(data)
    #Saves the inverse in the cache
    x$setinv(inv)
    #Returns the inverse
    inv
}
