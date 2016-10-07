makeCacheMatrix <- function(x = matrix()) {
    ## Function which takes in a matrix as input and returns a list with the 
    ## matrix, it's inverser and the set, get, setinv and getinv functions.
    
    # Assign the inverse to be NULL when the function is run first time.
    inv <- NULL
    
    # The set function is used if a new matrix is assigned. 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get() function which returns the input matrix
    get <- function() x
    # Store the inverse of the matrix to the inv variable.
    setinv <- function(inverse) inv <<- inverse   
    # get the inverse matrix using getinv() function
    getinv <- function() inv
    
    # Return this list
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    # Function which takes in the cached matrix as input. It then checks if
    # the cached matrix has an inverse. If not, it will compute it, and store
    # it back to the input matrix.
    
    # Check if x has an inverse matrix
    inv <- x$getinv()
    # If it has, return the inverse.
    if(!is.null(inv)) {
        message("Getting cached matrix inversion.")
        return(inv)
    }
    
    # If not cached before, solve the inverse of the matrix.
    m <- x$get()
    # Solve it here
    inv <- solve(m, ...)
    # Store the inverse to the input matrix
    x$setinv(inv)
    # Return the inverse to console
    inv
}
