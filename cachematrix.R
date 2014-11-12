## Put comments here that give an overall description of what your
## functions do

## This function store the matrix given in its argument and create 4 functions
## inside, that can be used by the matrix with the $ operator

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    # Function set: Store the given new matrix and initialize (NULL) the inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Function get: Return the stored original matrix
    get <- function() x
    
    # Function setinverse: Put the given matrix into the inverse i
    setinverse <- function(inverse) i <<- inverse

    # Function getinverse: Return the stored inverse matrix
    getinverse <- function() i
    
    # Return a list with the 4 functions with the same names
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function retrieve the inverse of the given matrix if exists.
## In other case, it calculate its inverse and cache it to be reused

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of the given matrix
    i <- x$getinverse()
    
    # If the inverse exists, return the cached inverse and ends the function
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # If the inverse doen't exist yet:
    
    # Get the original matrix
    data <- x$get()
    
    # Calculate the inverse of the matrix
    i <- solve(data)
    
    # Set the cached inverse
    x$setinverse(i)
    
    # Return the calculated inverse
    i    
}
