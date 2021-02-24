
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
        ## inverse initialization
        inv <- NULL
    
        ## set the matrix
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
    
        ## get the matrix
        get <- function() x
    
        ## set the inverse matrix
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
    
        ## methods list
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        
        ## inverse of x
        inv <- x$getinverse()
        
        ## conditional to return the inverse if its already cached
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        
        ## gets the matrix from object
        data <- x$get()
        
        ## calculates the inverse
        inv <- solve(data)
        
        ## sets the inverse object
        x$setinverse(inv)
        
        ## returns the matrix
        inv
}
