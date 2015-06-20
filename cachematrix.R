## This function stores a list of fucntions: set, get
## setsolve and getsolve. 

## This is the parent function.


makeCacheMatrix <- function(x = matrix()) {
    
    ## Function set : updates (renews) the matrix stored
    ## Function get : returns the matrix x stored
    ## Function setsolve stores the m value
    ## Function getsolve returns the stored m value
    
    ## set: input y is assigned to x BUT in the parent function makeMatrix. m is reset to NULL
    ## get :returns input x
    
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'
## cachesolve "caches" the inverse matrix

cacheSolve <- function(x, ...) {

        ## getsolve sets the inverse to m
        ## if condition verifies is m is NOT NULL, then cached the inv and returns it
        ## otherwise inversse input from poarent is gotten and inv calculated and 
        ## returned as m output
    
        m <- x$getsolve()
        if(!is.null(m)) {
            message("Getting cached data ... ")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
