library(MASS)  ## This package contains a directly method for calculate matrix inverse
makeCacheMatrix <- function( x = matrix() ) {
        
        
        m <- NULL
        
        ## set the matrix
        set <- function( matrix ) {
                x <<- matrix
                m <<- NULL
        }
        
        ## get the matrix and return the matrix
        get <- function() x 
                        
                
        ## set the inverse of the matrix
        setinvmatrix <- function(invmatrix) m <<- invmatrix
                
        
        ## get the inverse of the matrix
        getinvmatrix <- function() m
                        
        
        list(set = set, get = get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


##If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" presents the inverse from the cache memory.

cacheSolve <- function(x, ...) {
        
        ## Return the inverse of x
        m <- x$getinvmatrix()
        
        ## If the matrix is diferent show this message and the inverse matrix in cache
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix 
        y <- x$get()
        
        ## Calculate the inverse directly using ginv() in MASS package
        m<- ginv(y)
        
        ## Set the inverse
        x$setinvmatrix(m)
        
        ## Return the matrix
        m
}
