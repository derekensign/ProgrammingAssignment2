## These functions take a matrix, produce 2 set and 2 get functions, and 
## ultimately output the inverse of the matrix

## This function creates a list of four functions, set() which sets x to equal
## the local variable y, get() which returns x, setinv() which sets the inv 
## matrix variable, and getinv() which returns the inv matrix value

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inversematrix) inv <<- inversematrix
        getinv <- function () inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Running this function by inputting the output of the makeCacheMatrix
## function, will provide the inverse of the original matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
