## Takes a matrix as an argument and returns a list of functions 
## that performs the following operations:
##	a. set values of matrix
##	b. returns the matrix
##	c. computes the inverse of the matrix
##	d. returns the inverse of the matrix	

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setInv <- function(inverse) m <<- inverse
    getInv <- function() m
    
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Takes the special matrix created by makeCacheMatrix as a argument.
## It returns the inverse of the matrix if already exists; otherwise,
## it computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
     m <- x$getInv()    
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m
}
