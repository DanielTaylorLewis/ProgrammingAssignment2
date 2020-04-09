## Together these two functions are used to invert a matrix and store the inverse of the matrix so that it can be called in future 
## without the need for recalculating

## creates a "special" matrix that can be used in cacheSolve to either calculate the cached matrix inverse of a matrix x
## or if cacheSolve has already been run it will return the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(mat) m <<- mat
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Function will calculate the inverse of the special matrix output from makeCacheMatrix and either calculates the inverse or 
## returns the inverse if it has been previously calculated.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
x<-matrix(c(2,0,0,2),2,2)
y<-makeCacheMatrix(x)
cacheSolve(y)


