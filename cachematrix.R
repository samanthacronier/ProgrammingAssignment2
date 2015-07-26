## Programming assignment 2
## These two functions are used together to compute and store the 
## inverse of an invertible matrix. Here is an example of how to 
# use them: 
#   testmatrix = diag(2,10,10)
# 
#   testCache = makeCacheMatrix(testmatrix)
# 
#   cacheSolve(testCache)
# 
#   # The second time you run this line, it will pull the cache
# 
#   cacheSolve(testCache)
    



##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


##  This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

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
        ## Return a matrix that is the inverse of 'x'
}



