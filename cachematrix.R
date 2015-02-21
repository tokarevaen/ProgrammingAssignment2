## Set matrix, find inverse matrix, cache inverse matrix. Return cached inverse matrix if already exist;

# makeCacheMatrix: return a list of functions to: 
# 1. Set the value of the matrix; 
# 2. Get the value of the matrix;
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        invr = NULL
        set <- function(y) {x <<- y
                            invr <<- NULL}
        get <- function() x
        setinverse <- function(solve) invr <<- solve
        getinverse <- function() invr
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already exists, cached inverse is returned.

cacheSolve <- function(x, ...) {
        
        invr <- x$getinverse()
        if(!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        
        data <- x$get()
        invr <- solve(data, ...)
        x$setinverse(invr)
        invr
}

