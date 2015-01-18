## Programming Assignment 2 : The two functions below create a special "matrix" and cache the inverse of the matrix so that
## the time taken for computation is minimized.  These are especially useful in situations that require calculating an
## inverse every time in a loop.  Retrieving from Cache saves time.

## The makeCacheMatrix function creates a special "matrix" which is actually a list containing 4 functions that :
##  (1) set the matrix
##  (2) get the matrix
##  (3) set the inverse of the matrix
##  (4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {  ## Creates a special matrix (Programming Assignment 2)
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix created by the above function.  
## To test how these functions work together, try the following example:
## > p <- matrix(1:4, 2,2)
## > mat <- makeCacheMatrix(p)
## > cacheSolve(mat)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(mat)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Notice on the second time cacheSolve(mat) returns the inverse from cache as opposed to solving for the inverse

cacheSolve <- function(x, ...) {                   # Inverts a matrix or returns from Cache if already available
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

