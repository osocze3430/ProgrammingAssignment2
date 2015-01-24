## Put comments here that give an overall description of what your
## functions do

## make an object of a matrix which will additionally store its cached inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function (x, y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function () x
        
        setinverse <- function(i) {
                inverse <<- i
        }
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function allows us to retrieve an inverse stored in the matrix object - or compute it if no inverse
## was yet computed for this object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if( !is.null(i) ) {
                message("getting precomputed matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        message("inverse was computed&cached")
        i
}
