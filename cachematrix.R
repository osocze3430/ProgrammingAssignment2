## Put comments here that give an overall description of what your
## functions do

## make an object of a matrix which will additionally store its cached inverse
#exemplary usage:
#> mat <- matrix(1:4,2,2)
#> m1 = makeCacheMatrix(mat)
#> cacheSolve(m1)
#> cacheSolve(m1)

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        #set matrix x to this object, set inverse to NULL for now
        set <- function (x, y) {
                x <<- y
                inverse <<- NULL
        }
        #return matrix
        get <- function () x
        
        #set inverse of matrix x
        setinverse <- function(i) {
                inverse <<- i
        }
        
        #return inverse
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
        #if the inverse was already computed for the matrix - retrieve it:
        if( !is.null(i) ) {
                message("getting precomputed matrix")
                return(i)
        }
        #if inverse is not yet computed for the matrix:
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        message("inverse was computed&cached")
        i
}
