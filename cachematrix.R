## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function (x, y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        
        setinverse <- function(i) {
                inverse = i
        }
        getinverse <- function() inverse
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
