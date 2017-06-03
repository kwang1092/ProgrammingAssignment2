## makeCacheMatrix and cacheSolve make it much easier to compute the inverses of matrices by caching the inverses

## makeCacheMatrix creates a "matrix" object that is able to cache the inverse of a given matrix (assuming it's invertible)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<-y
                m <<-NULL
        }
        get <- function()x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix object created using makeCacheMatrix. If the inverse for that particular matrix
## has already been determined, the inverse is simply retrieved from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
