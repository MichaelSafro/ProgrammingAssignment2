## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - creates and sets an object allowing to cache  matrix and calculate and cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve uses object made by makeCache to get inverse when it is possible retrieving it from cache

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

## use of both objects as following:
##cm<-makeCacheMatrix(matrix(c(1,3,4,5),nrow=2,ncol=2))
##cacheSolve(cm)
