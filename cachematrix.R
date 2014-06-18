## Caching the Inverse of a Matrix

## The first function, `makeVector` creates a special "list" containing 4 functions
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

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


## calculates the inverse of the special "list" created by makeCachMatrix


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

## To calculattes inverse matrix, please create a special "list" using makeCacheMatrix.
## a <- makeCacheMatrix(X)
## And then calculate with cacheSolve
## cacheSolve(a)
