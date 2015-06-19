### Assignment: Caching the Inverse of a Matrix
##
## The first function, makeCacheMatrix, creates a special "matrix" object 
## that can cache its inverse.
## The second function cacheSolve, computes the inverse of the special 
## "matrix" returned by `makeCacheMatrix`. If the inverse has already 
## been calculated (and the matrix has not changed), then `cacheSolve` 
## retrieves the inverse from the cache.
##
### makeCacheMatrix -
## The first function, makeCacheMatrix, performs the following tasks:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invmx <- NULL
        set <- function(y) {
                x <<- y
                invmx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invmx <<- inverse
        getinverse <- function() invmx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

### cacheSolve - 
## The second function, cacheSolve, calculates the inverse of the special 
## "matrix" created with the first function, makeCacheMatrix. However, it 
## first checks to see if the inverse has already been calculated. If so, 
## it 'get's the inverse from the cache (and displays the message "Getting
## cached data...") and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the value of the mean in the cache via
## the `setinverse` function.

cacheSolve <- function(x, ...) {
        invmx <- x$getinverse()
        if(!is.null(invmx)) {
                message("Getting cached data...")
                return(invmx)
        }
        data <- x$get()
        invmx <- solve(data, ...)
        x$setinverse(invmx)
        invmx
}
