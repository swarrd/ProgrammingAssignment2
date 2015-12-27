## The function makeCacheMatrix either gets or initated the values of the matrix, and
## either gets or sets the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve funtion first checks to see if the inverse of the matrix has already 
## been calculated. If this has already been done, it looks up the inverse of the matrix
## and returns these values. If the inverse has not already been computed, it performs 
## this calculation. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

