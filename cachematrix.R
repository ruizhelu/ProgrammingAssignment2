## This function is caching the inverse of a Matrix.
## These functions define the setter and getter for a matrix and its inverse i.
## Using the method of lexical scoping to retrieve the values of the matrix
## and its inverse i.
## List defines both the setter and getter functions for us to call later.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cachesolve() is to populate and retrieve the inverse of a matrix above.
## using the getinverse to get the inverse of the input matrix.
## make sure with the if statement to check whether if it already exists.
## if not, we would calculate the inverse matrix by function solve()
## finally, retrieve the inverse of the matrix x by setinverse()

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
