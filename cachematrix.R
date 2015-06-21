## Put comments here that give an overall description of what your
## functions do

## This function creates the special "matrix" and cache its inverse
## using the original idea of the makeVector function from the sample

makeCacheMatrix <- function(x = matrix()) {

        ## Set the variable for the inverse of the matrix to NULL
        matrixinverse <- NULL	

        ##Creates the set and get functions for the data matrix
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        get <- function() x

        ##Creates the set and get functions for the inverse of the matrix
        setinverse <- function(inverse) matrixinverse <<- inverse
        getinverse <- function() matrixinverse

	## Return the functions as a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the matrix
## If the inverse is already calculate then retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

	## Get the matrix and the inverse of the matrix stored in cache
        datamatrix <- x$get()
        matrixinverse <- x$getinverse()

	## Return the cache object if already exists and the matrix has not change
        if(!is.null(matrixinverse) && x!= datamatrix) {
                message("getting cached matrix")
                return(matrixinverse)
        }

	## Otherwise calculates de inverse of the matrix and udpate the objects in cache
        matrixinverse <- solve(datamatrix, ...)
        x$setinverse(matrixinverse)

        ## Return a matrix that is the inverse of 'x'

        matrixinverse
}