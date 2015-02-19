## This first function makeCacheMatrix caches 
## a matrix that is an object in R.
## Let's us remark that, to be solve, the matrix should 
## be inversible i.e means there is square matrix and det (matrix) 
## should be different to 0.
## In R, we use solve function as recommended. Also, we assume for
## this assignment that, the matrix supplied is always invertible as it's says
## in the assignment.

## two functions have to be build, makeCacheMatrix and cacheSolve
## makeCacheMatrix is an object that stores a matrix by
## set values of a matrix, get its values and set the inverse
## of a matrix and get the inverse values.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## variable initializing
        setMatrix <- function(y) { ## matrix setting 
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x ## get the matrix set before
        setInverse <- function(solve) m <<- solve ## set the inverse of the matrix using solve function and store it on m.
        getInverse <- function() m ## getting the values of inverse matrix.
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}
## Write a short comment describing this function
## cacheSolve function computes the inverse of a matrix returned by
## makeCacheMatrix above. Assuming that matrix the matrix supplied in 
## makeCacheMatrix is always invertible, so cacheSolve calculated the
## inverse and return it's value.
cacheSolve <- function(x, ...) {
        m <- x$getInverse() 
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        m                       ## Return a matrix that is the inverse of 'x'
}
