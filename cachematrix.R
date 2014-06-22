
## makeCacheMatrix(matrix.object) creates and stores a matrix when first called and stores
## the inverse of the matrix when the cacheSolve(makeCacheMatrix.object) function is called on it.

## variableName$getMatrix() - returns the stored matrix
## variableName$setMatrix() - sets the stored matrix with new values and/or dimensions
## variableName$getInverse() - returns the matrix's inverse or NULL
## variableName$setInverse() - sets the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solvedMatrix) inverse <<- solvedMatrix
        getInverse <- function() inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve(makeCacheMatrix.object) checks for and returns a matrix
## that is the inverse of the matrix stored in a makeCacheMatrix.object

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        toSolveMatrix <- x$getMatrix()
        inverse <- solve(toSolveMatrix, ...)
        x$setInverse(inverse)
        inverse
}
