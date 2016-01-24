## This is a pair of functions where one function creates and caches a matrix and its inverse
## and the other function does the actual computation of the inverse (or calls the respective solve() function)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {

    ## initialize the inverse matrix to NULL
	inverseMatrix <- NULL
		
	## set the matrix
	## function parameter newMatrix is assigned to variable matrix which is 
    ## defined outside this setter function, thus the <<- operator is used
	set <- function(newMatrix) {
	    matrix <<- newMatrix
	    inverseMatrix <<- NULL		## assuming the new matrix is different from the existing matrix, the inverse matrix is re-initialized to NULL
	}
	
	## get the matrix
	get <- function() matrix
		
	## set the inverse matrix
	setInverse <- function(newInverseMatrix) inverseMatrix <<- newInverseMatrix
				
	## get the inverse matrix
	getInverse <- function() inverseMatrix
		
	## calling "makeCacheMatrix" returns a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## Function parameter is a custom matrix object created by a previous call to makeCacheMatrix()
cacheSolve <- function(matrix, ...) {

    ## for that matrix return the inverse matrix by calling getInverse from makeCacheMatrix()
	inverse <- matrix$getInverse()

	## if the inverse has been calculated previously return it from the cache
    if (!is.null(inverse)) {
	    message("Getting cached data")
		return(inverse)
	}
		
	## otherwise
	## get the actual matrix from makeCacheMatrix() ...
	data <- matrix$get()

	##  ... calculate the inverse ...
	inverse <- solve(data)
		
	## ... and set the inverse within the special object to retrieve it later thereby eleminating the need to re-calculate it
	matrix$setInverse(inverse)

    ## return a matrix that is the inverse of 'matrix'
    inverse
}
