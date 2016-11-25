## There are two functions in the cachematrix.R file"
##
##   1.  makeCacheMatrix()
##   2.  cacheSolve()
## 

## Function     : makeCacheMatrix
## Description  : Returns a list of 4 functions/methods that get and sets the
##                the matrix x and its inverse in environment variable. 
##
##                In an object oriented world, makeCacheMatrix is a subclass 
##                of matrix.
## Arguments    : x	- the matrix to be reversed.

makeCacheMatrix <- function(x = matrix()) {
	
    invMatrix <- NULL		## Initialize the inversed matrix
	
    ## Set x in the parent environment with the new matrix value.  
    ## With x being set to a new value, invMatrix is no longer 
    ## valid and should be set to NULL
    set <- function(matrixValue = matrix()) {
        x <<- matrixValue
        invMatrix <<- NULL
    }
	
    ## This is the get function to return x.
    ## Function can also be written as 
    ##    get <- function() {
    ##        x
    ##    }
    get <- function() x   
	
    ## Set the value of cached inversed matrix in the parent environment.
    setInverse <- function(invMatrixValue) invMatrix <<- invMatrixValue
	
    ## Get the value of the cached inversed matrix
    getInverse <- function() invMatrix
	
    ## Return a list of functions and set the names.
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function     : cacheSolve
## Description  : Check if the inverse of matrix x exists.  If not, create and
##                cache the matrix 
## Arguments    : x - the matrix to be reversed and should be create with the 
##                    function makeCacheMatrix

cacheSolve <- function(x = makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2)), ...) {
    ## Return a matrix that is the inverse of 'x'
	
    ## Get the inverse of matrix x
    inversedMatrix <- x$getInverse()
    
    ## Check if x matrix has been inversed and make sure it's a valid matrix
    if (!is.null(inversedMatrix) && is.matrix(inversedMatrix)) {
        return(inversedMatrix)
    }
    
    ## Get the inverse of the matrix
    tempMatrix = x$get()
    inversedMatrix <- solve(tempMatrix, ...)
    
    ## Cache the result
    x$setInverse(inversedMatrix)
    
    ## return the result
    return(inversedMatrix)
}
