## This function creates a cache of the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {  ## function call to cache the matrix x
        inversion <- NULL                    ## asigns NULL to inversion
        set <- function(y) {
                x <<- y
                inversion <<- NULL  ## function set inversion as NULL in global environment
        }
        get <- function() x ## function with no arguement, return x in the scope
        setTheInversion <- function(inverse) inversion <<- solve(x) ##function calculate inverse of x, update insersion globally
        getTheInversion <- function() inversion ##function return inversion
        list(set = set, get = get, setTheInversion = setTheInversion, getTheInversion = getTheInversion)## returns a list of functions
}

## This function returns the inverse of the matrix from the function above

cacheSolve <- function(x, ...) {
        inversion <- x$getTheInversion()    ## assigns result from getTheInversion to inversion
        if(!is.null(inversion)){            ## checks to see if inversion is set to NULL or not
                return(inversion)           ## returns result of inversion if it is not NULL
        }
        matrixdata <- x$get()               ## matrixdata is the matrix arguement used in makeCacheMatrix
        inversion <- solve(matrixdata, ...) ## calculate inverse of matrixdata, will be passed to cacheSolve as its arguement
        x$setTheInversion(inversion)        ## assign inversion globally
        inversion                           ## returns inversion                        
}