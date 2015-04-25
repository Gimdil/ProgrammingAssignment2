###=============================================================================
### Function to create a special matrix and store the inverse matrix associated
### Function to calculate the inverse matrix or return the cached inverse matrix
### Encode = UTF-8
### R version 3.1.2 (2014-10-31) -- "Pumpkin Helmet"
### B.Go. (25/04/2015)  jj/mm/aaaa 
###=============================================================================


## Running example
##=============================================================================

# Create matrix special function
# mat <- makeCacheMatrix()
#
# Set matrix 
# Note that calling this function will erase all cached inverse matrix
# mat$setMatrix(matrix(rnorm(100,1,1),10,10))
#
# Get matrix
# mat$getMatrix()
#
# Calculate and return inverse matrix
# cacheSolve(mat)
#
# Get inverse matrix
# mat$getInverseMatrix()




## Create a special matrix.
##=============================================================================

# Must be a square matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse.matrix <- NULL
    setMatrix <- function(y){ # Set square matrix function
        if(dim(y)[1] != dim(y)[2]){ # Check if matrix is a square matrix. If not, stop an return an error
            stop('Input matrix must be square matrix')
        }
        x <<- y # Set matrix
        inverse.matrix <<- NULL # Reset inverse.matrix to NULL if setMatrix() is called
    }
    getMatrix <- function() { # Return the matrix
        return(x)
    }
    setInverseMatrix <- function(inverse){ # Cache the inverse of the set matrix. Inverse is reset to NULL if setMatrix() is called
        inverse.matrix <<- inverse
    }
    getInverseMatrix <- function(){ # Return the inverse matrix
        return(inverse.matrix)
    }
    return(list(setMatrix = setMatrix, getMatrix = getMatrix, 
                setInverseMatrix = setInverseMatrix,
                getInverseMatrix = getInverseMatrix))
}


## CacheSolve calculate the inverse matrix or return the cached inverse matrix if already exist
##=============================================================================

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse.matrix <- x$getInverseMatrix() # Get the inverse matrix from the special matrix.
    set.matrix <- x$getMatrix() # Get the matrix from the special matrix. 
    
    if(!is.null(inverse.matrix)){ # Check if inverse matrix has already been calculated for the current matrix. 
        message("Getting cached inverse matrix") # If so, return a message and the cached inverse matrix
        return(inverse.matrix) 
    }
    inverse.matrix <- solve(set.matrix) # Else, calculate the inverse matrix
    x$setInverseMatrix(inverse.matrix) # Cache the inverse matrix in the special matrix.
    return(inverse.matrix) # Return the inverse matrix
}
