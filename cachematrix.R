## Function makeCacheMatrix stores 4 functions.
## 1) setMatrix() will allow user to enable/define a matrix (should be square)
## 2) getMatrix() will return the matrix created with setMatrix, or return NULL
    ##no matrix set.
## 3) setMatInverse() will allow the user to input a matrix, which should be the 
    ## inverse of getMatrix(), but does not necessarily have to be.
## 4) getMatInverse() will return the matrix created with setMatInverse, or 
    ##return NULL if no matrix set.

makeCacheMatrix <- function(x = matrix()) {
  MatInverse <- NULL 
  
  setMatrix <- function(y) {
    x <<- y
    MatInverse <<- NULL
  }
  
  getMatrix <- function() x
  
  setMatInverse <- function(Inv) MatInverse <<- Inv
  
  getMatInverse <- function() MatInverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatInverse = setMatInverse, getMatInverse = getMatInverse)
}


##function cacheSolve takes a function of matrices, and will either return
  ##the already established Inverse of getMatrix(), or compute the inverse
  ##of getMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    MatInverse <- x$getMatInverse()
    
    if(!is.null(MatInverse)) {
      message("retrieving cached Inverse...")
      return(MatInverse)
    }
    
    data <- x$getMatrix()
    
    MatInverse <- solve(data)
    
    x$setMatInverse(MatInverse)
    
    MatInverse
    
}