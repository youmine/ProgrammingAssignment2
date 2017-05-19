

##-----------------------------------------------------------------------------
##
## The following functions calculate the inverse of a special "matrix" object.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache (i.e. the matrix object) and 
## skips the computation. Otherwise, it calculates the inverse and sets the 
## value in the cache for later use.
##
## Example:
## 		Step 1: Create "matrix" object.
## 		mtxObj <- makeCacheMatrix() 
##			
## 		Step 2: Matrix to be solved is placed in the object.
##      x <- matrix(1:4,nrow=2,ncol=2)
##		mtxObj$setData(x)
##
##		Step 3: Solve matrix stored in the obj --or-- simply return
##		the cached value of the prior solution. (useful in loop)
##      for (i in 1:4) {
##			cacheSolve(mtxObj)
##		}
##
##-----------------------------------------------------------------------------

makeCacheMatrix <- function(m = matrix()) {
## The function creates a special "matrix" object that can cache its inverse.

        minv <- NULL							# Matrix inverse
        setData <- function(y) { 				# Load new matrix data & reset
                m <<- y							
                minv <<- NULL 
        }
        getData <- function() m 				# Return existing matrix data 			
        setInv <- function(ms) minv <<- ms  	# Put inverse solution in cache
        getInv <- function() minv 				# Return value of mc cache

        list(setData = setData, 				# List func available on obj
        	 getData = getData, 				
             setInv = setInv,
             getInv = getInv)
}


cacheSolve <- function(mObj, ...) {
## Check the "matrix" object and calculate the inverse only if needed

     mSolved <- mObj$getInv()					# Check for cached inverse 

     if (!is.null(mSolved)) {					# Found cached inverse
     	print('Returning cached value')
     	return(mSolved)							# Return cached inverse
     }

     # !Found, this is a new cache object that has not been previously solved

     mSolved <- solve(mObj$getData())			# Solve "matrix" in the obj
     mObj$setInv(mSolved)						# Cache inverse value
     print('Returning calculated value')		
     return(mSolved)							# Return calculated inverse
}
