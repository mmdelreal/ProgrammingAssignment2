###Moises M. del Real
## Put comments here that give an overall description of what your functions do

## "The following function makes a special "matrix object" that caches its own inverse

makeCacheMatrix <- function(x=matrix()) {

  matobj<-x
  invers<<-solve(matobj)
}


#This function checks the cache for a NULL value. If the cache is populated the function pulls
#the previously calculated 'inverse' value.  If not, the function re-calculates the inverse of
#the matrix 'x'

cacheSolve <- function(x, ...) {
        
  invers<-invers
  
  ## Checks to see if 'invers' value is NULL
  ## If 'invers' is not NULL, then return the original inverse of 'x'
  
  if(!is.null(invers)) {
    
    message("getting cached data")
    round(invers,4)
  
  } else {
  
  ## If 'invers' is NULL, then return a matrix that is the inverse of 'x'
  matobj<-x
  invers2<-solve(matobj)
  
  ##final product :)
  round(invers2,4)
  
  }
}




#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

#Write the following functions:
  
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

#Computing the inverse of a square matrix can be done with the solve function in R. For example, 
#if X is a square invertible matrix, then solve(X) returns its inverse.For this assignment, 
#assume that the matrix supplied is always invertible.
