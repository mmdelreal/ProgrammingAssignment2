###Moises M. del Real
#The following 2 functions create a "matrix object", invert it, 
#cache the inverted matrix, and then computes the inverse of the matrix in
#question, unless it has already been calculated and is in the cache (via
#the "matrix object")


## "The following function makes a special "matrix object" that caches its own inverse

makeCacheMatrix <- function(x=matrix()) {
  
  mobj <- NULL
  
  set <- function(y) {
    x <<- y
    mobj <<- NULL
  }
  get <- function() x
  setinvers <- function(solve) mobj <<- solve
  getinvers <- function() mobj
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}


#This function checks the cache for a NULL value. If the cache is populated the function pulls
#the previously calculated 'inverse' value.  If not, the function re-calculates the inverse of
#the matrix 'x'

cacheSolve <- function(x, ...) {
  
  mobj<-x$getinvers()
  
  ## Checks to see if 'mobj' value is NULL
  
  if(!is.null(mobj)) {
    
    # If 'mobj' is not NULL, then return the original, cached inverse of 'x'  
    
    message("Obtaining cached data... ^_^")
    mobj
    
  }
  
  
  ## If 'mobj' is NULL, then return a matrix that is the inverse of 'x'
  
  inversdata <- x$get()
  mobj <- solve(inversdata, ...)
  x$setinvers(mobj)
  
  ##final product :)
  mobj
}
