# The first function can be used to create a special "Matrix" like object
# which can hold the value and inverse of a matrix
# the second function is used to get the inverse of a "Matrix" made
# with the first function. The inverse is taken from the cache if available,
# thus possibly reducing computation time


# The function makes a special "matrix", and returns a list of 4 functions
# To set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
   
  setmatrix <- function(y)          
  {
    x <<- y  
    
    #inv = NULL so that the new inv can be calcu
    inv <<- NULL                   
  }
 
  getmatrix <- function() x        
  
  setinv <- function(i) inv <<- i
  
  getinv <- function() inv
  
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)

}



# Solves and returns the inverse of a "Matrix" created using the above function

cacheSolve <- function(x, ...) {
  
  y <- x$getmatrix()
  
  #check whether inverse is not there in cache
  if(is.null(x$getinv()))
  {
    
    #if not in cache, calculate the inverse and set it
    x$setinv(solve(y))
    
  }
  
  #if inverse is available in cache, simply return it
  else
  {
    print("Getting inverse from cache")
  }
  x$getinv()
  
}
