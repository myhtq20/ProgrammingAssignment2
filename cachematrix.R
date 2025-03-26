# Put comments here that give an overall description of what your
# functions do

# Write a short comment describing this function
# makeCacheMatrix:
# This function creates a special "matrix" object that can store its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Will hold the cached inverse
  inv <- NULL
  
  set <- function (y){
    x <<- y
    ## Reset the cached inverse when the matrix changes/is set
    inv <<- NULL
  }
  
  # Function to return the matrix
  get <- function ()x
  set_Inverse <- function(inverse) inv <<- inverse
  get_Inverse <- function ()inv
  ## Return a list of methods for setting/getting 
  ## the matrix and its inverse
  
  list(set = set, get = get,
       set_Inverse = set_Inverse,
       get_Inverse = get_Inverse)
}


# Write a short comment describing this function
# cacheSolve:
# This functions computes (or retrieves from cache) the inverse of the special matrix created (returned) by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the function retrieves the inverse from the cache.
# Otherwise, compute the inverse, cache it, then return it

cacheSolve <- function(x, ...) {

  inv <- x$get_Inverse()
  # If the inverse is already cached, just return it
  
  if(!is.null(inv)){
    # Notify that cached data is used
    message("getting cached data")
    return(inv)
  }
  
  # Get the matrix
  mat <- x$get()
  # Compute the inverse
  inv <- solve(mat, ...)
  # Cache the computed inverse
  x$setInverse(inv)
  
  inv
}
