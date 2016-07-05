## Put comments here that give an overall description of what your
## functions do
# 1 set the value of the vector
# 2 get the value of the vector
# 3 set the value of the inverse
# 4 get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initialize the inverse matrix value
  
  # 1 set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # 2 get the value of the vector
  get <- function() x
  
  # 3 set the value of the inverse
  set_inverse <- function(inv_input) inv <<- inv_input
  
  # 4 get the value of the inverse
  get_inverse <- function() inv
  
  # return a list of all the above functions
  list(set = set, get = get,
       setmean = set_inverse,
       getmean = get_inverse)
}




# The following function calculates the inverse of the special "matrix" 
# created with the above function. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the 
# computation. Otherwise, it calculates the inverse of the 
# matrix and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
  # check if the inverse is already cached,
  # if so, we get the inverse from the cache directly
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # else, we first get the matrix
  data <- x$get()
  
  # calculate the inverse
  inv <- solve(data, ...)
  
  # next, cache the inverse of the matrix
  x$set_inverse(inv)
  
  # return the result
  inv
}
