# Inverse of a matrix

x <- matrix(c(1,2,3,4), nrow =2)
x
class(x)
solve(x)

#### Caching the inverse of a matrix

# The following function creates a special This function creates a special "matrix" object
# that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) { # x is a numeric vector
  
  m <- NULL # Setting the inverse to NULL
  
  # Function to set the value of the matrix x to y and
  # reset the inverse back to NULL
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  
  # Function to get the value of the matrix
  
  get <- function() x
  
  # Function to set the value of the inverse
  
  setinverse <- function(inverse) m <<- inverse
  
  
  # Function to get the value of the mean
  getinverse <- function() m
  
  # Return a list containing all the 4 functions
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  # If the inverse has already been calculated, get the mean
  # from the cache and skip the computation.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If inverse has not been calculated before,
  # calculate the inverse of the data and sets the value of the inverse
  # in the cache via the setinverse function.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Checking the 2 functions:

mmat <- makeCacheMatrix()
x <- matrix(c(1,2,3,4), nrow =2)
mmat$set(x)
mmat$get()
mmat$getinverse()
cacheSolve(mmat)


