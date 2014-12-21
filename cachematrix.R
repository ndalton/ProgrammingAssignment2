## Nicolaas Dalton R Programming Assignment 2

## These functions allow us to reduce the effect of a potentially time
## consuming operation, finding the inverse of a matrix by caching it. 

## Modeling heavily off the example function for the mean, it should
## Be possible to write a very similar function for the inverse of the matrix
## It is assumed that the matrices presented will be square and solvable. 

makeCacheMatrix <- function(x = matrix()) { 
  
  ## create a matrix and set the value of the matrix and it's inverse
  ## The matrix will be part of a list that also contains the previous matrix
  ## as well as the previous values of the inverse so that we may save
  ## time in calculating the inverse
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## Set the solved value for the given matrix
  setsolve <- function(solve) m <<- solve
  
  ## looks to see what is cached for the function solve
  
  getsolve <- function() m
  
  #Update the list with the calculated values
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## The function cachesolve calculates the inverse of the special matrix created with 
## the above function. Tt first checks to see if the inverse has already been calculated. 
## If it was, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## set the value of m to the value of the inverse if it was already calculated
  ## if not already calculated, the lower part of the function will execute
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Set data to the new value of the matrix
  data <- x$get()
  # Calculate the inverse
  m <- solve(data, ...)
  # Store the new inverse in the list
  x$setsolve(m)
  # Return the value of the inverse
  m
  
}
