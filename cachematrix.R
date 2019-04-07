## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix below is a matrix that gets and sets the value of the
## matrix and its inverse. It can also cache its inverse.

# defines makeCaheMatrix function and specify that x is a matrix
makeCacheMatrix <- function(x = matrix()) { 
  # set inv as NULL; it will eventually hold the value of matrix inverse
  inv <- NULL     

  # defines set function
  set <- function(y) {                    
    # will set the value of matrix
    x <<- y   
    # new matrix will be reset inv to NULL
    inv <<- NULL                        
  }
  #will get the value of the matrix
  get <- function() x
  #sets value of inv
  setinverse <- function(inverse) inv <<- inverse 
  #gets the value of inv
  getinverse <- function() inv
  #creates the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}

## cacheSolve below makes use of the matrix made by the makeCacheMatrix 
## above. It checks whether there are values in the matrix made in 
## makeCacheMatrix. If there are values, it will get the inverse
## from the cache and tell the user it is getting it from the cache.
## 

cacheSolve <- function(x, ...) {
  # gets the inverse of x
  inv <- x$getinverse()
  #sets condition if matrix is not null
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  # if the matrix is null
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
