# Program Assignment 2
# Thank you to discussion forum posters; your insight was very helpful in completing this one
# Particularly James A. Stephenson, Richard Ambler & Michael Hartley.  Thanks!

# This function creates a list of functions to set matrix, get cached matrix
# Set the inverse of the matrix and get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# This function first checks to see if there is a cached inverse matrix and
# if so it returns a message saying it is getting the cached data and then returns the data
# if there is not a cached inverse matrix (i.e. cache is null), it gets the matrix data and
# then it solves for the inverse of the matrix and caches it

cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


a <-makeCacheMatrix()       # assigns function to a
x <- matrix(c(4,2,7,6),2,2) # assigns a matrix to x (also tested a 3x3 and it worked)
a$set(x)                    # passes x to the set function
cachesolve(a)               # passes a to cachesolve; cache is null, so calculates inverse at sets it
cachesolve(a)               # passes a to cachesolve; inverse is cached, returns cached inverse matrix
