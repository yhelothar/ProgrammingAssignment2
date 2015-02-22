## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #initializing variable
  set <- function(y) { #function to store matrix into cache
    x <<- y #storing matrix into cache
    m <<- NULL #empty placeholder for m in cache
  }
  get <- function() x #function to return the value of x
  setsolve <- function(solve) m <<- solve #function to set value of inverse matrix
  getsolve <- function() m #function to return value of inverse matrix
  list(set = set, get = get, #compiling functions into a list to be returned
       setsolve = setsolve,
       getsolve = getsolve)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) { #checks if value of mean already exists in cache
    message("getting cached data") 
    return(m) #returns cached value
  }
  data <- x$get() #elseif no value for m exists, pull the matrix stored in x
  m <- solve(data, ...) #find inverse of matrix
  x$setsolve(m) #store value inverse matrix into cache
  m #print value of inverse matrix
}
