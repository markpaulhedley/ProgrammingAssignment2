## this file contains two functions to fulfill the requirements 
## of R programing- assignment 2. The two functions are makeCacheMatrix
## whichcreates a matrix that can cache its own inverse and
## cachesolve which will either look up an already "solved"
## matrix or find the inverse depending on weather this action 
## has been completed already.

## This is the first function which creates a matrix object and
## can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                            #this sets the matrix's vaue
    x <<- y
    m <<- NULL
  }
  get <- function()                               #this retrives the value of the matrix
  setinverse <- function(inv) m <<- inv           #this sets the inverse value
  getinverse <- function() m                      #this gets the inverse value
  list(set = set, get = get,
       setinverse = setinverse,                   #list of thefour functions that are defined above
       getinverse = getinverse)
  
}


## This function looks for the pre-calculated inverse of the special matrix in the cache. Ift he inverse has not been calculated
## and the cashe is empty then it calculates the inverse.

cacheSolve <- function(x, ...) {        ## Return a matrix that is the inverse of 'x'
        
  m <- x$getinverse()                   ##looks in the cache
  if(!is.null(m)) {                     ##asks if the cache has something in it
    message("getting cached data")      
    return(m)                           ##and returns the stored data if its there
  }
  data <- x$get()
  m <- solve(data, ...)                 ##otherwise the inverse is computed stored in the cache and the result returned
  x$setinverse(m)
  m
  
}
