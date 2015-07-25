## it has function cacheSolve to find inverse of matrix. And function makeCacheMatrix to to cache inverse of matrix


## Function to cache an inverse of matrix. It has function set matrix and get matrix method
## It has set method to set and get inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL

  ## if new matrix is set then inverse is set to null 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## retrieve matrix
  get <- function() x
  
  ##set inverse of matrix
  setinverse <- function(inv) i <<- inv
  
  ##retrieve inverse of matrix
  getinverse <- function() i
  
  ##set list with all these functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Function to find an inverse of matrix
cacheSolve <- function(x, ...) {
  
  ## Find an inverse of matrix from cache
  i <- x$getinverse()
  
  ##if cache is not null retrun inverse of matrix from cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Function to retrieve matrix
  data <- x$get()
  
  ## in built function to find inverse of matrix
  i <-solve(data,...)
  
  ## function to set(cache) inverse of matrix
  x$setinverse(i)
  
  ## return inverse of matrix
  i
  
}  
