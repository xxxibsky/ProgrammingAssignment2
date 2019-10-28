## catch the inverse of a matrix
## method to set tge matrix and get the matrix
## method to set the inverse of the matric and to get 
## the inverse of the matrix
## return a list of the methods

makeCacheMatrix <- function( x = matrix() ){
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse){
    
   inver <<- inverse
  }
  getInverse <- function() {
    inver
  }
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## calculate the inverse of the matrix created above
## it would retrieve the inverse from the cache if
## the inverse had alreadt been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  
  object <- x$get()
  inver <- solve(object, ...)  
  x$setInverse(inver)
  inver
  
}
