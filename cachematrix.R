#save a matrix in the memory
makeCacheMatrix <- function(x=matrix()) {
  inv_x <- NULL
  
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv_x <<- inverse
  getinverse <- function() inv_x
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#check if matrix inverse exist, if not, compute for inverse
cacheSolve <- function(x, ...) {
  inv_x <- x$getinverse()
  
  if ( !is.null(inv_x) ) {
    message("getting cached inverse matrix")
    return(inv_x)
    
  } else {
    my_matrix = x$get()
    inv_x <- solve(my_matrix)
    x$setinverse(inv_x)
    return(inv_x)
  }
}