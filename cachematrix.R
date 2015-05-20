##There are two separate functions defined as it was done in the given
##example of caching the mean of a vector.
##Function 1.) is called the makeCacheMatrix which makes the matrix special 
##Function 2.) is called the cacheSolve which checks for the existence of the inverse of the new
##matrix and caches it or calculates it


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL         ## making the matrix null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {   ## conditional statement checking the existence of the inverse of the new matrix 
    message("getting cached data")
    return(m)         ## If value is TRUE then it returns the cached inverse and ends the function
  }
  data <- x$get()
  m <- solve(data, ...)   ## Solving the inverse of the matrix using solve()
  x$setinverse(m)
  m
}
