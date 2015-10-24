#makeCacheMatrix is the matrix to calculate the inverse and caching the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(x) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
} 

#cacheSolve is to calculate the inverse either from the cache or the new inverse
cacheSolve <- function(x,...)
{
  cache_var <- makeCacheMatrix(x)
  m <- cache_var$getinverse()
  if(!is.null(m))
  {
    message("Getting inverse of matrix from cache")
    return(m)
  }
  data <- cache_var$get()
  m <- solve(data)
  z$setinverse(data)
  m
}
