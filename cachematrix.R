makeCacheMatrix <- function(x = matrix()){   #creating a function with argument as default matrix
  m <- NULL #initializing object m which stores the inverse of the matrix
  set <- function(y){ #setting the function set means setting the value of matrix to variable x
    x <<- y
    m <<- NULL
  }
  get <- function(){ #returning the value of x (matrix)
    x
  }
  setInverse <- function(inverse){ #setting the inverse of the matrix which will be calculated using solve(x)
    m <<- inverse # value of inverse will be stored in m object
  }
  getInverse <- function(){ #this function will return the value of inverse calculated
    m
  }
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) #this is required because we are using
                                    #multiple functions inside one function, if we dont use it, it will take only
                                    # the last function
}


cacheSolve <- function(x)
{
  m <- x$getInverse() #getting the inverse of the matrix from cache
  if(!is.null(m)){ #checking if the cache data is not null
    message("getting cached data")
    return(m) #returns the value of cached data
  }
  else{
    data <- x$get() #passing the value of matrix to data
    m <- solve(data) #calculating the inverse of the matrix
    x$setInverse(m) #setting the value of inverse
    m #returning the value of inverse
  }
}