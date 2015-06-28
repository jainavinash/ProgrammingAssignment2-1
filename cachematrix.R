
##  function creates the matrix object

makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set<-function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get<-function() 
    x
  
  setinv <-function(inverse) 
    inv<<- inverse
  
  getinv<-function() 
    inv
  
  list(set=set, get=get,setinv=setinv,getinv=getinv)
  
}

## function returns cached data.In case the data was not in cache it calulates the mean and put in cache

cacheSolve <- function(x, ...) 
{
  print(" 1 ")
  ## Return a matrix that is the inverse of 'x'
  inv  <- x$getinv()
  if(!is.null(inv))
  {
    message("getting cached data")
    return (inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  return (inv)
}
