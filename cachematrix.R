
##  function creates the matrix object

makeCacheMatrix <- function(x = matrix())
{
  m<-NULL
  set<-function(y)
  {
    x <<- y
    m <<- NULL
  }
  get<-function() 
    x
  
  setmatrix<-function(solve) 
    m<<- solve
  
  getmatrix<-function() 
    m
  
  list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
  
}

## function returns cached data.In case the data was not in cache it calulates the mean and put in cache

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m))
  {
    message("getting cached data")
    return (m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  return (m)
}
