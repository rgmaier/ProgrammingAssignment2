## As part of the course requirements, we would like to calculate the inverse of a matrix and then caching it.
## The requirements state that the matrix is always invertable. 

## This function defines and sets the object based on the user input of a matrix
## For example, matrx <- cbind(c(2,1),c(5,3)) is the matrix used
## Calling makeCacheMatrix(matrx) sets the necessary variables and defines the functions of the object
## Please note, that no inverse is calculated at this point - the following function must be called at least once.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) m <<- solve
  
  getinv <- function() m
  
  list(set = set, get=get, setinv = setinv, getinv=getinv)
}


## This function initially gets the matrix as defined by the above function.
## Then, we check whether the inverse has already been calculated. If yes, we do not calculated it again but simply return the value
## If not, we calculate the inverse, save it in the object and return the inverted matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  
  data <- x$get()
  
  m <- solve(data,...)
  x$setinv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
