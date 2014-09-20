# We create a matrix whose inverse can be cached

# makes the matrix whose inverse can be cached

makeCacheMatrix<- function(x = matrix()) {
  m <- NULL
  
  # initizaization
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # returns
  get <- function() x
  
  #sets m to inverse
  setInvs <- function(invs) m <<- invs
  
  #gets inverse
  getInvs <- function() m
  list(set = set, get = get,
       setInvs = setInvs,
       getInvs = getInvs)
}

# finds the inverse of the matrix, if we did not already find it

cacheSolve <- function(x, ...) {
  m <- x$getInvs()
  
  # if inverse found, do nothing
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  #find inverse
  m <- solve(data, ...)
  x$setInvs(m)
  m
}
