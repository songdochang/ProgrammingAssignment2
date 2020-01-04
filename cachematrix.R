
## Returns a list that contains setters and getters
## for the data and the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # cache matrix
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Returns cached inverse matrix from cache if it exists
## otherwise, invert matrix and cache the matrix

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # get the inverse of matrix
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
