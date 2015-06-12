## cacheSolve(makeCacheMatrix(x)) returns computed or cached 
## inverse of matrix x

## Build a special list for passing a matrix to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  iMatrix <- NULL
  set <- function(y) {
    x <<- y
    iMatrix <- NULL
  }
  get <- function() x
  setiMatrix <- function(solve) iMatrix <<- solve
  getiMatrix <- function() iMatrix
  list(set = set, get = get,
       setiMatrix = setiMatrix,
       getiMatrix = getiMatrix)
}

## Return the inverse of matrix x via computation or cache look up.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getiMatrix()
  if(!is.null(iMatrix)) {
    message("getting cached data")
    return(iMatrix)
  }
  data <- x$get()
  iMatrix <- solve(data)
  x$setiMatrix(iMatrix)
  iMatrix
}