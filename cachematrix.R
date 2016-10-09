


## Function to find the matrix inverse
# 1. The function set and get value of matrix
# 2. The function set and get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinv_matrix <- function(inverse) matrix_inverse <<- inverse
  getinv_matrix <- function() matrix_inverse
  list(set = set, get = get,
       setinv_matrix = setinv_matrix,
       getinv_matrix = getinv_matrix)
}


## This function returns matrix inverse

cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getinv_matrix()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data)
  x$setinv_matrix(matrix_inverse)
  matrix_inverse
}
