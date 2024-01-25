#This function creates a special 'matrix' which is a list that
# sets the value of the matrix, gets the value of that
# matrix, then sets the value of the inverse of the matrix,
# and gets the value of the inverse of the matrix. This
# function can cache the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set_matrix <- function(y) {
    m <<- NULL
    x <<- y
  }
  get_matrix <- function() x
  
  set_inv <- function(inverse) m <<- inverse
  get_inv <- function () m
  
  list(set_matrix = set_matrix, 
       get_matrix = get_matrix,
       set_inv = set_inv,
       get_inv = get_inv)
}



## This function checks to see if the inverse of the matrix
# has already been calculated. If so, it pulls the inverse 
# from the cache and doesn't have to compute the inverse using
# the function above (makeCacheMatrix)

cacheSolve <- function(x, ...) {
  m <- x$get_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get_matrix()
  m <- solve(data, ...)
  x$set_inv(m)
  print(m)
}

