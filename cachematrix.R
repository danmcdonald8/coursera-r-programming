#PART I
#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to

#1.set the value of the matrix; #2.get the value of the matrix;
#3.set the value of the inverse of matrix; #4.get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inv <<- inverse
  getinverse <- function() x_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#PART II
#The following function calculates the inverse of the special "matrix"
#created with the above function. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the data and sets the value of the inverse in the cache via the `setinverse`
#function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  else{
    x_inv <- solve(x$get())
    x$setinverse(x_inv)
    return(x_inv)
  }
}
