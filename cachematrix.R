
## This function creates a special "cache matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## define the cache m and give it NULL as the default value
  m <- NULL
  
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x in the parent environment
    m <<- NULL ## re-initialize m in the parent environment to null
  }
  
  get <- function() x ## return the matrix x (simple matrix object)
  
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal to the inverse of the matrix x
  
  getinverse <- function() m ## return the cached inverse of x
  
  return (list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

cacheSolve <- function(x, ...) {
  
  
    # check if the inverse of the matrix is already computed, 
    # if so, get the inverse from the "cache matrix" objet x
    m <- x$getinverse()
    if(!is.null(m)) {
      return(m)
    }
    # if the inverse is not computed, solve the provided matrix 
    # and set the inverse to the "cache matrix" objet x
    m <- solve(x$get(), ...)
    x$setinverse(m)
    return(m)
 
}
