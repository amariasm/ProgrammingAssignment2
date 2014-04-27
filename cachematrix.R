## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix:is function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
# inversa almacena la matriz inversa
  inversa <- NULL
  
  # Set de la matriz
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  # Get de la matriz
  get <- function() x
  
  # Set de inversa
  setinversa <- function(inverse) inversa <<- inverse
  # Getter for the inverse
  getinversa <- function() inversa
 
  # Devuelve la matriz con las nuevas functiones definidas
  list(set = set, get = get, setinversa = setinversa, getinversa= getinversa)
}


## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inversa <- x$getinv()
      # Si la inversa ya ha sido calculada la devuelve
  if (!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
   
  # Si no ha sido calculada, la calcula
  data <- x$get()
  inversa <- solve(data, ...)
  
  
  # Toma la inversa
  x$setinversa(inversa)
  
  
  # Devuelve la matriz
  inversa
}
