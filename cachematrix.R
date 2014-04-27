#makeCacheMatrix:is function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
# inversa almacena la matriz inversa/ inversa save the inverse matrix
  inversa <- NULL
  
  # Set de la matriz/matrix
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  # Get de la matriz/ get the matrix
  get <- function() x
  
  # Set inversa
  setinversa <- function(inverse) inversa <<- inverse
  # Get de inversa
  getinversa <- function() inversa
 
  # Devuelve la matriz con las nuevas functiones definidas/reurn the matrix with the new funtions defined
  list(set = set, get = get, setinversa = setinversa, getinversa= getinversa)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Devuelve la matriz inversa de x/Return a matrix inverse of x
      inversa <- x$getinv()
      #Si la inversa ya ha sido calculada la devuelve con mensage/if the inverse has been calculates returns the mesagge and the matrix
      if (!is.null(inversa)) {
      message("getting cached data")
      return(inversa)
      }
# Si no ha sido calculada, la calcula/Return the inverse if it has not been calculates
  data <- x$get()
  inversa <- solve(data, ...)
  
# Toma la inversa/Get inversa
  x$setinversa(inversa)

# Devuelve la matriz/ Return the matix
  inversa
}
