## The following is a pair of functions that cache and compute the 
## inverse of a matrix.


## MakeCacheMatrix Function

# This Function takes in a matrix and returns an object that is used 
# to catche its inverse. it basically  sets the value of the matrix, 
# gets the value of the matrix, sets the value of the inverse and gets 
# the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  Inv<- NULL
  set <- function(y){
    x <<- y
    inv  <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  return(list(set =set, get = get, 
              setInverse = setInverse, getInverse = getInverse))

}


## CacheSolve Function

# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix function. If the inverse has already been calculated 
#(and the matrix has not changed),the cachesolve retrieves the 
# inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data,...)
  x$setInverse(inv)
  return(inv)
}

