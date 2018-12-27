## The following functions make use of R's lexical scoping to cache the results of a time-intensive operation:
    ## calculating the inverse of a matrix.

## The makeCacheMatrix function creates a series of setter and getter functions and returns those functions in a named list.
  ## x and i are created in the makeCacheMatrix parent environment along with the set function (allows user to enter matrix 
  ## for use in subsequent functions), get function (retrieves the matrix for which the inverse will be calculated),
  ## setinverse function (calculates the inverse of x matrix and saves this as i in the parent environment), and getinverse
  ## function (retrieves the inverse matrix i).

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- matrix(y)
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function will retrieve the inverse matrix stored in makeCacheMatrix's i using getinverse().
  ## If the inverse is not null (i.e., i was alread calculated and is available) cacheSolve returns i (the inverse). 
  ## If the inverse is null (i.e., i hasn't been calculated), cacheSolve will apply the get() function from 
  ## makeCacheMatrix to create the data object, determine the matrix inverse using solve(), save
  ## the inverse using setinverse(), and return the inverse i. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
