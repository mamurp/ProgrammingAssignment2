## Put comments here that give an overall description of what your
## functions do
## Functions return the inversion of a given matrix.  As matrix inversion
## may use significant amounts of memory, function first checks to see if
## the calculation previously was done and saved in a cache.

## First function creates a list of functions and places the given matrix
## in the cache for later inversion

makeCacheMatrix <- function (x=matrix()) {
  m <- NULL
  setmat <- function (y)  {
    
    x <<- y
    m<<-NULL
  }
  getmat <- function () x
  setmatinvert <- function (solve)  m <<- solve  ##function inverts matrix
  getmatinvert <- function() m                   ##retrieves inversion
  list(setmat = setmat,
       getmat = getmat,
       setmatinvert = setmatinvert,
       getmatinvert = getmatinvert)
  
}

## Second function calculates the inversion unless the calculation has 
## been performed and placed in cache.

cacheSolve <- function(x, ...){
  s <- x$getmatinvert()
  ## retrieve, if exists, cached invert
  if(!is.null(s)) {
    message("Retrieving cached invert")
    return(s)
    
  }
  ## checks for cached invert, if exists, messages and uses invert
  s <- x$getmat()   ## null cached invert, retrieves initials matrix
  v <- solve(s)     ## solves(inverts) retrieved matrix
  x$setmatinvert(v)  ## place solution in cache
  v   ## output the solution
  
}
