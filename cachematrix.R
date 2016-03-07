makeCacheMatrix <- function(x = matrix()) {
  +     inver <- NULL
  +     setting <- function(y) {
    +         x <<- y
    +         inver <<- NULL
    +     }
  +     get <- function() x
  +     setinverse <- function(inverse) inver <<- inverse
  +     getinverse <- function() inver
  +     list(setting = setting,
             +          get = get,
             +          setinverse = setinverse,
             +          getinverse = getinverse)
  + }
> 

  > 
  > cacheSolve <- function(x, ...) {
    +     inver <- x$getinverse()
    +     if (!is.null(inver)) {
      +         message("getting cached data")
      +         return(inver)
      +     }
    +     mat <- x$get()
    +     inver <- solve(mat, ...)
    +     x$setinverse(inver)
    +     inver
    + }
