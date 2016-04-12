# Caching the Inverse of a Matrix:
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
# Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       inver <- NULL
       setting <- function(y) {
             x <<- y
             inver <<- NULL
         }
       get <- function() x
       setinverse <- function(inverse) inver <<- inverse
       getinverse <- function() inver
       list(setting = setting,
                       get = get,
                       setinverse = setinverse,
                       getinverse = getinverse)
   }
 

# This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then it should retrieve the inverse from the cache.

   cacheSolve <- function(x, ...) {
         inver <- x$getinverse()
         if (!is.null(inver)) {
               message("getting cached data")
               return(inver)
           }
         mat <- x$get()
         inver <- solve(mat, ...)
         x$setinverse(inver)
         inver
     }

# 5a8d5bc69c4a61848af3ac2af03a7dce2d2b695e