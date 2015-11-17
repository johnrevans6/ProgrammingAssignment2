##makeCacheMatrix creates a matrix object that can cache the inverse
##of a matrix. 
## .set sets the value of the matrix 
## .get gets the matrix (if it exists)
## .setCachedInverse caches the inverse matrix
## .getCachedInverse gets the cached inverse matrix (if it exists)

makeCacheMatrix <- function(x = matrix()) {
  
  inverted_matrix <- NULL
  
  set <- function(y){
    x <<- y
    inverted_matrix <<- NULL
  }
  
  get <- function() x
  
  setCachedInverse <- function(mat) inverted_matrix <<- mat
  
  getCachedInverse <- function() inverted_matrix
  
  list(set=set,get=get,
         setCachedInverse=setCachedInverse,
         getCachedInverse=getCachedInverse)
  

}


##cacheSolve computes the inverse of the matrix object 
##created in makeCacheMatrix by first checking to see if the inverse
##is cached. If it is, then cacheSolve retrieves the inverse, if it
##isn't, cacheSolve solves the inverse

##Assumption: All matrices passed to function are invertible
cacheSolve <- function(x, ...) {
        
  inverted_matrix <- x$getCachedInverse()
  
  if(!is.null(inverted_matrix)){
    message("Inverse is cached. Returning inverse.")
    inverted_matrix
  }
  
  
  data <- x$get()
  
  message("Calculating inverse matrix and writing to cache")
  inverted_matrix <- solve(data)
  x$setCachedInverse(inverted_matrix)
  
  ## Return a matrix that is the inverse of 'x'
  inverted_matrix
  
}
