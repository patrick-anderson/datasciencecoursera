  
 #This function creates a special "matrix" object that can cache its inverse.
 # makeCacheMatrix: return a list of functions to:
 # 1. Set the value of the matrix
 # 2. Get the value of the matrix
 # 3. Set the value of the inverse
 # 4. Get the value of the inverse
 
 
 makeCacheMatrix <- function(x = matrix()) {
 # inv will store the cached inverse matrix    
 inv <- NULL
 
 # Sets the value of the matrix
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
 # Gets the value of the matrix
     get <- function() x
 # Sets the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
 # Gets the value of the inverse
     getinverse <- function() inv
 # Return the matrix with newly defined functions
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 }
 
 #This function computes the inverse of the special "matrix" returned by 
 #makeCacheMatrix above. If the inverse has already been calculated 
 #(and the matrix has not changed), then the cachesolve should retrieve 
 #the inverse from the cache.
  
 cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     # If the inverse is already calculated, return it
     if(!is.null(inv)) {
         message("getting cached data.")
         return(inv)
     }
 
  # The inverse is not yet calculated, so we calculate it
     data <- x$get()
     inv <- solve(data)

     # Cache the inverse
     x$setinverse(inv)
 
  # Return it
     inv
 }
 
 ## Sample run:
   x = rbind(c(2, -3), c(-3, 2))
   m = makeCacheMatrix(x)
   m$get()

 
 cacheSolve(m)

 

