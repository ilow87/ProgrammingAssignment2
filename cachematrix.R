## makeCacheMatrix() defines 3 functions and assigns them as a list to be returned
## by the function makeCaceMatrix. These 4 functions are called within cacheSolve().
## The 3 functions defined are as follows:
## 1. get() retrieves the cached matrix when called
## 2. setinv() caches the value of the inversed matrix when called
## 3. getinv() retrieves the cached inverse matrix when called. It returns a NULL when
##    no inversed matrix has been stored.


## This function creates a list containing functions for retrieval and the setting of values 
## for both the original matrix and the inverse matrix.
## Each time this function is called, the value of the inverse matrix is set to NULL and the
## original matrix is cached within the function get().
## All functions defined in makeCacheMatrix() are only called within the matrix cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  get <- function() x
  setinv <- function(matrixinv) inv <<- matrixinv
  getinv <- function() inv
  list(get = get, 
       setinv = setinv,
       getinv = getinv)

}


## This matrix retrieves the cached matrix and inversed matrix (if available) from x
## and assigns the values into data and inv respectively.
## The cached inverse matrix is assigned to inv by calling of the function getinv()
## that is defined within x. If the inversed matrix is not null, the cacheSolve immediately
## returns the inversed matrix.
## If the cached inverse matrix is null, the function proceeds to assign the cached matrix 
## to data by calling the get() function that is defined within x.
## The function proceeds to compute the inversed matrix and assigns the value to inv.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
