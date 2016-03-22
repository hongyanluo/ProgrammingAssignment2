## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its matrix inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  ## define argument matrix()
    inv_matrix<- NULL                         ## matrix inversion, give initial value of NULL to inv_matrix
    set <- function(y){                      ## define set function
      x<<- y                                 ## assign new value of the matrix in parent environment
      inv_matrix<<- NULL                     ## reset inv_matrix to NULL
    }
    get<- function()x                        ## set get function
    setinverse<- function(inverse) inv_matrix<<- inverse  ## assign value of inv_assign in parent environment
    getinverse<- function()inv_matrix                    ## get the value
    list(set=set,                                       ## create a list for later use
         get=get, 
         setinverse=setinverse,
         getinverse=getinverse) 
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_matrix<-x$getinverse()
    if(!is.null(inv_matrix)){
      message ("getting cached data")
      return(inv_matrix)
    }
    data<- x$get()
    inv_matrix<- solve(data,...)
    x$setinverse(inv_matrix)
    inv_matrix
}

