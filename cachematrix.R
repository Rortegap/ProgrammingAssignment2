##Return the inverse of the matrix which is keep cached for save computation cost

## Write a short comment describing this function
##This function contains sub-functions to operate with the Matrix that we want to inverse. 
## set: Write the values of the matrix we are going to work with. We use here the reference to the parent environement.
## get: Display the values of the matrix we have worked with.
## getinverse: Get the values of the inverse matrix we have worked with. We use here the reference to the parent environement.
## setinverse: Display the values of the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  
  inverseMatrix <- NULL
  
  set <- function(y) {
    
    x <<- y
    inverseMatrix <<-  NULL
    
  }
  
  get <- function() {
    
    x
    
  }
  
  setinverse <- function(invM) {
    
    inverseMatrix <<- invM
    
  }
  
  getinverse <- function() {
    
    inverseMatrix
    
  }
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
#Function which return the value of the inversed Matrix. 
#The function just return the inversed matrix if the inversed Matrix has been already calculated 
#otherwise it perform the operation using the tool solve()


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  inverseMatrix<- x$getinverse()
  
  if(!is.null(inverseMatrix)) {
    
    message("getting cached data")
    
    return(inverseMatrix)
    
  }
  
  data <- x$get()
  
  inverseMatrix <- solve(data, ...)
  
  x$setinverse(inverseMatrix)
  
  inverseMatrix
}
