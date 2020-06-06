## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  #Empty inverse matrix
  a <- NULL
  
  set <- function(y)
  {
    #sets matrix value in parent env
    x <<- y
    #sets empty inverse matrix in parent env
    a <<- NULL
  }
  
  #To get the value of matrix
  get <- function() { x }
  
  #Sets inverse matrix value in parent env
  setInvMatrix <- function(y)
  { a <<- y }
  
  #To get the value of inverse matrix
  getInvMatrix <- function() {a}
  
  
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
  
}


## It computes the inverse of matrix returned by previous function, 
## If inverse is not empty then it will return cached matrix
cacheSolve <- function(x, ...) {
  
  #To get inverse matrix
  a <- x$getInvMatrix()
  
  
  #if inverse matrix already exist (not NULL) then print msg and return cached matrix
  if(!is.null(a))
  {
    message("Accessing Cache")
    return(a)
  }
  
  #If inverse matrix does not exist 
  
  #To get the matrix
  matrix <- x$get()
  
  #Inverse calculation
  a <- solve(matrix, ...)
  
  #Update inverse matrix in parent env
  x$setInvMatrix(a)
  
  
  #Return inverse matrix
  a
}