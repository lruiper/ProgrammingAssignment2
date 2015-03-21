## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL                          #defines a variable to store the inverse matrix							
	set <- function(y) {			  #function that sets the initial value of the matrix and the inverse matrix
		x <<- y				  #sets matrix x's value to y
		xinv <<- NULL			  #sets inverse matrix xinv's value to null
	}
	get <- function() x			  #function that returns the matrix x
	setInv <- function(inv) xinv <<- inv  #function to set the value of the inverse matrix xinv to the value of the parameter isnv
	getInv <- function() xinv             #function to return the inverse matrix 
	list(set = set, get = get,            #creates a list of functions 
	     setInv = setInv, 
           getInv = getInv)

}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m = x$getInv()      			 #query x's matrix cache
	  if(!is.null(m)){  			 #if not null, inverse matrix was previously calculated and stored
		message("getting cached data") #Message informing the data was previously cached
		return(m)				 #returns inverse matrix
	  }
	  data = x$get()				 #gets cached matrix
	  m = solve(data, ...)			 #solves its inverse matrix
	  x$setInv(m)				 #caches the result and returns it
	  return(m)	 
}
