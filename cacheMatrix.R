## This first function creates a list
## that contains 4 member functions: 
## 1.set the value of the matrix 
## 2.get the value of the matrix
## 3.set the value of the inverse matrix 
## 4.get the value of the inverse matrix  

## It uses <<- operator  which can be used to assign a value 
## to an object in an environment that is different from the current one.

makeCacheMatrix <- function(x = matrix()) {
	
	minv <- NULL # the result of the inverse is stored
	set <- function(y) {
		x <<- y
		minv <<- NULL # initialises minv to NULL
	}
	get <- function() x # returns the input matrix
	setInverse <- function(solve) minv <<- solve # sets the inverse matrix
	getInverse <- function() minv # returns the inverse matrix
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)

}


## The following function calculates the inverse of the matrix 
## returned with the makeCacheMatrix function. 
## If the inverse has already been calculated 
## cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)){ # if result is there:
		message("getting cached data")
		return(m) # returns the inverse 
	}
	data <- x$get() # if result is not there, gets the matrix
	m <- solve(data, ...) # it solves the inverse
	x$setInverse(m)
	m # returns the result
}

## Test
test <- matrix(rnorm(16),4,4) # creates a 4 x 4 square random matrix
testCache <- makeCacheMatrix(test)
