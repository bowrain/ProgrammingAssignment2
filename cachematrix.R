## makeVector and cacheSolve calculates the inverse of a matrix, checking first
## to see whether an inverse has already been calculated and stored


## makeVector() takes a matrix as input, and outputs a list of 4 functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}

## cacheSolve takes a the output of the above makeVector function as input
## and calculates the inverse of the matrix
## checking first whether an inverse is already stored
## if so, it simply gets the inverse
## if not, it calculates the inverse and stores it using the setsolve function

cacheSolve <- function(x, ...) {
       m <- x$getsolve()
       if(!is.null(m)) {
               message("getting cached data")
               return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setsolve(m)
       m
}