## There are 2 functions that are created in this script. 
## 1. makeCacheMatrix 
## 2. cacheSolve

## The purpose of this script is to calculate the inverse of the matrix and save the computed value of the inverse of matrix 
## so that the inverse of the matrix should not have to be re-calculated if the inverse of the matrix has already been 
## previously calculated.

## makeCacheMatrix takes matrix as input. It is assumed that the matrix provided is inversable matrix
## There are no additional check performed if the matrix is inversable or not



## Write a short comment describing this function

##  makeCacheMatrix fuction can be used to perform 4 functionalities
##	1. setMatrix sub-fuction sets the matrix
##	2. getMatrix sub-fuction gets and displays the matrix that is already set
##	3. setInverse sub-fuction sets the inverse of the matrix. It simply sets the value and checks not additional constraints
##	4. getInverse sub-fuction gets the value of the Inverse of matrix already set

##  How to use the function makeCacheMatrix to get and set values
##  Define invertible matrix: B = matrix( c(4,3,3,2), nrow=2, ncol=2)
##  Get the inverse without using the function in this script: solve(B)
##  Create intial value of matrix : m1<-makeCacheMatrix(B)
##  Get the value of matrix : m1$getMatrix()



makeCacheMatrix <- function(x = matrix()) {
	Inverse <- NULL
	
        setMatrix <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(Inv) Inverse <<- Inv
        getInverse <- function() Inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)

}

## Write a short comment describing this function

##  cacheSolve fuction is dependent on makeCacheMatrix function and it's sub-functions. It tries to get the inverse of matrix if
##  the value of inverse of matrix has already been calculated else it calculates the inverse of the matrix and sets the inverse of 
## matrix so that it does not have to be calculated later.
##  and sets the inverse of matrix to the environment variable if the inverse

##How to execute it?

##B = matrix( c(4,3,3,2), nrow=2, ncol=2)
##B
##solve(B)

##m1<-makeCacheMatrix(B)
##m1$getMatrix()

##cacheSolve(m1)
##cacheSolve(m1)



cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
		LocalInverse <- x$getInverse()
		LocalInverse
		if(!is.null(LocalInverse)) {
		        message("getting cached data")
                return(LocalInverse)
        }
			message("Calculating Inverse of the Matrix")
			data <- x$getMatrix()
			CalculatedInverse<- solve(data)
			x$setInverse(CalculatedInverse)
			CalculatedInverse
}



 
