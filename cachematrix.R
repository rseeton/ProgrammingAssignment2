###############################################################################
## Richard Seeton (richard@seeton.net)                                       ##
###############################################################################
##                                                                           ##
## R Programming - Programming Assignment #2                                 ##
## Write  a pair of functions that cache the inverse of a matrix.            ##
##                                                                           ##
##	makeCacheMatrix: This function creates a special "matrix" object         ##
##                   that can cache its inverse.                             ##
##	cacheSolve:  This function computes the inverse of the special "matrix"  ##
##	             returned by makeCacheMatrix above. If the inverse has       ##
##				 already been calculated (and the matrix has not changed),         ##
##				 then the cachesolve should retrieve the inverse from the          ##
##				 cache.                                                            ##  
###############################################################################

makeCacheMatrix <- function(matrix_In = matrix()) {

###############################################################################
## makeCacheMatrix takes a matrix object (Matrix_In) as an arguement and     ##
## returns a list containing 4 functions                                     ##
##                                                                           ##	
##	set the value of the matrix                                              ##
##	get the value of the matrix                                              ##
##	set the value of the inverse of the matrix                               ##
##	get the value of the inverse of the matrix                               ##
##                                                                           ##
###############################################################################

	m <- NULL
	
	matrix_Set <- function(y) {          #Creates setter function (Not required but good practice)
		matrix_In <<- y
		m <<- NULL
	}
		
	matrix_Get <- function() matrix_In   #Creates function to return the source matrix provided
	
	matrix_Inverse_Set <- function(solve) m <<- solve	#Create function to calculate the inverse of the source matrix
	
	matrix_Inverse_Get <- function() m   #Creates function to return the inverse of the source matrix
	
	list(matrix_Set = matrix_Set, 						
		 matrix_Get = matrix_Get,
		 matrix_Inverse_Set = matrix_Inverse_Set, 
		 matrix_Inverse_Get = matrix_Inverse_Get)		#Assembles the list of the functions

}	
cacheSolve <- function(list_CacheMatrix, ...) {
###############################################################################
## cacheSolve takes the function list from makeCacheMatrix as an arguement.  ##
## cacheSolve returns the inverse of the matrix used when creating the       ##
## makeCacheMatrix output.                                                   ##
## If the inverse of the matrix has already been calculated, the function    ##
## returns the cached value.  Otherwise, the matrix is inverted ('solved')   ##
## and this solution is cached and returned                                  ##
###############################################################################

  m <- list_CacheMatrix$matrix_Inverse_Get()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- list_CacheMatrix$matrix_Get()
  m <- solve(data, ...)
  list_CacheMatrix$matrix_Inverse_Set(m)
  m
}
