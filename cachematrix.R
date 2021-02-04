###################################################################################
## Richard Seeton (richard@seeton.net)                                           ##
## 2021-02-03                                                                    ##
###################################################################################
##                                                                               ##
## R Programming - Programming Assignment #2                                     ##
## Write  a pair of functions that cache the inverse of a matrix.                ##
##                                                                               ##
##	makeCacheMatrix: This function creates a special "matrix" object             ##
##                   that can cache its inverse.                                 ##
##	cacheSolve:      This function computes the inverse of the special "matrix"  ##
##	                 returned by makeCacheMatrix above. If the inverse has       ##
##	                 already been calculated (and the matrix has not changed),   ##
##	                 then the cachesolve should retrieve the inverse from the    ##
##	                 cache.                                                      ##  
###################################################################################

makeCacheMatrix <- function(matrix_In = matrix()) {

###################################################################################
## makeCacheMatrix takes a matrix object (Matrix_In) as an arguement and         ##
## returns a list containing 4 functions                                         ##
##                                                                               ##	
##      matrix_Set  - assign the value of the original matrix                    ##
##      matrix_Get  - retrieve the value of the original matrix                  ##
##      matrix_Inverse_Set - assign the value of the inverse of the matrix       ##
##      matrix_Inverse_Set - retrieve the value of the inverse of the matrix     ##
##                                                                               ##
###################################################################################

	matrix_Inverse <- NULL
	
	#Creates setter function (Not required but good practice)
	matrix_Set <- function(y) {
		matrix_In <<- y
		matrix_Inverse <<- NULL
		message("set Original matrix, clear Inverse matrix cached data")
	}
	
	#Creates function to return the source matrix provided
	matrix_Get <- function() matrix_In   
	
	#Create function to calculate the inverse of the source matrix
	matrix_Inverse_Set <- function(solve) {matrix_Inverse <<- solve}
	#Creates function to return the inverse of the source matrix
	matrix_Inverse_Get <- function() {matrix_Inverse}
	
	#Assembles the functions in a list
	list(matrix_Set = matrix_Set, 						
		 matrix_Get = matrix_Get,
		 matrix_Inverse_Set = matrix_Inverse_Set, 
		 matrix_Inverse_Get = matrix_Inverse_Get)

}	
cacheSolve <- function(list_CacheMatrix, ...) {
###############################################################################
## cacheSolve takes the function list from makeCacheMatrix as an arguement.  ##
## cacheSolve returns the inverse of the matrix used when creating the       ##
## makeCacheMatrix output.                                                   ##
##                                                                           ##
## If the inverse of the matrix has already been calculated, the function    ##
## returns the cached value.                                                 ##
##                                                                           ##
## If the inverse of the matrix has not already been calculated, the function##
## inverts ('solves') the matrix is inverted and the solution is cached and  ##
## returned.                                                                 ##
###############################################################################

 	# Get the object currently stored in the slot for the Inverted Matrix
	matrix_Inverse <- list_CacheMatrix$matrix_Inverse_Get()
	
	# Return the object retrieved from the Inverted Matrix slot, if it exists
	if(!is.null(matrix_Inverse)) {
		message("getting cached data")
		return(matrix_Inverse)  #This sends us back to the calling frame
	}
	else {
	  # message("no cached data available")
	}
  
  # Get the supplied matrix, 'solve' it, save it to the Inverted Matrix slot 
  # and return the saved result
	matrix_Original <- list_CacheMatrix$matrix_Get()
	matrix_Inverse  <- solve(matrix_Original, ...)
	list_CacheMatrix$matrix_Inverse_Set(matrix_Inverse)
	return(matrix_Inverse)
}
