# makeCacheMatrix returns a list of functions 
# It is meant to create a special matrix object that can cache its inverse
# It contains four functions:
# 1. set            stores the matrix  
# 2. get            returns the stored matrix 
# 3. setInverse     sets the cached value of the inverse of the matrix
# 4. getInverse     gets the cached value of the inverse of the matrix
 
    makeCacheMatrix <- function(originalMatrix = numeric()) {
          
    # variable originalMatrix holds the cached value,  or NULL if nothing is cached 
    # set the intial cache to NULL
      cachedMatrix <- NULL 
          
    # store the matrix 
    set <- function(newValue) { 
           originalMatrix <<- newValue 
           # since the matrix is assigned a new value, flush the cache 
           cachedMatrix <<- NULL 
    } 
  
    # returns the stored matrix 
    get <- function() { 
      originalMatrix 
    } 

    # sets the cached value  
    setInverse <- function(solve) { 
      cachedMatrix <<- solve 
    } 
   
    # get the cached value 
    getInverse <- function() { 
      cachedMatrix 
    } 
            
    # return a list. Each named element of the list is a function 
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
} 




 # The following function calculates the inverse of a "special" matrix created with makeCacheMatrix 

cacheSolve <- function(y, ...) {
    
  # get the cached value 
  invertedMatrix <- y$getInverse() 
    
  # if a cached value exists return it 
  if( !is.null(invertedMatrix) ) { 
               message("getting cached data") 
               return(invertedMatrix) 
       } 
   
    # else get the matrix, caclulate the inverse and store it in 
    matrixToInvert <- y$get()
  
     ## Calculate the inverse using matrix multiplication 
     invertedMatrix <- solve(matrixToInvert)
  
     ## Set the inverse to the object 
     y$setInverse(invertedMatrix) 
  
     ## Return the matrix 
    invertedMatrix 
 } 


