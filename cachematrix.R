## Here are two functions that optimize the computational time , when repeatedly compute the inverse of a matrix that has not changed , it is better to have the result stored ready for use, without recalculating .

## This function "makeCacheMatrix" creates a matrix, and with the use of the solve function, computes the inverse and saved it on the cache.

makeCacheMatrix <- function(e= matrix()){
    ma<- NULL                # provides a default value of NULL, if cacheSolve has not yet been used
    z<- NULL                 # provides a default value of NULL, if cacheSolve has not yet been used
    setmatriz <-function(z){ # set the value of the matrix
      e<<-z                  # caches the inputted matrix so that cacheSolve can check whether it has changed
      ma<<- NULL             # sets the value of ma to NULL if cacheSolve hasn't been used to compute the matrix inverse
    }
    getmatriz <- function() e # to get the value of the input matrix
    setinversa<- function(solve) ma <<-solve # set the value of the inverse matrix
    getinversa <- function() ma # to get the value of the input matrix
    list(setmatriz = setmatriz, getmatriz = getmatriz,
         setinversa = setinversa,
         getinversa = getinversa) # This list contains the four functions that will be used as reference in the cacheSolve
}



## This function "cacheSolve" ,computes the inverse of a matrix "e", if it doesn't have a previous result of the matrix inverse saved in cache, in that case it returns the inverse matrix of the cache, or else after of obtaining the result it returns the inverse matrix "ma".

cacheSolve <- function (e=matrix(), ...){
  ma<- e$getinversa()           # if an inverse matrix has been previously computed, this function gets it
  if(!is.null(ma)){             # this verify if cacheSolve has been eject before, and has stored in cache the result
    message("getting cached data")
    return(ma)                  # Return a matrix that is the inverse of 'e', that has been previously computed and as stored on the Cache Memory.
  }
  z <-e$getmatriz()             # this function gets the value of the input matrix
  e$setmatriz(z)                # this function store the result in cache
  ma<-solve(z, ...)             # here, the inverse of the matrix input is computed
  e$setinversa(ma)              # ith the aplication of this function, the inverse matrix is stored in cache
  ma                            # Return a matrix that is the inverse of 'e'
}
        

