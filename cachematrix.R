## This function takes a matrix X as input. It returns a list of useful functions which will have the ability to store the inverse 
## of X in cache later. The idea is that the functions in makeCacheMatrix(X) are able to store variables in an environment via the
## <<- operator and will be able to get the values from this environment later on. This environment is our "cache".

## This function avoids the multiple computation of the inverse of a matrix. When the inverse of a matrix has been calculated via 
## cacheSolve(makeCacheMatrix(X)), it will be stored in cache and running cacheSolve(makeCacheMatrix(X)) again will just get the inverse 
## from the cache.

makeCacheMatrix <- function(X = matrix()) {
  
  INV <- NULL 
  
  setMatrix <- function(Y) {   #Useful if we want to change the value of the input matrix X and store another inv. 
    X<<-Y         # But then, we could store another setMatrix() output in a separate variable 
    INV <<-NULL
  }
  
  getMatrix <- function() {X} # When we'll need to get the value of X from the cache 
  
  setInv <- function(M) {  #When the inverse matrix will be computed, we want to store it in the << envt thanks to the setInv function.
    INV <<- M
  }   
  
  getInv <- function() {INV} # Gets the INV from << if exists and to < if doesn't exist. Useful to get the INV from <<.
  
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInv=setInv, getInv=getInv)
}



## This function will compute the inverse of a matrix and store it in a cache thanks to the makeCacheMatrix() function if it 
## hasn't been calculated yet.
## Otherwise, it will get the calculated inverse from the cache without having to compute it and will print it.

## The goal of makeCacheMatrix and cacheSolve is to avoid the multiple computing of the inverse of a matrix.

cacheSolve <- function(X, ...) {
  ## Return a matrix that is the inverse of 'x'
  INV <- X$getInv()                              #if INV already computed, got it. Else, we'll compute it (INV = NULL in the < envt)
  
  if(!is.null(INV)) {
    print("Got the value from cache")
    return(INV)
  }
  #Here, else is not needed because we are RETURNING a value if we enter the if loop.
  
  mat <- X$getMatrix()  #Let's get our Matrix from the function's envt (not << though)
  
  INV <- solve(mat)     #Well, let's compute the actual inverse matrix !
  
  X$setInv(INV)         # Now, the inverse is stored into the << envt.
  
  INV           

}

message('done')

