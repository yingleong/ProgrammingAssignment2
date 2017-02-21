## makeCacheMatrix contains 4 internal functions and 1 internal variable
## m is the cache Matrix which is set to NULL by default

## set function is created for the same reason it is created in the makeVector(),
## so that its value can be changed without intializing another instance of the object
## (Quoted from https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md)

## get function is created so that it can be used to read the original raw data to be used for processing

## setInverse function is created to overwrite the internal variable m, which is the cache

## getInverse function is created to check the cache value of m, which is the internal variable as mentined



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function()x
  setInverse<-function(inverse)m<<-inverse
  getInverse<-function()m
  list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## cacheSolve is created as a shortcut to compute the inverse of raw matrix input through the function
## It basically makes use of the makeCacheMatrix internal function

## Generally, the function flow first checks the current cache data of in the input cache matrix data type
## and return it immediately if the cache data exists
## If not, it will retrieve the cache value, computes the inverse and store within the same cache matrix data type
## as its internal cache variable


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data");
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setInverse(m)
  m
}
