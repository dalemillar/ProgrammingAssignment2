## makeCacheMatrix is a constructor function which takes ip_matrix, a matrix, as a
## formal arguement.  It has a local variable, cacher, which holds the cached
## inverse matrix - this is initialized to NULL.  There are four functions inside
## makeCachMatrix: 
## set - which has a formal arguement of the inverted matrix and
## this is passed to cache
## get - simply returns the value of ip_matrix
## set_inverse takes a formal arguement of the inverse and places it in cacher
## local variable.
## get_inverse

makeCacheMatrix <- function(ip_matrix = matrix()) {
  
  cacher<-NULL
  
  set<-function(new_matrix){
    ip_matrix<<-new_matrix
    cacher<<-NULL
    ## a new matrix is being fed in and so cache is NULLed - ie inversion not done
    ## yet.  Note that cacher(local) and ip_matrix(arguement) are in parent
    ## environment, hence the deep assignment.
  }
  
  get<-function()ip_matrix
  ##simply returns ip_matrix
  
  set_inverse<-function(inverse)cacher<<-inverse
  ##places inverse in cacher - note use of deep assignment as cacher is in parent
  ##environment
  
  get_inverse<-function() cacher
  ##simply returns the value in cacher
  
  list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
  
}


## cacheSolve function uses the embedded functions of makeCacheMatrix to see if the 
## calculation is already in cacher, if so it returns the cached inverse matrix;
## if not, it calculates the inverse, places it in cache and prints the inverse.

cacheSolve <- function(x, ...) {
  cacher<-x$get_inverse() 
  
  if(!is.null(cacher)){
    message("getting cached data")
    return(cacher)
  ##If cacher is not NULL, print message and return cacher
  }
  
  data<-x$get()
  cacher<-solve(data,...)
  x$set_inverse(cacher)
  cacher
  ##Otherwise get the input_matrix, solve the inverse and put it in cacher.  
}  

test_cache_solve<-function(){
  b<-makeCacheMatrix(matrix(1:4,2,2))
  print(cacheSolve(b))
  print(cacheSolve(b))
}

