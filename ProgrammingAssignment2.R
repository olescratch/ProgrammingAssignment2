##These two functions first create a matrix, and than evaluates the inverse 
##of that function.

##The first function, entitled "MakeCacheMatrix", creates a matrix that
##first sets the value of the matrix, then gets that value. Secondly it
##sets the inverse value of the matrix, and then gets that value.


 MakeCacheMatrix <- function(x = matrix()) {
    +     m<-null
    +     set<-function(y){
        + x<<-y
        + n<<-NULL      +     
}
        +         get<-function()x
        +         setmatrix<-function(solve) m<<-solve
        +         getmatrix<-function()m
        +         list(set=set, get=get,
                       setmatrix=setmatrix,
                       getmatrix=getmatrix)
        + }

##The CacheSolve function evaluates the inverse of the above matrix. If 
##this value has already been calculated it retrieves it from the cache,
##if not it calculates the inverse of the function.
 
 cacheSolve <- function(x=matrix(), ...) {
     m<-x$getmatrix()
     if(!is.null(m)){
         message("getting cached data")
         return(m)
     }
     matrix<-x$get()
     m<-solve(matrix, ...)
     x$setmatrix(m)
     m
 }


