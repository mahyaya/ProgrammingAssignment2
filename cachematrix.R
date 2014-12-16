## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set <- function(m){
			x <<- m
			inv<<-NULL
	  }
	  get <- function() x
	  setinv <- function(inverse) inv <<- inverse
	  getinv <- function() inv
	  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve caches the inverse of a matrix (if the matrix isn't square or isn't solvable return's a NULL)

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		mat <- x$get()
		## testing if the matrix is solvable
		dims<-dim(mat)
		if(dims[1]!=dims[2]){
				message("matrix is not square")
				return(NULL)
		}
		if(det(mat)<1e-9){ 
				message("inverse may be impossible det~0")
                return(NULL)
		  }
		  ## So far so good
		inv <- solve(mat)
		x$setinv(inv)
		inv
}
