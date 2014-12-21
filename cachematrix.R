## functions the cache a matrix inverse and read it directly  from cache 
## instead of calculating everytime which can be very slow 

## makecachematrix, makes the "objet" where to cache the inverse of the matrix entered as argument

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


## cacheSolve caches the inverse of a matrix "x" if not already cached and returns its inverse

## the function can be faster if we add another argument for a verbose mode
## so we could activate the message (line 28) only if needed
cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		## if the inverse is already set returns the cached data
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        	## the inverse isn't cached yet need to calculate the inverse and cache it
		inv <- solve(x$get())
		x$setinv(inv)
		## return the newly cached inverse
		return(inv)## could also use : return(x$getinv())
}
