## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates and returns a list of functions used by cacheSolve to get or set the inverted
## matrix in cache

makeCacheMatrix <- function(x = matrix()) {   
        cached <- NULL         # stores cached value and initilizes it to Null
        set <- function(y){   # creates the matrix in the working environment
                x <<- y
                cached <<- NULL
        }
        get <- function() x    # gets the value of the matrix
        setMatrix <- function(inverse)    
                cached <<- inverse    # invert matrix and store in cache
        getInverse <- function() cached   # get the inverted matrix from cache
        list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse) 
        #returns the created functions to the working environment
}


## cacheSolve attempts to get the inverse of the matrix stored in cache but if it can't it then creates 
## it from matrix <- x$get()

cacheSolve <- function(x, ...) {
        cached <- x$getInverse()
        if(!is.null(cached)){  #returns inverted matrix from cache if it exists
                message("getting cached data")  # if it does exists, this message appears
                return(cached) #displays matrix in console
        }
        
        matrix <- x$get()  #creates matrix if the cache didn't exist
        cached <- solve(matrix, ...)
        x$setMatrix(cached)   # set inverted matrix in cache
        cached  # display matrix in console
}

## Now we will test to see if the function works
B <- (matrix(1:4,2,2))  # creates matrix
B1 <- makeCacheMatrix(B) #assigns the matrix to a vector
cacheSolve(B1) # runs the cacheSolve function on stored matrix
print(B) # prints original matrix to compare against the inverted matrix
