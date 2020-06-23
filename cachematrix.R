## These two functions (makeCacheMatrix and cacheSolve) compute the inverse of a matrix and cache this inverse. 


# Function 1: makeCacheMatrix

# Description: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {        # The argument given to the makeCacheMatrix function is matrix 
                                                   # (which has th be square in order for it to be invertible)
  
        i <- NULL                                  # i is the inverse of x and at this point it has no value assigned 
                                                   # to it (NULL), i.e. it works as a placeholder for future values
  
        set <- function(y) {                       # A function (set) that
    
                x <<- y                            # sets the value of matrix to y, and
    
                i <<- NULL                         # and the value of the inverse of y to NULL 
        }
 
        get <- function() x                        # A function (get) that returns the value of the matrix x
  
        setinv <- function(inverse) i <<- inverse  # A function (setinv) that sets the inverse (i) of x to inverse
  
        getinv <- function() i                     # A function (getinv) that returns the inverse, i
  
        list(set = set, get = get,                 # returns the 'special matrix' containing all of the functions 
             setinv = setinv,
             getinv = getinv)
}


# Function 2: cacheSolve

# Description: This function calculates the inverse of the special "matrix" created with the makeCacheMatrix.
# Before computing the inverse, the function checks if the inverse has already been calculated, and thereby is 
# cached. If the inverse has already been computed, the inverse is retrieved from the cache and no (sometimes) 
# timeconsuming computation is performed. If the inverse has NOT been computed before, it is computed and cached 
# (its value is set) in the cache via the setinv function. This means that next time we want to compute the 
# inverse of the same matrix, it does not have to be computed but can be retrieved from the cache. 


cacheSolve <- function(x, ...) {                  # the argument is the special matrix from makeCacheMatrix, 
                                                  # i.e. the output from makeCacheMatrix
  
        i <- x$getinv()                           # i is the inverse of the special matrix from makeCacheMatrix
  
        if(!is.null(i)) {                         # If i is not NULL, i.e. if the inverse of the special matrix has 
                                                  # been computed before, we will get the message...
    
                message("getting cached data")    # ...telling us that the inverse is retrieved from the cache, and
    
                return(i)                         # the inverse will just be returned (not computed). 
        }
  
        matrix <- x$get()                         # If i on the other hand has NOT been computed before, the function 
                                                  # will 'get' the special matrix returned by makeCacheMatrix, and
  
        i <- solve(matrix, ...)                   # compute the inverse (i) of this matrix, using the R solve function. 
  
        x$setinv(i)                               # The inverse will be cached for the future (setinv), and 
  
        i                                         # the newly computed inverse will be returned. 
}

## Validation 

## Create a matrix that we want to do all this for
m <- matrix(1:4, 2, 2)

# Output

#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4


## Create the 'special' matrix for m
sm <- makeCacheMatrix(m)

# Output 

#$set
#function(y) {                       # A function (set) that
#  
#  x <<- y                            # sets the value of matrix to y, and
#  
#  i <<- NULL                         # and the value of the inverse of y to NULL 
#}
#<environment: 0x000000000bb79670>
#  
#  $get
#function() x
#<environment: 0x000000000bb79670>
#  
#  $setinv
#function(inverse) i <<- inverse
#<environment: 0x000000000bb79670>
#  
#  $getinv
#function() i
#<environment: 0x000000000bb79670>

## Calculate the inverse of the special matrix

ism <- cacheSolve(sm)

# Output

#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


## What happens now if we want to calculate the inverse again?

cacheSolve(sm)

#Output

#We get the message: getting cached data, and the inverse is collected from the cache instead of being calculated:

# getting cached data
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


