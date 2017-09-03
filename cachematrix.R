## Put comments here that give an overall description of what your
## functions do

#   Creates a vector, which is really a list that contains functions which will allow you to create 
#     special matrix objects that can then be used in conjunction with the cacheSolve function to 
#     calculate theinverse of matrices, and then to retrieve already-calculated and cached inverses 
#     more quickly.


## Write a short comment describing this function


#   Initializes x variable as matrix argument
#   Initializes my_inverse variable as NULL (so anytime a new makeCacheMatrix object is created the
#     my_inverse variable will be NULL/empty to begin with)
#   Creates a function, set(), which will allow the x variable to reset the matrix variable x to a 
#     new value if desired without having to run the makeCacheMatrix function again
#   Creates a function, get(), which will allow you to retrieve the matrix variable x
#   Creates a function, getinverse(), which will allow you to retrieve the matrix variable x's inverse once 
#     calculated with cacheSolve() function
#   Creates a list of the functions above so you can access them using the '$' notation   
#   


makeCacheMatrix <- function(x = matrix()) {
    my_inverse <- NULL
    set <- function(y){
      x <<- y
      my_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) my_inverse <<- inverse
    getinverse <- function() my_inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

#   The input, x, for the function is a matrix previously created using the prior function makeCacheMatrix
#   Uses the previously created function getinverse() to grab potentially previously calculated inverse 
#     of the input x
#   If the result is a value then it returns the already-calculated inverse
#   If the result is NULL then it needs to calculate the inverse
#     so it gets the matrix created with the makeCacheMatrix() function, using the $get() function
#   It uses that to assign the my_invesrve variable the inverse of the matrix
#   It then sets the setinverse value of the first function to the inverse
#     and then returns the inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    my_inverse <- x$getinverse()
    if(!is.null(my_inverse)){
        message("getting cached inverse")
        return(my_inverse)
    }
    data <- x$get()
    my_inverse <- solve(data)
    x$setinverse(my_inverse)
    my_inverse
}
