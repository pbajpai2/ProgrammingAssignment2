 # makeCacheMatrix: this function makes a special "matrix", composed in a list of functions that ultimatey gets the inverse 

# of its matrix



makeCacheMatrix <- function(x = matrix()) {    # The function takes the argument x, which is an empty matrix

  I <- NULL                                    # I, the inverse of the matrix, is originally set to be an empty object

  set <- function(y)  {                        # The function 'set' defines the argument, x, as a new empty matrix, y,

    x <<- y                                    # and redefines I as an empty object

    I <<- NULL

  }

  get <- function() x                          # The function 'get' returns the matrix, x

  

  setInverse <- function(solve)  I <<- solve   # The function 'setInverse' utilizes the 'solve' function in order to 

                                               # calculate the inverse

  getInverse <- function() I                   # The function 'getInverse' returns the inverse of the matrix, I

  

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)   # Lists the functions to be carried out

}





# cacheSolve: this funcction calculates the inverse of the special "matrix" created by makeCacheMatrix.

# This is carried out by first checking if the inverse has already been calculated. If so, the inverse matrix, I, is returned.

# If not, then the function calculates the inverse using the setInverse-function, and then the inverse, I, is returned.



cacheSolve <- function(x, ...) {               # The function takes the argument x, and passes other potential arguments to the ...

  I <- x$getInverse()                          # Assignes the inverse of the matrix to I, if it already exists

  if(!is.null(I))  {                           # if-loop which checks if I contains an object

    message("getting cached data")             # If the statement checks out, a message is printed and the inverse matrix, I, is returned

    return(I)                                  # If not, the computation is carried out, as displayed below

  }

  data <- x$get()                              # The matrix, x, is assigned to the object 'data'

  I <- solve(data, ...)                        # The solve-fuction is assigned to I

  x$setInverse(I)                              # The solve-function carries out the computation of the inverse

  I                                            # Return a matrix that is the inverse of 'x'

}
