# Assignment 2 is to write a pair of functions that cache the inverse of a matrix.
#First we need to initialize the objects x and inv.
makeCacheMatrix <- function(x = matrix()) { 
        #This function creates a special "matrix" object that can cache its inverse.
        #x is intialised as afunction argument.
        inv <- NULL
        #inv(inverse) is initialised to  Null within this makeCachematrix environment which will be used later.
        set <- function(y) {
                # "setters are modules which will set the data values within an object.
                # It is taking an argument y(cannot be x since it is already defined in the first function)
                x <<- y
                #<<- is an assignment operator which assigns y to the object x(parent environment).
                inv <<- NULL
                # It assigns value Null untill it gets any value that has beenn executed by cacheSolve function
        }
        get <- function() x
        #"getters" are modules that retrieve data within an object.
        # x is not defined with get function. so it is called from parent environment(makeCacheMatrix)
        setinv <- function(inverse) inv <<- inverse
        #inv is defined in parent environment and we will acess it after this function and uses
        #this <<- operator to assign the argument to the value of inv.
        getinv <- function() inv
        #The makeCachematrix will define the inverse inv
        #All the get and set values for the objects have been defined.
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        #This will assign all the functions as elements of a list and returns it to parent environment.
}	
cacheSolve <- function(x, ...) {
        ## This function is required to retrieve the inverse from makeCachematrix	        
        inv <- x$getinv()
        #This is to retrieve inverse from the object passed in as argument
        if(!is.null(inv)) {
                #It will check if the value is null.whenever we set the new matrix to the object
                #It returns the valid inverse to the parent environment.
                message("getting cached result")
                return(inv)
                
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        #If it has a null value.It calculates the solve(),uses the setinv() function to 
        #set the inverse in the input object and then returns the inverse to the parent
        #environment by printing the inverse of a matrix.
}	

> x= matrix(rnorm(36),6,6)
> m = makeCacheMatrix(x)
> m$get()
            [,1]       [,2]      [,3]       [,4]       [,5]       [,6]
[1,]  0.43689784  0.6915704 0.6194693 -0.4047107  0.3173676  0.1666396
[2,]  0.49750496 -0.8781118 0.9902050 -1.5279713  0.3978849 -0.1375128
[3,]  1.56366727  1.5700992 0.9807998  0.1947928 -1.2462574 -0.3943614
[4,]  0.56711195  1.4309466 1.0772564 -0.2092455  0.1973003 -0.6802498
[5,] -0.69222071  1.1526850 0.6367687 -0.4464797 -0.3535713 -2.0504924
[6,]  0.02381657 -1.0604711 1.3799194  0.9676459  1.3784530  2.2738786
> cacheSolve(m)
           [,1]        [,2]         [,3]       [,4]       [,5]        [,6]
[1,] -1.7211201  0.32898687 -0.079114822  1.7506446 -1.0489168 -0.28984546
[2,]  1.4061032 -0.41496210  0.009885609 -0.4754638  0.2196005 -0.07063771
[3,]  0.1775181  0.16029046  0.321815802 -0.4878866  0.5689254  0.41957584
[4,] -1.7339671 -0.16024559 -0.035630554  1.1647475 -0.3302518  0.16183847
[5,] -1.1531288  0.07748203 -0.725382722  1.8989948 -0.7602129 -0.15404127
[6,]  2.0029927 -0.27302378  0.265041215 -1.5908518  0.3695338  0.17975866
> cacheSolve(m)
getting cached result
           [,1]        [,2]         [,3]       [,4]       [,5]        [,6]
[1,] -1.7211201  0.32898687 -0.079114822  1.7506446 -1.0489168 -0.28984546
[2,]  1.4061032 -0.41496210  0.009885609 -0.4754638  0.2196005 -0.07063771
[3,]  0.1775181  0.16029046  0.321815802 -0.4878866  0.5689254  0.41957584
[4,] -1.7339671 -0.16024559 -0.035630554  1.1647475 -0.3302518  0.16183847
[5,] -1.1531288  0.07748203 -0.725382722  1.8989948 -0.7602129 -0.15404127
[6,]  2.0029927 -0.27302378  0.265041215 -1.5908518  0.3695338  0.17975866
