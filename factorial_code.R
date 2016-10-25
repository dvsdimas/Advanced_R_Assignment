if(!require(purrr)) {
    stop("the 'purrr' package needs to be installed first")
}


# data for tests

numbers <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
factorials <- c(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, 39916800, 479001600, 6227020800, 87178291200, 
                1307674368000, 20922789888000, 355687428096000, 6402373705728000, 121645100408832000, 2432902008176640000)

# test function

test_function <- function(f) {
    stopifnot(all(map2_lgl(numbers, factorials, function(x, y) { identical(f(x), y) } )))
}



# Factorial_loop: a version that computes the factorial of an integer using looping (such as a for loop)

Factorial_loop <- function(n) {
    
    stopifnot(n >= 0)
    
    ret <- 1
    
    if( n > 1) {
        for(i in 2:n) {
            ret <- ret * i
        }
    } 
    
    ret
}

test_function(Factorial_loop)



# Factorial_reduce: a version that computes the factorial using the reduce() function in the purrr package

Factorial_reduce <- function(n) {
    
    stopifnot(n >= 0)
    
    if( (n == 0) || (n == 1) ) {
        return(1)
    } 
    
    reduce(as.numeric(2:n), `*`)
}

test_function(Factorial_reduce)



# Factorial_func: a version that uses recursion to compute the factorial

Factorial_func <- function(n) {
    
    stopifnot(n >= 0)
    
    if( (n == 0) || (n == 1) ) {
        return(1)
    } 
    
    n * Factorial_func(n - 1)
}

test_function(Factorial_func)




# Factorial_mem: a version that uses memoization to compute the factorial

factorial_cache <- c(rep(NA, 100))

Factorial_mem <- function(n) {
    
    stopifnot(n >= 0)
    
    if( (n == 0) || (n == 1) ) {
        return(1)
    } 
    
    if(is.na(factorial_cache[n])){
        factorial_cache[n] <<- n * Factorial_mem(n - 1)
    } 
    
    factorial_cache[n]
}

test_function(Factorial_mem)




### Function's performance tests




























