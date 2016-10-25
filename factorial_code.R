if(!require(purrr)) {
    stop("the 'purrr' package needs to be installed first")
}

# data for tests

numbers <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
factorials <- c(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800)

# test function

test_function <- function(f) {
    stopifnot(all(map2_lgl(numbers, factorials, function(x, y) { identical(f(x), y) } )))
}


# Factorial_loop: a version that computes the factorial of an integer using looping (such as a for loop)

fuctorial_loop <- function(n) {
    
    stopifnot(n >= 0)
    
    ret <- 1
    
    if( n > 1) {
        for(i in 2:n) {
            ret <- ret * i
        }
    } 
    
    ret
}

# test fuctorial_loop

test_function(fuctorial_loop)



# Factorial_reduce: a version that computes the factorial using the reduce() function in the purrr package



















# Factorial_func: a version that uses recursion to compute the factorial.
# Factorial_mem: a version that uses memoization to compute the factorial.