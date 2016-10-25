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

# test implementation fuctorial_loop

stopifnot(identical(fuctorial_loop(0), 1))
stopifnot(identical(fuctorial_loop(1), 1))
stopifnot(identical(fuctorial_loop(2), 2))
stopifnot(identical(fuctorial_loop(3), 6))
stopifnot(identical(fuctorial_loop(4), 24))
stopifnot(identical(fuctorial_loop(5), 120))




# Factorial_reduce: a version that computes the factorial using the reduce() function in the purrr package. Alternatively, you can use the Reduce() function in the base package.
# Factorial_func: a version that uses recursion to compute the factorial.
# Factorial_mem: a version that uses memoization to compute the factorial.