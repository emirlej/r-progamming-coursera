set.seed(1)
rpois(5, 2)

set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e

add_two <- function(x, y) return( x + y)

system.time({ for (i in 1:100000) add_two(i, i**2)})