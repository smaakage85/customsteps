fn <- function(x, add = 5, subtract = 4) {
 x + add - subtract
}

out_fn <- function(x, ...) {
  fn(x, ...)
}

l1 <- list(x = 5)
l2 <- list(add = 2, subtract = 4)
l2 <- NULL
do.call(out_fn, append(l1,l2))


