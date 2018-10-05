


f <- function(...) {
  append(list(a = 1, b = 2), list(...))
}

f(d = 4, e = 5)

g <- function(...) {
  list(...)
}

tester <- g()

length(tester)

