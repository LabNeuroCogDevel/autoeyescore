system.time( {
  N <- 1000
  lst <- lapply(1:N, function(i) data.frame(x=rnorm(10), y=rnorm(10)))
  f <- function(df) lm(y ~ x, data=df)
  models <- lapply(lst, f)
  g <- function(m) coef(m)[2]
  slopes <- lapply(models, g)
  unlist(slopes, use.names=F)
} )

system.time( {
  N <- 1000
  slopes <- numeric(N)
  for (i in 1:N) {
    df <- data.frame(x=rnorm(10), y=rnorm(10))
    slopes[i] <- coef(lm(y ~ x, data=df))[2]
  }
  slopes
} )

unlist(slopes, use.names=F)


f <- function() {
  x <- 2
  g <- function() x
  i <- h
  environment(i) <- environment()
  list(x, g(), h(), i())
}

h <- function() x

x <- 1
f()

