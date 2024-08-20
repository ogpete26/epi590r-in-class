square <- function(x) {
	sq_val <- x*x
	return(sq_val)
}

y <- 52
square(y)
52^2


prop <- function(x, multiplier=1) {
	n <- length(x)
	proportion_val <- sum(x)/n
	multiplied_val <- multiplier*proportion_val
	return(multiplied_val)
}

prop(c(1,1,1,0,0))
prop(c(1,1,1,0,0), multiplier = 100)
