x <- c(1,3,5,7,9)

new_mean <- function() {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}

