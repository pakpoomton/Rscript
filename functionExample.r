add <- function(x,y){
	x+y
}

above10 <- function(x){
	use <- x > 10
	x[use]
}

above <- function(x, n=12){
	use <- x>n
	x[use]
}

columnean <- function(y, removeNA = TRUE){
	nc <- ncol(y)
	means <- numeric(nc)
	for(i in 1:nc){
		mean[i] <- mean(y[,i], na.rm=removeNA)
	}
}

make.power <- function(n){
	pow <- function(x){
		x^n
	}
	pow
}