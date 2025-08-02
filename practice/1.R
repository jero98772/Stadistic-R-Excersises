my_mean <- function(x){
	sum(x)/length(x)
}
media <-function(x){
	mean(x)
}

my_median <- function(x){
	x_ordered <- sort(x)
	n <- length(x)
	if(n %% 2 == 1){
		x_ordered[(n+1)/2]
	}else{
		mitad1 <-x_ordered[n/2]
		mitad2 <-x_ordered[n/2+1]
		return((mitad1+mitad2)/2)
	}
}

mediana <- function(x){
	median(x)
}

moda <- function(x){
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}
my_mode <- function(x){
	freq <- list(x)
	for (val in x ){
		key <- as.character(val)
		if (!is.null(freq[[key]])){
			frequ[[key]] <- frequ[[key]] + 1
		}
		else{
			freq[[key]]<- 1
		}
	}
	counts <- unlist(freq)
	max_val<- as.numeric(names(counts[which.max(counts)]))

	return(max_val)
}