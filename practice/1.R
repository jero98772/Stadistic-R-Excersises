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

rango <- function(x){
	max(x) - min(x)
}


varianza_manual <- function(x){
	n <- length(x)

	suma<-sum((x-mean(x))^2)/(n-1)
	return(suma)
}

desviacion_estandar_manual <- function(x){
	return(sqrt(varianza_manual(x)))	
}
varianza <- function(x){
	var(x)
}

desviacion_estandar <- function(x){
	sd(x)
}

quantile_manual <- function(x, x_ordered, p){
	n <- length(x)	
	res <- c()
	for (p in probs) {
		pos <-p*(n-1)+1
		k<- floor(pos)
		delta <- pos - k
		if(k<n){
			valor <- x[k] + delta * (x[k+1]-x[k])
		}else{
			valor <- x[n]
		}
		res <- c(res, valor)
	}
	return(res)<-paste0(probs*100, "%")
	return(res)
}
percentil <- function(x, p){
	x_ordered <- sort(x)
	quantile_manual(x, x_ordered,p/100)
}