# ------------------------------------------------------------------------------
# This file contains functions used to help data processing
# ------------------------------------------------------------------------------

# download online data and save them in folder 'data'
Data_Update <- function(){
	require(data.table)
	# US COVID-19 case data from nytimes (https://github.com/nytimes/covid-19-data)
	# nytimes updates this data every day
	dat <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
	write.csv(dat, file = "data/case_state.csv")
	dat <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
	write.csv(dat, file = "data/case_county.csv")
	
	# Google mobility report (updated every Sunday)
	dat <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
	write.csv(dat, file = "data/moby.csv")
}

RenameLabels <- function(q, digits, percentage=TRUE){
	if (percentage==TRUE){
		q <- percent(q/100, 2)
	}else{
		q <- round(q, digits)
	}
	n <- length(q)
	labels <- array(NA, length(q) + 1)
	labels[1] <- paste("\u2264 ", q[1], sep = "")
	labels[n+1] <- paste("> ", q[n], sep = "")
	for (i in 2:(n)){
		labels[i] <- paste("(", q[i-1], ", ", q[i], "]", sep = "")
	}
	return(labels)
}



# plot
splot <- function(coord.state, data, var, q, title){
	data[, fills := findInterval(get(var), q)]
	f <- ggplot(data, aes(x, y, group = group)) +
		geom_polygon(aes(fill = fills)) +
		geom_polygon(data=coord.state, fill = NA, color = "black") +
		scale_fill_distiller(palette="RdYlBu", direction=-1, name="", na.value="white") +
		ggthemes::theme_map() +
		theme(legend.position = "none") +
		ggtitle(title)
	print(f)
	ggsave(paste("figs/fig2_", var, ".pdf", sep=""), f, width=5.2, height=4)
}

# table 
table1 <- function(x, z){
	result <- array(NA, length(z))
	y <- table(x)
	result[match(names(y), z)] <- y
	return(result)
}

# log zero
log1 <- function(x){
	y <- log(x)
	y[!is.finite(y)] <- 0
	return(y)
}

# mean
mean1 <- function(x, digit=2){
	return(round(mean(x[is.finite(x)]), digit))
}

# quantile
quantile1 <- function(x, digit=2){
	y <- round(quantile(as.numeric(x), probs=c(0,0.25,0.5,0.75,1), na.rm=TRUE), digit)
	return(y)
}


