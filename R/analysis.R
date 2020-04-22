# ------------------------------------------------------------------------------
# Load packages and functions
# ------------------------------------------------------------------------------
packages <- c("data.table", "ggplot2", "sp", "spdep", "spatialreg", "tigris", 
							"tmaptools", "usmap", "scales", "egg", "splm", "plm")
lapply(packages, require, character.only = TRUE)
source("R/functions.R")

#-------------------------------------------------------------------------------
# Data Preparation -------------------------------------------------------------
#-------------------------------------------------------------------------------

# update data by downloading from web to folder 'data')
#Data_Update()

# controls: county characteristics
dat <- fread("data/control1_county.csv", skip=1)
statenames <- dat[fips%%1000==0, list(state, county)]
contl <- dat[fips%%1000>0]

# controls: demographics
dat <- fread("data/control2_county.csv")
dhs <- dat[, lapply(.SD, sum), by=fips, .SDcols=c(6:10)]
dhs$old <- dat[agegroup>=13, sum(pop), by=fips]$V1
contl <- merge(contl, dhs, by="fips", all.x=TRUE)

# controls: calculate ratios
contl[, highschl := 100*(1-as.numeric(gsub(",","",less_than_high))/pop)]
contl[, ln_income := log(income)]
contl[, gender := 100*male/pop]
contl[, nonwhite := 100*(1-white/pop)]
contl[, old := 100*old/pop]
contl <- contl[, list(fips, highschl, unemp, ln_income, poverty, mig_dom, gender, nonwhite, old)]

# controls: pop density from US census data
dat <- fread("data/census.txt")
dat[, dens := log1(PST045214/LND110210)] # dens is in people/sq mi
contl <- merge(contl, dat[, list(fips, dens)], by="fips", all.x=TRUE)

# variable of interest: mobility
moby <- fread("data/moby.csv")
setnames(moby, c("V1", "country_code", "country_name", "state", "county", "date",
								 "retail", "grocery", "parks", "transit", "workplaces", "residential"))
moby <- moby[country_code=="US" & county!="", 4:12] # select county data
moby[county=="Doña Ana County", county := "Dona Ana County"]
moby[county=="Shannon County" & state=="South Dakota", county := "Oglala Lakota County"]
moby[, id := tolower(paste(state, county, sep = ","))]
moby[, date := as.Date(date)]
moby[, mob:=mean(c(grocery, retail, parks, transit, workplaces), na.rm=TRUE), by=.(id, date)]
setorder(moby, id, date) 

# dependent variable: COVID-19 cases
cases <- fread("data/case_county.csv")[, !c('V1', 'deaths'), with=FALSE]
cases <- cases[!is.na(fips)] # some cases have no fips
cases[, date := as.Date(date)]
cases[, ln_cases := log(cases)]
setorder(cases, fips, date)

# US state coordinates
coord.state <- data.table(us_map("state"))
setnames(coord.state, "full", "state")
coord.state <- coord.state[, list(x, y, group, state)]

# US county coordinates
coord.cnty <- data.table(us_map("county"))
coord.cnty[, fips := as.numeric(fips)]
coord.cnty[, id := tolower(paste(full, gsub(" city", "", county), sep = ","))]
coord.cnty <- coord.cnty[, list(x, y, group, fips, id)]

#-------------------------------------------------------------------------------
# Data description -------------------------------------------------------------
#-------------------------------------------------------------------------------

# county map of cases
dt.fig1 <- merge(coord.cnty, cases[date=="2020-04-18"], by="fips", all.x=TRUE)
q <- c(10, 50, 100, 1000, 10000)
fig1 <- ggplot(dt.fig1, aes(x, y, group = group)) +
	geom_polygon(aes(fill = findInterval(cases, q))) +
	geom_polygon(data=coord.state, fill = NA, color = "black") +
	scale_fill_distiller(palette="Reds", direction=1, name="", na.value="white", labels=RenameLabels(q,0,0)) +
	ggthemes::theme_map() + 
	theme(legend.justification=c(1,0), legend.position=c(1,0))
ggsave("figs/fig1.png", fig1, width=6.5, height=5)

# county map of changes in mobility
dt.fig2 <- merge(coord.cnty, moby[date=="2020-04-11"], by="id", all.x=TRUE)
q <- c(-40, -30, -20, -10, 0, 10, 20)
places <- c("Groceries and Pharmacies", "Parks", "Residential", "Retail and Recreation", "Transit Stations", "Workplaces")
splot(coord.state, dt.fig2, "grocery", q, places[1])
splot(coord.state, dt.fig2, "parks", q, places[2])
splot(coord.state, dt.fig2, "residential", q, places[3])
splot(coord.state, dt.fig2, "retail", q, places[4])
splot(coord.state, dt.fig2, "transit", q, places[5])
splot(coord.state, dt.fig2, "workplaces", q, places[6])

# summary statistics - cases
dates <- seq(as.Date("2020-02-29"), max(cases$date), 7)
q <- c(10, 50, 100, 1000, 10000)
dt.tab1 <- cases[date %in% dates]
dt.tab1[, cases_group := findInterval(cases, q)]
tab1 <- dcast(dt.tab1[, .N, keyby=.(date, cases_group)], date ~ cases_group, value.var="N")
setnames(tab1, c("date", RenameLabels(q,0,FALSE)))
tab1$Counties_with_Cases <- apply(tab1[,-1], 1, sum, na.rm=TRUE)
tab1$Total_Number_of_Counties <- rep(3142, length(dates))
tab1[is.na(tab1)] <- ""

# summary statistics - mobility
dt.tab2 <- moby[date %in% dates][, c(10,3,1,2,5,6,9,4,7,8,11)]
tab2a <- dt.tab2[, lapply(.SD, function(z) sum(!is.na(z))), by=date, .SDcols=c(5:10)]
tab2a$date <- as.character(tab2a$date)
tab2a <- rbind(tab2a, data.table(date="Total_Counties", matrix(rep(3142,6), nrow=1)), use.names=FALSE)
tab2b <- dt.tab2[, lapply(.SD, mean1, digit=0), by=date, .SDcols=c(5:10)]
tab2c <- dt.tab2[date==max(date), lapply(.SD, quantile1, digit=0), .SDcols=c(5:10)]
tab2c$Q <- c("min", "Q1", "median", "Q3", "max")
tab2c <- tab2c[, c(7,1:6)]

#-------------------------------------------------------------------------------
# Spatial Regression -----------------------------------------------------------
#-------------------------------------------------------------------------------

# merge contl and moby via coordinates
X <- merge(moby, unique(coord.cnty[, list(fips, id)], by="fips"), by="id")
X <- merge(X, contl, by="fips")

# calculate percentage changes in cases
cases[, g := c(NA, diff(ln_cases)), by=fips]
cases[, g_1 := shift(g, 1, NA, "lag"), by=fips]
cases[, n := as.numeric(date - moby_date)]

# estimation
SPs <- counties()
SPs$GEOID <- as.numeric(SPs$GEOID)
list.queen <- poly2nb(SP, queen=TRUE)
W <- nb2listw(list.queen, style="W", zero.policy=TRUE)
err1 <- err2 <- err3 <- err4 <- err5 <- err6 <- err7 <- list()


for (i in 1:13){
	Y <- cases[, list(fips, date, g, g_1)]
	Y[, date := date - i]
	data <- merge(Y, X[date>"2020-03-15"], by=c("fips", "date"), all.x=TRUE)
	data <- pdata.frame(data, index=c("fips","date"), drop.index=TRUE)
	summary(plm(g~residential+g_1, data=data, effect = "twoways", model="within"))
	felm
	#spml(g~grocery, data=data, listw=W, model="within", sptial.error="b", Hess=FALSE, zero.policy=TRUE, na.action = na.omit)
}



for (i in 1:13){
	
	data <- merge(cases[n==i, list(fips, g, g_1)], X, by="fips", all.x=TRUE)
	
	SP <- SPs
	SP@data = merge(SP@data, data, by.x="GEOID", by.y="fips", all.x=TRUE)
	err1[[i]] <- errorsarlm(g ~ grocery, data=SP@data, W, zero.policy=TRUE)
	err2[[i]] <- errorsarlm(g ~ retail, data=SP@data, W, zero.policy=TRUE)
	err3[[i]] <- errorsarlm(g ~ parks, data=SP@data, W, zero.policy=TRUE)
	err4[[i]] <- errorsarlm(g ~ transit, data=SP@data, W, zero.policy=TRUE)
	err5[[i]] <- errorsarlm(g ~ workplaces, data=SP@data, W, zero.policy=TRUE)
	err6[[i]] <- errorsarlm(g ~ residential, data=SP@data, W, zero.policy=TRUE)

	#err_ful[[i]] <- errorsarlm(g ~ mob+residential+highschl+unemp+ln_income+poverty+mig_dom+gender+nonwhite+old+dens, data=SP@data, W, zero.policy=TRUE)
	
}

















