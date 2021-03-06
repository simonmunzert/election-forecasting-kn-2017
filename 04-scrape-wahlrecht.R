### ----------------------------------------------------------
### election forecasting
### simon munzert
### ----------------------------------------------------------


# get packages and functions
source("packages.r")
source("functions.r")


# scrape data
url <- getURL("http://www.wahlrecht.de/umfragen/index.htm")
siteHtml <- htmlTreeParse(url,useInternalNodes=TRUE)
reg <- xpathSApply(siteHtml,"//th/a/@href")

list.institutes <- list()
for(i in 1:length(reg)){
	inst.url <- getURL(paste("http://www.wahlrecht.de/umfragen/",reg[i],sep=""))
	institute <- str_replace(reg[i],".htm","")
	
	if(i != 7){
	siteHtml2 <- htmlTreeParse(inst.url,useInternalNodes=TRUE)
	reg.year <- xpathSApply(siteHtml2,"//a/@href")
	reg.year <- reg.year[str_detect(reg.year,institute[1])]
	reg.year <- reg.year[str_detect(reg.year,"[0-9]+")]
	year.links <- paste("http://www.wahlrecht.de/umfragen/", reg.year,sep="")
	year.links[length(year.links)+1] <- paste("http://www.wahlrecht.de/umfragen/",reg[i],sep="")
	}
	
	if(i == 7){
	year.links <- paste("http://www.wahlrecht.de/umfragen/",reg[i],sep="")
	}
	
	list.year <- list()
	for(j in 1:length(year.links)){
		url.year <- getURL(year.links[j])
		siteHtml3 <- htmlTreeParse(url.year,useInternalNodes=TRUE)
		names1 <- xpathSApply(siteHtml3,"//thead/tr/th[@class='part']",xmlValue)
		names2 <- xpathSApply(siteHtml3,"//thead/tr/th/a",xmlValue)
		names1 <- as.character(c(names1,names2[length(names2)]))
		
		tab <- readHTMLTable(url.year)
		tab <- data.frame(tab[2])
		party.votes <- tab[3:(2+length(names1))]
		colnames(party.votes) <- names1
		colnames(party.votes) <- str_replace(colnames(party.votes),"Ãœ","Ü")
	
		party <- list()
		forecast <- list()
		for(k in 1:ncol(party.votes)){
			party[[k]] <- rep(colnames(party.votes)[k],nrow(party.votes))
			forecast[[k]] <- party.votes[[k]]
		}
		party <- unlist(party)
		forecast <- unlist(forecast)
		poll.date <- rep(tab[[1]], ncol(party.votes))
		if(any(str_detect(colnames(tab),"[Bb]efrag") == TRUE)){
			sample.size <- rep(tab[[which(str_detect(colnames(tab),"[Bb]efrag"))]],ncol(party.votes))}
		else{
			sample.size <- rep(NA,ncol(party.votes))}
		institute <- str_replace(rep(reg[i], length(poll.date)),".htm","")
		list.year[[j]] <- data.frame(poll.date,sample.size,institute,party,forecast)
	}
	list.institutes[[i]] <- do.call("rbind", list.year)
}
	
wahlrecht_raw <- do.call("rbind", list.institutes)	
save(file="data/wahlrecht_raw.RData", list = "wahlrecht_raw")


# tidy data
load("data/wahlrecht_raw.RData")

### Stichprobengröße
sample.size <- str_replace(wahlrecht_raw[[2]], "[.]","") #ist ok das in manchen keine samplesize vorkommt; ist auf Wahlrecht.de nicht immer angegeben
sample.size <- as.numeric(str_extract(sample.size,"[0-9]+"))

### Datum
poll.date <- str_replace_all(wahlrecht_raw[[1]],"[*]","")
test <- str_split(poll.date,"[.]")
year <- sapply(test,function(x)if(length(x)==3){x[[3]]}else{NA}) # this is for the normal date format with day, month, year
year2 <- sapply(test,function(x)if(length(x)==1){str_extract(x[[1]],"[0-9]+")}else{NA}) #gibt kein datum wo nur das Jahr ist?
year3 <- sapply(test,function(x)if(length(x)==2){str_extract(x[[2]],"[0-9]+")}else{NA}) #this is for the date format that is month year, or Wahl 1998
year[is.na(year)] <- year2[is.na(year)]
year[is.na(year)] <- year3[is.na(year)]
year <- as.numeric(year)

#empty fields because of http://www.wahlrecht.de/umfragen/emnid/2004.htm vielleicht den Zeitraum nehmen?

length(which(is.na(year))) #hier sind noch NAs drin; 93 weil u.a. bei emnid manchmal kein genaues Datum angegeben ist http://www.wahlrecht.de/umfragen/emnid/2004.htm 


month <- sapply(test,function(x)if(length(x)==3){x[[2]]}else{NA})
month2 <- sapply(test,function(x)if(length(x)==1){str_extract(x[[1]],"Mai")}else{NA})
month3 <- sapply(test,function(x)if(length(x)==2){x[[1]]}else{NA})
month[is.na(month)] <- month2[is.na(month)]
month[is.na(month)] <- month3[is.na(month)]

length(which(is.na(month))) #ebenso

month <- str_replace(month, "Jan","1")
month <- str_replace(month, "Feb","2")
month <- str_replace(month, "Mrz","3")
month <- str_replace(month, "Apr","4")
month <- str_replace(month, "Mai","5")
month <- str_replace(month, "Jun","6")
month <- str_replace(month, "Jul","7")
month <- str_replace(month, "Aug","8")
month <- str_replace(month, "Sep","9")
month <- str_replace(month, "Okt","10")
month <- str_replace(month, "Nov","11")
month <- str_replace(month, "Dez","12")
month <- as.numeric(month)

day <- sapply(test,function(x)if(length(x)==3){x[[1]]}else{NA})
day <- as.numeric(day)

length(which(is.na(day)))

### Partei
party <- str_replace(wahlrecht_raw$party,"Ã¼","ü") #braucht man glaub ich nicht mehr
party[party == "Grüne"] <- "GRÜNE"
party[party == "Linke.PDS"] <- "LINKE"
party[party == "PDS"] <- "LINKE"
party <- as.factor(party)

### Institut 
institute <- as.factor(wahlrecht_raw$institute)

### Stimmen#####################

#there are 7 NAs which are also in the original data set; this is because on Wahlrecht.de, there was a "Zur telefonischen Umfrage (CATI) von INSA"
#which the standard scraper could not use; therefore, inserting the missings manually: Maybe scrape the whole table later
#http://www.wahlrecht.de/umfragen/weitere-umfragen.htm#btw-insa-2017-02-02

wahlrecht_raw$forecast <- as.character(wahlrecht_raw$forecast)

wahlrecht_raw$forecast[wahlrecht_raw$poll.date == "02.02.2017" & wahlrecht_raw$institute == "insa" & wahlrecht_raw$party == "CDU/CSU"] <- "33.0"
wahlrecht_raw$forecast[wahlrecht_raw$poll.date == "02.02.2017" & wahlrecht_raw$institute == "insa" & wahlrecht_raw$party == "SPD"] <- "27.0"
wahlrecht_raw$forecast[wahlrecht_raw$poll.date == "02.02.2017" & wahlrecht_raw$institute == "insa" & wahlrecht_raw$party == "GRÜNE"] <- "9.0"
wahlrecht_raw$forecast[wahlrecht_raw$poll.date == "02.02.2017" & wahlrecht_raw$institute == "insa" & wahlrecht_raw$party == "LINKE"] <- "9.0"
wahlrecht_raw$forecast[wahlrecht_raw$poll.date == "02.02.2017" & wahlrecht_raw$institute == "insa" & wahlrecht_raw$party == "FDP"] <- "6.0"
wahlrecht_raw$forecast[wahlrecht_raw$poll.date == "02.02.2017" & wahlrecht_raw$institute == "insa" & wahlrecht_raw$party == "PIRATEN"] <- "?" #not asked
wahlrecht_raw$forecast[wahlrecht_raw$poll.date == "02.02.2017" & wahlrecht_raw$institute == "insa" & wahlrecht_raw$party == "AfD"] <- "12.0"
wahlrecht_raw$forecast[wahlrecht_raw$poll.date == "02.02.2017" & wahlrecht_raw$institute == "insa" & wahlrecht_raw$party == "FW"] <- "?" #not asked
wahlrecht_raw$forecast[wahlrecht_raw$poll.date == "02.02.2017" & wahlrecht_raw$institute == "insa" & wahlrecht_raw$party == "Sonstige"] <- "4.0"

wahlrecht_raw$sample.size[wahlrecht_raw$poll.date == "02.02.2017" & wahlrecht_raw$institute == "insa"] <- "1.003" #add the sample size

wahlrecht_raw$forecast <- as.factor(wahlrecht_raw$forecast)

#now we can extract the forecasts
forecast <- str_replace_all(wahlrecht_raw$forecast, ",",".") #replace , with . ; there are 7 NAs which are also in the original data set
forecast <- str_trim(str_replace(forecast, "%","")) #get rid of the percentages
l <- str_detect(forecast,"[Ss]onst") #only 8 true; this is when it says "PIR 2,2 %Sonst. 4,0 %"

#fc <- forecast[l] evtl. später um AFD zu identfizieren; now the error message disappeared
forecast[l] <- str_extract(forecast[l],"Sonst.+") #extracts the Sonstige XX%
forecast[l] <- str_extract(forecast[l],"[0-9]+.+")
forecast[l] <- str_trim(str_replace(forecast[l],"%",""))
forecast <- as.numeric(forecast) #the NAs are the "–" in the data and the ? (1800 NAs) 1,789 "-" + 1 k. A. + 5 ? ...

length(which(is.na(forecast)))

### Wahlergebnisse
l <- wahlrecht_raw[2] == "Bundestagswahl" #iffer Bundestagswahl bei samplesize (= 285 Mal)

### 1998
bw1998 <- data.frame(x=1:13)
bw1998$party <- party[l & !is.na(wahlrecht_raw[[2]]) & year == 1998] #get the parties of the BTW 1998
bw1998$result <- forecast[l & !is.na(wahlrecht_raw[[2]]) & year == 1998] #get the election results
bw1998$institute <- institute[l & !is.na(wahlrecht_raw[[2]]) & year == 1998] #get the institutes
bw1998_2 <- data.frame(x=1:21)
bw1998_2$party <- party[wahlrecht_raw[[1]]=="Wahl 1998"] #because for some the BTW is in the polldate
bw1998_2$result <- forecast[wahlrecht_raw[[1]]=="Wahl 1998"]
bw1998_2$institute <- institute[wahlrecht_raw[[1]]=="Wahl 1998"]
bw1998 <- list(bw1998,bw1998_2)
bw1998 <- do.call("rbind", bw1998)	

### 2002
bw2002 <- data.frame(x=1:49) #why 49 here? length(which(duplicated(bw2005[, -1]))); some doubles?
bw2002$party <- party[l & !is.na(wahlrecht_raw[[2]]) & year == 2002]
bw2002$result <- forecast[l & !is.na(wahlrecht_raw[[2]]) & year == 2002]
bw2002$institute <- institute[l & !is.na(wahlrecht_raw[[2]]) & year == 2002]

### 2005
bw2005 <- data.frame(x=1:54) #before it was 55....why?
bw2005$party <- party[l & !is.na(wahlrecht_raw[[2]]) & year == 2005]
bw2005$result <- forecast[l & !is.na(wahlrecht_raw[[2]]) & year == 2005]
bw2005$institute <- institute[l & !is.na(wahlrecht_raw[[2]]) & year == 2005]

### 2009
bw2009 <- data.frame(x=1:76) #before it was 63?
bw2009$party <- party[l & !is.na(wahlrecht_raw[[2]]) & year == 2009]
bw2009$result <- forecast[l & !is.na(wahlrecht_raw[[2]]) & year == 2009]
bw2009$institute <- institute[l & !is.na(wahlrecht_raw[[2]]) & year == 2009]

### 2013
bw2013 <- data.frame(x=1:93) 
bw2013$party <- party[l & !is.na(wahlrecht_raw[[2]]) & year == 2013]
bw2013$result <- forecast[l & !is.na(wahlrecht_raw[[2]]) & year == 2013]
bw2013$institute <- institute[l & !is.na(wahlrecht_raw[[2]]) & year == 2013]

### Genaue Datumsangaben
d <- rep(NA,length(forecast))
date <- as.Date(rep(NA,length(forecast)))
d[!is.na(day)] <- paste(day[!is.na(day)],month[!is.na(day)],year[!is.na(day)],sep="/")
date[!is.na(day)] <- as.Date(d[!is.na(day)],"%d/%m/%Y")

### Zuordnen der Wahlergebnisse für eindeutige Datumsangaben
election <- rep(NA,length(forecast))
actual.votes <- rep(NA,length(forecast))
bws <- list(bw1998,bw2002,bw2005,bw2009, bw2013)
bw.date <- as.Date(c("27/09/1998","22/09/2002", "18/09/2005", "27/09/2009", "22/09/2013"),"%d/%m/%Y")
for(z in 1:5){
	for(i in 1:length(levels(institute))){
		for(j in 1:length(levels(party))){
			if(z == 1){
			l <- date < bw.date[z] & institute == levels(institute)[i] & party == levels(party)[j]
			}
			else{
			l <- (date < bw.date[z] & date > bw.date[z-1]) & institute == levels(institute)[i] & party == levels(party)[j]
			lfut <- date > bw.date[z]
			lfut[is.na(lfut)] <- FALSE
			}
			l[is.na(l)] <- FALSE
			l2 <- bws[[z]]$institute == levels(institute)[i] & bws[[z]]$party == levels(party)[j]
			if(length(bws[[z]]$result[l2]) > 0){
				actual.votes[l] <- rep(bws[[z]]$result[l2][1], length(l[l]))
				election[l] <- rep(as.numeric(format(bw.date[z], "%Y")),length(l[l])) 
				if(z == 4){
				actual.votes[lfut] <- rep("AUSSTEHEND", length(lfut[lfut]))
				election[lfut] <- rep(2013,length(lfut[lfut])) 
				}
			}
			else{"WARNUNG"}
		}
	}
}	

### Zuordnen der Wahlergebnisse für nicht-eindeutige Datumsangaben

### Monat - Jahr - Datumsangaben
d2 <- rep(NA,length(forecast))
date2 <- as.Date(rep(NA,length(forecast)))
d2[is.na(day) & !is.na(month)] <- paste(month[is.na(day) & !is.na(month)],year[is.na(day) & !is.na(month)],sep="/")
date2 <- as.yearmon(d2,"%m/%Y")

bw.date <- as.yearmon(c("09/1998","09/2002", "09/2005", "09/2009", "09/2013"),"%m/%Y")
for(z in 1:5){
	for(i in 1:length(levels(institute))){
		for(j in 1:length(levels(party))){
			if(z == 1){
			l <- date2 <= bw.date[z] & institute == levels(institute)[i] & party == levels(party)[j]
			}
			else{
			l <- (date2 <= bw.date[z] & date2 > bw.date[z-1]) & institute == levels(institute)[i] & party == levels(party)[j]
			lfut <- date2 >= bw.date[z]
			lfut[is.na(lfut)] <- FALSE
			}
			l[is.na(l)] <- FALSE
			l2 <- bws[[z]]$institute == levels(institute)[i] & bws[[z]]$party == levels(party)[j]
			#if(length(bws[[z]]$result[l2]) == 1 | bws[[z]]$result[l2][1] == bws[[z]]$result[l2][2] & length(bws[[z]]$result[l2]) > 0){
			if(length(bws[[z]]$result[l2]) > 0){
				actual.votes[l] <- rep(bws[[z]]$result[l2][1], length(l[l]))
				election[l] <- rep(as.numeric(format(bw.date[z], "%Y")),length(l[l])) 
				if(z == 4){
				actual.votes[lfut] <- rep("AUSSTEHEND", length(lfut[lfut]))
				election[lfut] <- rep(2013,length(lfut[lfut])) 
				}
			}
			else{"WARNUNG"}
		}
	}
}

actual.votes[actual.votes == "AUSSTEHEND"] <- 999
actual.votes <- as.numeric(actual.votes)

df.forecast <- data.frame(actual.votes, election, party, forecast, sample.size, institute, day, year, month,date)

bw.date <- as.Date(c("27/09/1998","22/09/2002", "18/09/2005", "27/09/2009", "22/09/2013"),"%d/%m/%Y")
l <- list()
for(i in 1:length(bw.date)){
		l[[i]] <- which(df.forecast$date == bw.date[i])
}
l <- unlist(l)
df.forecast <- df.forecast[-l,]

### AFD 
forecast <- str_replace_all(wahlrecht_raw$forecast, ",",".")
forecast <- str_trim(str_replace(forecast, "%",""))
l <- str_detect(forecast,"[Ss]onst")
fc.afd <- forecast[l] 
fc.afd <- as.numeric(str_extract(str_extract(fc.afd,"AfD [0-9.]+"),"[0-9.]+")) #Sonstige = Afd?
sample.size.afd <- sample.size[l]
institute.afd <- institute[l]
party.afd <- rep("AFD",length(fc.afd))
actual.votes.afd <- rep(999, length(fc.afd))
election.afd <- rep(2013,length(fc.afd))
day.afd <- day[l]
month.afd <- month[l]
year.afd <- year[l]
date.afd <- as.Date(paste(day.afd,month.afd,year.afd,sep="/"),"%d/%m/%Y")
df.afd <- data.frame(actual.votes.afd, election.afd, party.afd, fc.afd, sample.size.afd, institute.afd, day.afd,year.afd,month.afd,date.afd)
df.afd <- df.afd[!is.na(df.afd$fc.afd),]

### AFD an restlichen Datensatz anhängen
df.forecast$party <- as.character(df.forecast$party)
colnames(df.afd) <- colnames(df.forecast)

data.forecast <- list(df.forecast,df.afd)
data.forecast <- do.call("rbind", data.forecast)	
data.forecast$party <- as.factor(data.forecast$party)

wahlrecht_df <- data.forecast
save(wahlrecht_df, file = "data/wahlrecht_df.RData")



