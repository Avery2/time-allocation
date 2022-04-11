
library(dplyr) #for between function

## PRE-PROCESS DATA ##

#-READ IN DATA
fpath = ...
fpath_data = paste0(fpath, "data/")
fpath_aux = paste0(fpath_data, "AUXIL/")
tapraw <- read.csv(paste0(fpath_data, "TAPOriginal.csv"))
tapo <- tapraw
tapo$SOC_CODE <- as.character(tapo$SOC_CODE)
tapo[which(tapo$SOC_CODE %in% c("EFEMN", "EFEWM")),]$SOC_CODE <- "EFE" #relabel efe soc code
tapo[which(tapo$SOC_CODE == "kips"),]$SOC_CODE <- "KIPS" #relabel kips soc code
tapo <- subset(tapo, SOC_CODE %in% c("CAMAN", "EFE", "KIPS", "MADUR", "MEKRA", "SHIMA", "YEKWA", "YEKVS", "YUKPA") & AGE_SEX %in% c("MA", "FA")) #grab adults from included societies
tapo[which(tapo$STDACT %in% c(" X", "X-")),]$STDACT = "X" #fix some typos
table(tapo$SOC_CODE, tapo$STDACT) #check typos
tapo$DATE = as.Date(tapo$DATE, format = "%d-%b-%y") #fix date format
tapo <- subset(tapo, !(SOC_CODE == "CAMAN" & substr(SOURCEACT,1,1) == "L")) #exclude CAMAN who live out of the village


#-IMPUTATION FUNCTIONS

#function to compute window around time of day
hourWindow = function(inTime, increment) {

	# the hour part comes from dividing the time by 100 and effectively removing the last two digits, and then multiplying by 100
	hourVar = (floor(inTime / 100)) * 100;
	# the minute part comes from subtracting the time from the hour part
	minVar = inTime - hourVar;

	# get the upper/lower bound by adding the size of (half of) the bin (ie. 1 hour bins -> +/- 30 mins)
	newMinHi = minVar + increment;
	newMinLo = minVar - increment;
	# translate that into an hour part by adding the new hour part to the old hour part
	hourVarHi = hourVar + (floor(newMinHi/60) * 100);
	hourVarLo = hourVar + (floor(newMinLo/60) * 100);
	# translate that also into a minute part by getting the new minutes mod 60
	minVarHi = newMinHi %% 60;
	minVarLo = newMinLo %% 60;

	# return the bound time
	return(c(hourVarLo + minVarLo, hourVarHi + minVarHi))

}

#function to compute window around date
dateWindow = function(inDate, increment) {

	dateLo = as.Date(inDate, format = "%d-%b-%y") - increment
	dateHi = as.Date(inDate, format = "%d-%b-%y") + increment
	return(c(dateLo, dateHi))
	
}

#function to resample within date/hour window and replace, with iterative window opening when needed
resampleFunction <- function(resampleFrom, resampleTo, monthwindow = 2, hourwindow = 3, incmonth = 1, inchour = 3) {
	sapply(1:nrow(resampleTo), function(i) {
		subsamplei = data.frame(); monthwins <- hourwins <- rep(NA, nrow(resampleTo))
		while(nrow(subsamplei) == 0) { subsamplei = subset(resampleFrom, between(DATE, dateWindow(resampleTo[i,]$DATE, (30/2)*monthwindow)[1], dateWindow(resampleTo[i,]$DATE, (30/2)*monthwindow)[2]) & between(HOUR, hourWindow(resampleTo[i,]$HOUR, (60/2)*hourwindow)[1], hourWindow(resampleTo[i,]$HOUR, (60/2)*hourwindow)[2])); monthwindow = monthwindow+incmonth; hourwindow = hourwindow+inchour; }
		return(sample(subsamplei$STDACT,1))
		})
}

#function to get window values used for resampling
resampleWindowFunction <- function(resampleFrom, resampleTo, monthwindow = 2, hourwindow = 3, incmonth = 1, inchour = 3) {
	sapply(1:nrow(resampleTo), function(i) {
		subsamplei = data.frame(); monthwins <- hourwins <- rep(NA, nrow(resampleTo))
		while(nrow(subsamplei) == 0) { subsamplei = subset(resampleFrom, between(DATE, dateWindow(resampleTo[i,]$DATE, (30/2)*monthwindow)[1], dateWindow(resampleTo[i,]$DATE, (30/2)*monthwindow)[2]) & between(HOUR, hourWindow(resampleTo[i,]$HOUR, (60/2)*hourwindow)[1], hourWindow(resampleTo[i,]$HOUR, (60/2)*hourwindow)[2])); monthwindow = monthwindow+incmonth; hourwindow = hourwindow+inchour; }
		return(c(monthwindow, hourwindow))
		})
}



#-IMPUTATION for MEKRA

#--Extract MEKRA
mekra <- subset(tapo, SOC_CODE == "MEKRA" & HOUR >= 600 & HOUR <= 2000) #extract data, exclude nighttime obs (500, 2300)
mekranot <- read.csv(paste0(fpath_aux, "MEKRANOT.csv")) #read in original mekranoti data with away/trek info
mekraAT = merge(mekra, mekranot[,c("RECORD", "AWAY", "TREKKING")], by.x = "OBSNUM", by.y = "RECORD") #merge away/trek info into extracted data frame
mekraAT = subset(mekraAT, !(AWAY == "V")) #exclude incidentally collected people
mekraAT$AWAY <- as.character(mekraAT$AWAY); #change data type
mekraAT$TREKKING <- as.character(mekraAT$TREKKING); mekraAT$TREKKING[is.na(mekraAT$TREKKING)] = ""; mekraAT$TREKKING[mekraAT$TREKKING == "TRUE"] = "T" #fix data type
#table(mekraAT$AWAY, mekraAT$TREKKING) #check counts of data

#subset(mekraAT, AWAY == "" & TREKKING == "") #anthropologist and target both in town; leave as is
#subset(mekraAT, AWAY == "A" & TREKKING == "") #anthropologist in town, target on long trip; coded already, leave as is
#subset(mekraAT, AWAY == "" & TREKKING == "T") #anthropologist accompanying on long trip; subset further:
	#subset(mekraAT, SOURCEACT == "XV") #all of these are AWAY == "" & TREKKING == "T", anthropologist accompanying on long trip, target back in village; impute with same gender/month/time from NOTAWAY/NOTTREK
	#subset(mekraAT, SOURCEACT == "XT") #all of these are AWAY == "" & TREKKING == "T", anthropologist accompanying on long trip, target ?; leave as is
	#subset(mekraAT, AWAY == "" & TREKKING == "T" & !(SOURCEACT %in% c("XV", "XT"))) #coded already, leave as is
#subset(mekraAT, AWAY == "A" & TREKKING == "T") #anthropologist accompanying on long trip, target away for other reason; coded already, leave as is

#--Resample MEKRA
#get dfs to sample for and from
mekResampleFrom = subset(mekraAT, AWAY == "" & TREKKING == "") #mekra data to draw samples from
mekXV = subset(mekraAT, SOURCEACT == "XV") #grab subset of mekra with XV
#sample from window +- 15 days, +- 6 hours
mekXVresampled = resampleFunction(mekResampleFrom, mekXV) #mekXVresampledWindows = resampleWindowFunction(mekResampleFrom, mekXV)
#put resamples back into df
mekraAT[which(mekraAT$SOURCEACT == "XV"),]$STDACT = mekXVresampled #put resamples back into mekraAT
tapo <- subset(tapo, !(SOC_CODE %in% "MEKRA")) #remove from main data frame
tapo = rbind(tapo, mekraAT[,setdiff(names(mekraAT), c("AWAY","TREKKING"))]) #put mekra back into main tap df


#-IMPUTATION for YEKWA

#--Extract YEKWA
yekwa <- subset(tapo, SOC_CODE == "YEKWA") #extract data
yekvs <- subset(tapo, SOC_CODE == "YEKVS") #extract data
yekwa[which(is.na(yekwa$DATE)),]$DATE = yekwa[which(is.na(yekwa$DATE))-1,]$DATE #impute lone missing date with closest date

#--Resample MEKRA
#get df to sample for
yek8000 = subset(yekwa, SOURCEACT == "8000") #grab subset of yekwa away
#sample from window +- 15 days, +- 6 hours
yek8000resampled = resampleFunction(yekvs, yek8000) #yek8000resampledWindows = resampleWindowFunction(yekvs, yek8000)
#put resamples back into df
yekwa[which(yekwa$SOURCEACT == "8000"),]$STDACT = yek8000resampled #put resamples back into yekwa
tapo <- subset(tapo, !(SOC_CODE %in% c("YEKVS","YEKWA"))) #remove from main data frame
tapo = rbind(tapo, yekwa) #put mekra back into main tap df


#-CHECK STDACTS IN ALL SOCIETIES
#round(prop.table(table(tapo$SOC_CODE, tapo$STDACT),1),2)


#write.csv(tapo, paste0(fpath_data, "TAPpostproc.csv"))
