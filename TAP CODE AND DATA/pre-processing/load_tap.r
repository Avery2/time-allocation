
#Code for Time Allocation Project

#load in (processed) data
library(dplyr)
fpath_data = paste0(fpath, "data/")
fpath_aux = paste0(fpath_data, "AUXIL/")
tapl <- read.csv(paste0(fpath_data, "TAPpostproc.csv")) #read in processed data (after resampling)
taps <- read.csv(paste0(fpath_aux, "TAPsocinfo.csv")) #read in society-level info
workdefs <- read.csv(paste0(fpath_data, "TAPworkdefEn.csv")) #read in definitions of work/commercial activities

#merge into main data frame
tap <- merge(tapl, taps, by="SOC_CODE") #incorporate society-level info into main data frame
tap[which(tap$SOC_CODE == "EFE")[which(sapply(subset(tap, SOC_CODE == "EFE")$SOURCEACT, grepl, x = "03|04|18"))],]$STDACT <- "FX" #change EFE travel to work
tap <- merge(tap, workdefs, by="STDACT") #incorporate work characterizations into main data frame
tap <- droplevels(tap)
SOCS = unique(tap$SOC_CODE) #construct list of included society codes

#merge age into main data frame
tap_age_list <- lapply(1:length(SOCS), function(soc) {
												tapSOC = subset(tap, SOC_CODE == SOCS[soc])
												tapSOC = droplevels(tapSOC)
												ageSOC = read.csv(paste0(fpath_aux, "Age", SOCS[soc], "_H.csv")); names(ageSOC) <- c("ID", "AGE", "HOUSEHOLD"); ageSOC$HOUSEHOLD = as.factor(ageSOC$HOUSEHOLD)
												tapa = merge(tapSOC, ageSOC, by = "ID", all.x = T)
												return(tapa)
												})
tap <- do.call("rbind", tap_age_list)
tap <- subset(tap, AGE >= 14) #exclude people younger than 15 and missing age data; 14 is min bound since some people turned 15 during year of record

#transform into individual-level data
tap_id_watt = aggregate(tap[,names(workdefs)[-1]], tap[,c("SOC_CODE", "ID", "AGE", "AGE_SEX", "HOUSEHOLD")], FUN = mean) #compute proportion of obs spent on workdefs for each individual, plus their attributes
names(tap_id_watt)[which(names(tap_id_watt) == "AGE_SEX")] = "Female"; tap_id_watt$Female = (tap_id_watt$Female == "FA") #rename heading
tap_id_count = aggregate(tap[,"ID"], tap[,c("SOC_CODE", "ID")], FUN = length); names(tap_id_count)[3] = "Count" #count number of obs per person
tap_id = merge(tap_id_watt, tap_id_count, by = c("SOC_CODE", "ID")) #merge table of work amounts with id counts
tap_id = merge(tap_id, taps, by = "SOC_CODE") #merge individual attributes with society info
tap_id$Total_Work = tap_id$WORK_W*tap_id$DayLength*7 #total weekly work hours
tap_id$Total_WorkX = tap_id$WORK_WX*tap_id$DayLength*7 #total weekly work hours including other as work
tap_id$Prop_Work_Comm = tap_id$COMM_C/tap_id$WORK_W #fraction of work that is commercial (excluding information acquisition/education) [ENC]
tap_id$Prop_Work_CommE = tap_id$COMM_CE/tap_id$WORK_W #fraction of work that is commercial (including information acquisition/education) [EC]
tap_id$Prop_WorkX_Comm = tap_id$COMM_C/tap_id$WORK_WX #fraction of work (including other activities) that is commercial (excluding information acquisition/education and excluding other activities)
tap_id$Prop_WorkX_CommE = tap_id$COMM_CE/tap_id$WORK_WX #fraction of work (including other activities) that is commercial (including information acquisition/education and excluding other activities)
tap_id$Prop_WorkX_CommX = tap_id$COMM_CX/tap_id$WORK_WX #fraction of work (including other activities) that is commercial (excluding information acquisition/education and including other activities)
tap_id$Prop_WorkX_CommXE = tap_id$COMM_CXE/tap_id$WORK_WX #fraction of work (including other activities) that is commercial (including information acquisition/education and including other activities)
tap_id$Total_Energy = (tap_id$Energy*tap_id$DayLength/24) + (1*(24-tap_id$DayLength)/24) #energy expenditure; remainder of day assumed to expend 1 MET
tap_id = tap_id[,c("SOC_CODE", "ID", "AGE", "Female", "Count", "HOUSEHOLD", "Total_Work", "Total_WorkX", "Prop_Work_Comm", "Prop_Work_CommE", "Prop_WorkX_Comm", "Prop_WorkX_CommE", "Prop_WorkX_CommX", "Prop_WorkX_CommXE", "Total_Energy")] #grab relevant columns

#transform into society-level mean data
tap_agg = aggregate(subset(tap, between(AGE, 14, 64))[,names(workdefs)[-1]], subset(tap, between(AGE, 14, 64))[,c("SOC_CODE", "AGE_SEX")], FUN = mean) #select people within age range
names(tap_agg)[which(names(tap_agg) == "AGE_SEX")] = "Female"; tap_agg$Female = (tap_agg$Female == "FA") #rename heading
tap_agg = merge(tap_agg, taps, by = "SOC_CODE") #merge societal attributes with society info
tap_agg$Total_Work = tap_agg$WORK_W*tap_agg$DayLength*7 #total weekly work hours
tap_agg$Total_WorkX = tap_agg$WORK_WX*tap_agg$DayLength*7 #total weekly work hours including other as work
tap_agg$Prop_Work_Comm = tap_agg$COMM_C/tap_agg$WORK_W #fraction of work that is commercial (excluding information acquisition/education) [ENC]
tap_agg$Prop_Work_CommE = tap_agg$COMM_CE/tap_agg$WORK_W #fraction of work that is commercial (including information acquisition/education) [EC]
tap_agg$Prop_WorkX_Comm = tap_agg$COMM_C/tap_agg$WORK_WX #fraction of work (including other activities) that is commercial (excluding information acquisition/education and excluding other activities)
tap_agg$Prop_WorkX_CommE = tap_agg$COMM_CE/tap_agg$WORK_WX #fraction of work (including other activities) that is commercial (including information acquisition/education and excluding other activities)
tap_agg$Prop_WorkX_CommX = tap_agg$COMM_CX/tap_agg$WORK_WX #fraction of work (including other activities) that is commercial (excluding information acquisition/education and including other activities)
tap_agg$Prop_WorkX_CommXE = tap_agg$COMM_CXE/tap_agg$WORK_WX #fraction of work (including other activities) that is commercial (including information acquisition/education and including other activities)
tap_agg$Total_Energy = (tap_agg$Energy*tap_agg$DayLength/24) + (1*(24-tap_agg$DayLength)/24) #energy expenditure; remainder of day assumed to expend 1 MET
tap_agg = tap_agg[,c("SOC_CODE", "Female", "Total_Work", "Total_WorkX", "Prop_Work_Comm", "Prop_Work_CommE", "Prop_WorkX_Comm", "Prop_WorkX_CommE", "Prop_WorkX_CommX", "Prop_WorkX_CommXE", "Total_Energy")] #grab relevant columns

#transform into household-level mean data
tap_hhmf_watt = aggregate(tap[,names(workdefs)[-1]], tap[,c("HOUSEHOLD", "SOC_CODE", "AGE_SEX")], FUN = mean)
names(tap_hhmf_watt)[which(names(tap_hhmf_watt) == "AGE_SEX")] = "Female"; tap_hhmf_watt$Female = (tap_hhmf_watt$Female == "FA") #rename heading
tap_hhmf_count = aggregate(tap[,"HOUSEHOLD"], tap[,c("SOC_CODE", "HOUSEHOLD")], FUN = length); names(tap_hhmf_count)[3] = "Count" #count number of obs per hhmf
tap_hhmf_size = aggregate(tap[,"ID"], tap[,c("SOC_CODE", "HOUSEHOLD")], FUN = function(x) length(unique(x))); names(tap_hhmf_size)[3] = "Size" #number of adults in hhmf
tap_hhmf_age = aggregate(tap[,"AGE"], tap[,c("SOC_CODE", "HOUSEHOLD")], FUN = mean); names(tap_hhmf_age)[3] = "Age" #mean age of adults in hhmf
tap_hhmf = merge(tap_hhmf_watt, tap_hhmf_count, by = c("SOC_CODE", "HOUSEHOLD")) #merge table of work amounts with hhmf counts
tap_hhmf = merge(tap_hhmf, tap_hhmf_size, by = c("SOC_CODE", "HOUSEHOLD")) #merge table of work amounts with hhmf size
tap_hhmf = merge(tap_hhmf, tap_hhmf_age, by = c("SOC_CODE", "HOUSEHOLD")) #merge table of work amounts with hhmf age
tap_hhmf = merge(tap_hhmf, taps, by = "SOC_CODE") #merge household attributes with society info
tap_hhmf$Total_Work = tap_hhmf$WORK_W*tap_hhmf$DayLength*7 #total weekly work hours
tap_hhmf$Total_WorkX = tap_hhmf$WORK_WX*tap_hhmf$DayLength*7 #total weekly work hours including other as work
tap_hhmf$Prop_Work_Comm = tap_hhmf$COMM_C/tap_hhmf$WORK_W #fraction of work that is commercial (excluding information acquisition/education) [ENC]
tap_hhmf$Prop_Work_CommE = tap_hhmf$COMM_CE/tap_hhmf$WORK_W #fraction of work that is commercial (including information acquisition/education) [EC]
tap_hhmf$Prop_WorkX_Comm = tap_hhmf$COMM_C/tap_hhmf$WORK_WX #fraction of work (including other activities) that is commercial (excluding information acquisition/education and excluding other activities)
tap_hhmf$Prop_WorkX_CommE = tap_hhmf$COMM_CE/tap_hhmf$WORK_WX #fraction of work (including other activities) that is commercial (including information acquisition/education and excluding other activities)
tap_hhmf$Prop_WorkX_CommX = tap_hhmf$COMM_CX/tap_hhmf$WORK_WX #fraction of work (including other activities) that is commercial (excluding information acquisition/education and including other activities)
tap_hhmf$Prop_WorkX_CommXE = tap_hhmf$COMM_CXE/tap_hhmf$WORK_WX #fraction of work (including other activities) that is commercial (including information acquisition/education and including other activities)
tap_hhmf = tap_hhmf[,c("SOC_CODE", "HOUSEHOLD", "Size", "Age", "Female", "Count", "Total_Work", "Total_WorkX", "Prop_Work_Comm", "Prop_Work_CommE", "Prop_WorkX_Comm", "Prop_WorkX_CommE", "Prop_WorkX_CommX", "Prop_WorkX_CommXE")] #grab relevant columns

