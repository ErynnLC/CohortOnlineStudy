#norming study
#messed with by: Arthur Leung
#created: 13/04/2021

#///////////////////////////////////////////////////////////////////////////////
#before we start
#Clear environment
rm(list = ls()) 

#set working directory
setwd("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources")

# load packages
library(readr)
library(tidyverse)
library(dplyr)

#///////////////////////////////////////////////////////////////////////////////
# loading, preparing and reading data

# load data into data frame
# skip = 1 will skip the first row and make the second row the header
# [-c(1),] removes the second row (do not want them added in the middle of the data set when we combine)

Ax1 <- read_csv("Online Norming Study_Ax1.csv", skip = 1)[-c(1),] 
Ax2 <- read_csv("Online Norming Study_Ax2.csv", skip = 1)[-c(1),]
Ax3 <- read_csv("Online Norming Study_Ax3.csv", skip = 1)[-c(1),]

#remove the spaces in column names, substitute for "."
names(Ax1) <- make.names(names(Ax1), unique = TRUE) 
names(Ax2) <- make.names(names(Ax2), unique = TRUE)
names(Ax3) <- make.names(names(Ax3), unique = TRUE)

#rename column with ID as 'ParticipantID', this is needed to combine/merge data frames
Ax1 <- rename(Ax1, ParticipantID=Please.enter.your.Prolific.ID.)
Ax2 <- rename(Ax2, ParticipantID=Please.enter.your.Prolific.ID)
Ax3 <- rename(Ax3, ParticipantID=Please.enter.your.Prolific.ID)

#get rid of unneeded columns and keep relevant columns
Ax1_tidy <- Ax1 %>% select(6:9,18:19,22:27,31,36:41,44,46,48,50,52:281,292,293)
Ax2_tidy <- Ax2 %>% select (6:9,18:143)
Ax3_tidy <- Ax3 %>% select (6:9,18:126)

#load/read in cognitive games
#VMACR1A <- read_csv("dataforscript/Online Norming Study_Ax1_Prolific_April 20, 2021_09.30.csv")
VMACR3A
BART <- read_csv("BART_Norm.csv", skip = 1)
SST <- read.csv("SST_Norm.csv")

#select relevant columns AKA tidy
VMACR_1A <- VMACR-v1 %>% select()
BART <- BART %>% select(1,8:27)
SST <- SST %>% select(1,8:20)

# remove '' from Prolific ID using gsub 
# here we have asked gsub to look for all ' and replace with blank
BART$ParticipantID <- gsub("'",'',BART$ParticipantID)
SST$ParticipantID <- gsub("'",'',SST$ParticipantID) 

#create play number variable that counts the number of time the games appear for each participant
Ax1$PlayNumber <- ave(Ax1$ParticipantID, Ax1$ParticipantID,  FUN = seq_along)
Ax2$PlayNumber <- ave(Ax2$ParticipantID, Ax2$ParticipantID,  FUN = seq_along)
Ax3$PlayNumber <- ave(Ax3$ParticipantID, Ax3$ParticipantID,  FUN = seq_along)
BART$PlayNumber <- ave(BART$ParticipantID, BART$ParticipantID,  FUN = seq_along)
SST$PlayNumber <- ave(SST$ParticipantID, SST$ParticipantID,  FUN = seq_along)

#merge BART & SST by two columns: 'ParticipantID' and 'PlayNumber'
BART_SST <- merge(BART,SST, by = c('ParticipantID','PlayNumber'), all= TRUE)

#merge Qualtrics data 2 at a time
QualtricsData <- merge(Ax1,Ax2, by = c('ParticipantID','PlayNumber'), all= TRUE)
QualtricsData <- merge(QualtricsData,Ax3, by = c('ParticipantID','PlayNumber'), all= TRUE)

#now merge qualtrics data with games
Masterdatabase <- merge(QualtricsData,BART_SST, by = c('ParticipantID','PlayNumber'), all= TRUE) 

BART$ParticipantID <- gsub("'",'',BART$ParticipantID)
SST$ParticipantID <- gsub("'",'',SST$ParticipantID) 

#refining DBT raw data
####################################################################################################################
#read in DBT raw data
DBT <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/DBT norm raw.csv")
#Select the two only relevant columns 'TotalScore' & 'Name'
DBT <- DBT %>% select(3,11)
#specify where to look "DBT" and which column
View(DBT)
names(DBT) <- gsub("\\W", "", names(DBT))
#rename Name to ParticipantID
View(DBT)
DBT <- rename(DBT, ParticipantID=Name)
#remove 's from ParticipantID column
DBT$ParticipantID <- gsub("'",'',DBT$ParticipantID)
#select rows based on the ParticipantID column which have values that either start with 5 or 6
#NOTICE might include IDs you may not want
DBT <- DBT %>% 
  filter(str_detect(ParticipantID, "^5|^6"))
#check data frame is properly refined
View(DBT)

################################################################################
#refining CST raw data
#read in the category switch raw data
CST <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/CST norm raw.csv")
#confirm that the data set is correct
View(CST)
#rename column 4 subjectid to ParticipantID
CST <- rename(CST,ParticipantID=subjectid)
#keep relevant columns: 4 ParticipantID, 9 keyAssignmentgroup,10 congruentKeys, 15-22
CST <- CST %>% select(4,9:10,15:22)
#create play number variable that counts the number of time the games appear for each participant
CST$PlayNumber <- ave(CST$ParticipantID, CST$ParticipantID,  FUN = seq_along)
################################################################################

#configuring v-MAC Data
#import raw data for 3A version of V-MAC and skip first row
VMAC3a <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/dataforscript/Online Norming Study_Ax1_Prolific - VMAC C3_April 20, 2021_09.30.csv", skip = 1)[-c(1),] 
#replace spaces with full stops
names(VMAC3a) <- make.names(names(VMAC3a), unique = TRUE) 
#rename ProlificID into ParticipantID
VMAC3a <- rename(VMAC3a, ParticipantID=Please.enter.your.Prolific.ID.)
#select relevant columns
VMAC3a <- VMAC3a %>% select(18)
#create new dataframe titled "vmac.colour" for a column filled with entries of 3A
#use to rep function to enter 114 rows worth of 3A
vmac.colour<-rep(c("3A"),times=114)
#push it into a dataframe titled
vmac.colour<-data.frame(vmac.colour)
#check, if done properly there should only be one column titled "vmac.colour
vmac.colour
#use cbind function to add the two dataframes together into a new dataframe titled "vmac3a"
vmac3a <-cbind(VMAC3a, vmac.colour)
#check the new dataframe, there should now be two columns: one of them ParticipantID and one named vmac.colour
#indication of participants who done the 3a version of the vmac now complete
#now load in the full raw data of the vmac
vmacfull <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/vmacfull.csv")
#replace the apostrophes around the column headers
names(vmacfull) <- gsub("\\W", "", names(vmacfull))
#replacing spaces with periods are not necessary here and ParticipantID is already named correctly
#replace spaces with full stops
#select relevant columns
vmacfull <- vmacfull %>% select(1,5:73)
#now combine 3a vmac participants with full raw vmac data by ParticipantIDs
vmacfull <- merge(vmacfull,vmac3a, by = "ParticipantID", all = TRUE)
vmacfull <- merge(vmacfull, vmac3a, by = "ParticipantID", all.x = TRUE) 
vmacfull <- merge(vmacfull, vmac3a, by = "ParticipantID", all.y = TRUE)  
vmacfull <- merge(vmacfull, vmac3a, by = "ParticipantID", all.x = TRUE, all.y = TRUE)


vmacfull['vmac.colour'] <- NA

vmacfull$vmac.colour 

<- c("m", "m", "f", "f", "m")

vmacfull <- merge(vmacfull,vmac3a, by = "ParticipantID", all = TRUE)
Example <- merge(df1, df2, by = "col1", all = TRUE)

dplyr::bind_rows(vmacfull, vmac3a)

##############################################################################################################################
#standardising DDT raw data
#read in the DDT raw data

# export to csv
write.csv(Ax1_tidy, "Ax1_Data_Tidy.csv", row.names = FALSE) #this removes built in R row numbers 
write.csv(Ax2_tidy, "Ax2_Data_Tidy.csv", row.names = FALSE)
write.csv(Ax3_tidy, "Ax3_Data_Tidy.csv", row.names = FALSE)

# export to csv
write.csv(Fulldatabase, "NormStudyNIH_Data.csv", row.names = FALSE)

#clear console and environment
#ctrl + l + 
rm(list=ls())