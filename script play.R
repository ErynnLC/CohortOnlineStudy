#norming study
#messed with by: Arthur Leung
#created: 13/04/2021

rm(list = ls()) #before starting, clear your environment
#ctrl + l as well

setwd("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources") #set working directory

library(readr)
library(tidyverse) #load packages for data wrangling
library(dplyr)

#loading & reading data

#handling qualtrics data
ax1 <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/dataforscript/Online Norming Study_Ax1_Prolific_April 20, 2021_09.30.csv", skip = 1)[-c(1),] #load qualtrics data into data frame, skip = 1 will skip the first row and make the second row the header
ax2 <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/dataforscript/Online Norming Study_Ax2_Prolific_April 20, 2021_09.31.csv", skip = 1)[-c(1),] 
ax3 <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/dataforscript/Online Norming Study_Ax3_Prolific_April 20, 2021_09.32.csv", skip = 1)[-c(1),] #[-c(1),] removes the second row (do not want them added in the middle of the data set when we combine)
names(ax1) <- make.names(names(ax1), unique = TRUE) 
names(ax2) <- make.names(names(ax2), unique = TRUE) #replace spaces in column headers with periods
names(ax3) <- make.names(names(ax3), unique = TRUE)
ax1 <- rename(ax1, ParticipantID=Please.enter.your.Prolific.ID.)
ax2 <- rename(ax2, ParticipantID=Please.enter.your.Prolific.ID) #rename column indicating ID as ParticipantID
ax3 <- rename(ax3, ParticipantID=Please.enter.your.Prolific.ID)
ax1 <- ax1 %>% select(6:9,18:19,22:27,31,36:41,44,46,48,50,52:281,292,293)
ax2 <- ax2 %>% select (6:9,18:143) #select relevant columns
ax3 <- ax3 %>% select (6:9,18:126)
ax1 <- ax1 %>% 
  filter(str_detect(ParticipantID, "^5|^6")) #select rows based on the ParticipantID column which have values that either start with 5 or 6 for ax1
ax2 <- ax2 %>% 
  filter(str_detect(ParticipantID, "^5|^6")) #select rows based on the ParticipantID column which have values that either start with 5 or 6 for ax2
ax3 <- ax3 %>% 
  filter(str_detect(ParticipantID, "^5|^6")) #select rows based on the ParticipantID column which have values that either start with 5 or 6 for ax3
ax1$PlayNumber <- ave(ax1$ParticipantID, ax1$ParticipantID,  FUN = seq_along)
ax2$PlayNumber <- ave(ax2$ParticipantID, ax2$ParticipantID,  FUN = seq_along) #create play number variable that counts the number of times the game appears for each participant
ax3$PlayNumber <- ave(ax3$ParticipantID, ax3$ParticipantID,  FUN = seq_along)
ax1ax2 <- merge(ax1,ax2, by = c('ParticipantID','PlayNumber'), all= TRUE) #merge qualtrics data day 1 and 2
qualtricsdata <- merge(ax1ax2,ax3, by = c('ParticipantID','PlayNumber'), all= TRUE) #merge qualtrics data days 1 & 2 with 3 to get all qualtrics data
qualtricsdata = qualtricsdata[-1,] #Remove first ParticipantID as it does long belong to this study

#read in cognitive games

#configuring v-mac data
VMAC3a <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/dataforscript/Online Norming Study_Ax1_Prolific - VMAC C3_April 20, 2021_09.30.csv", skip = 1)[-c(1),] #import raw data for 3a version of V-MAC and skip first row
names(VMAC3a) <- make.names(names(VMAC3a), unique = TRUE) #replace spaces with full stops
VMAC3a <- rename(VMAC3a, ParticipantID=Please.enter.your.Prolific.ID.) #rename ProlificID into ParticipantID
VMAC3a <- VMAC3a %>% select(18) #select relevant columns
#create new dataframe titled "vmac.colour" for a column filled with entries of 3A
vmac.colour<-rep(c("3A"),times=114) #use to rep function to enter 114 rows worth of 3A
vmac.colour<-data.frame(vmac.colour) #push it into a dataframe titled
View(vmac.colour) #check, if done properly there should only be one column titled "vmac.colour
vmac3a <-cbind(VMAC3a, vmac.colour) #use cbind function to add the two dataframes together into a new dataframe titled "vmac3a"
#check the new dataframe, there should now be two columns: one of them ParticipantID and one named vmac.colour
View(vmac3a)#indication of participants who done the 3a version of the vmac now complete
vmac3a <- vmac3a %>% 
  filter(str_detect(ParticipantID, "^5|^6")) #remove irrelevant data based on ParticipantID
View(vmac3a) #check again to make sure relevant Participants of 3a vmac are all accounted for
#now load in the full raw data of the vmac
vmacfull <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/vmacfull.csv")
names(vmacfull) <- gsub("\\W", "", names(vmacfull)) #replace the apostrophes around the column headers
#replacing spaces with periods are not necessary here and ParticipantID is already named correctly
vmacfull <- vmacfull %>% select(1,5:73) #select relevant columns
vmacfull$ParticipantID <- gsub("'",'',vmacfull$ParticipantID) #remove apostrophes for all data under ParticipantID
vmacfull = vmacfull[-1,] #remove first row as this was a test to make sure game was running properly
vmacfull <- vmacfull %>% 
  filter(str_detect(ParticipantID, "^5|^6")) #filter out ParticipantIDs not relevant to study
#now combine 3a vmac participants with full raw vmac data by ParticipantIDs
vmacfull <- merge(vmacfull,vmac3a, by = "ParticipantID", all = TRUE) #now the additional vmac.colour column will give an indication of Participants who did the 3a version of vmac
vmacfull$PlayNumber <- ave(vmacfull$ParticipantID, vmacfull$ParticipantID,  FUN = seq_along) #create play number variable that counts the number of time the participants did the qualtrics

#tidying BART data
BART <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/BART_Norm.csv") #load raw BART data into a dataframe
BART <- BART %>% select(1,8:27) #select relevant columns
BART$ParticipantID <- gsub("'",'',BART$ParticipantID) #replace apostrophes with blanks for all values under the ParticipantID column, this will make future processing easier
BART$PlayNumber <- ave(BART$ParticipantID, BART$ParticipantID,  FUN = seq_along) #create play number variable that counts the number of time the game appears for each participant
BART <- BART %>% 
  filter(str_detect(ParticipantID, "^5|^6")) #select rows based on the ParticipantIDs which must have values that either start with 5 or 6
cognitivegames <- merge(vmacfull,BART, by = c('ParticipantID','PlayNumber'), all= TRUE) #merge vmacfull & BART by two columns: 'ParticipantID' and 'PlayNumber'

#refining DBT data
DBT <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/DBT norm raw.csv") #read DBT raw data into a dataframe
DBT <- DBT %>% select(3,11) #select the two only relevant columns 'TotalScore' & 'Name'
names(DBT) <- gsub("\\W", "", names(DBT)) #replace the apostrophes in the headers with spaces, otherwise R won't recognise the column names
DBT <- rename(DBT, ParticipantID=Name) #rename Name column header into ParticipantID
DBT$ParticipantID <- gsub("'",'',DBT$ParticipantID) #remove all apostrophes in the data of under the ParticipantID column
DBT <- DBT %>% 
  filter(str_detect(ParticipantID, "^5|^6")) #select rows based on the ParticipantIDs which must have values that either start with 5 or 6
#NOTICE may include IDs you may not want
DBT = DBT[-1,] #in this particular instance the only unwanted ParticipantID was in the first row, hence it was deleted
DBT$PlayNumber <- ave(DBT$ParticipantID, DBT$ParticipantID,  FUN = seq_along) #create play number variable that counts the number of time the game appears for each participant
cognitivegames <- merge(cognitivegames,DBT, by = c('ParticipantID','PlayNumber'), all= TRUE) #merge cognitive games two columns: 'ParticipantID' and 'PlayNumber'

#tidying CST data
CST <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/CST norm raw.csv") #read in the category switch raw data into a dataframe
CST <- rename(CST,ParticipantID=subjectid) #rename column 4 header subjectid to ParticipantID
CST <- CST %>% select(4,9:10,15:22) #keep relevant columns
CST$PlayNumber <- ave(CST$ParticipantID, CST$ParticipantID,  FUN = seq_along) #create play number variable that counts the number of time the game appears for each participant
cognitivegames <- merge(cognitivegames,CST, by = c('ParticipantID','PlayNumber'), all= TRUE) #merge CST into cognitivegames by ParticipantID & PlayNumber

#refining SST data
SST <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/SST_Norm.csv") #read raw SST data into a dataframe
SST <- SST %>% select(1,8:20) #select relevant columns
SST$ParticipantID <- gsub("'",'',SST$ParticipantID) #replace apostrophes in data under the ParticipantID column with spaces, makes future processing easier
SST$PlayNumber <- ave(SST$ParticipantID, SST$ParticipantID,  FUN = seq_along) #create play number variable that counts the number of time the game appears for each participant
cognitivegames <- merge(cognitivegames,SST, by = c('ParticipantID','PlayNumber'), all= TRUE) #merge BART_DBT_CST with SST by ParticipantID & PlayNumber

#standardizing DDT data
DDT <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/DDT norm raw data.csv") #read in the DDT raw data
DDT <- rename(DDT,ParticipantID=ID) #rename column 1 titled: ID to ParticipantID
DDT$ParticipantID <- gsub("'",'',DDT$ParticipantID) #replace all ' with spaces in data under the ParticipantID column
DDT <- DDT %>% select(1,4:6) #keep relevant columns
DDT <- DDT %>% 
  filter(str_detect(ParticipantID, "^5|^6")) #select rows based on the ParticipantID column which have values that either start with 5 or 6
#NOTICE might include IDs you may not want
DDT = DDT[-1,] #in this case the ParticipantID of the first row is not wanted, hence its removal
DDT$PlayNumber <- ave(DDT$ParticipantID, DDT$ParticipantID,  FUN = seq_along) #create play number variable that counts the number of time the game appears for each participant
cognitivegames <- merge(cognitivegames,DDT, by = c('ParticipantID','PlayNumber'), all= TRUE) #merge BART_DBT_CST_SST with DDT by ParticipantID & PlayNumber

#tidying EAT data
EAT <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/EAT norm raw.csv") #import raw EAT data into dataframe
EAT <- rename(EAT,ParticipantID=subjectid) #rename subjectid into ParticipantID
EAT <- EAT %>% select(5,8:12) #select relevant columns
EAT$PlayNumber <- ave(EAT$ParticipantID, EAT$ParticipantID,  FUN = seq_along) #create play number variable that counts the number of times the game appears for each participant
cognitivegames <- merge(cognitivegames,EAT, by = c('ParticipantID','PlayNumber'), all= TRUE) #merge BART_DBT_CST_SST_DDT with EAT by ParticipantID & PlayNumber

#n-back data refinement
nback <- read_csv("C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/n-back_raw.csv") #load in n-back raw data to a dataframe
nback <- rename(nback,ParticipantID=subjectid) #rename column 5 titled: subjecid to ParticipantID
nback <- nback %>% select(5,9:49) #keep relevant columns
nback$PlayNumber <- ave(nback$ParticipantID, nback$ParticipantID,  FUN = seq_along) #create play number variable that counts the number of times the game appears for each participant
cognitivegames <- merge(cognitivegames,nback, by = c('ParticipantID','PlayNumber'), all= TRUE) #merge BART_DBT_CST_SST_DDT_EAT & nback by ParticipantID & PlayNumber

#finally combine qualtrics data with cognitive games into full database
fulldatabase <- merge(qualtricsdata,cognitivegames, by = c('ParticipantID','PlayNumber'), all= TRUE)
#export full database to desired location to check if everything is formatted properly
write.csv(fulldatabase,"C:/Users/ajratkheur/Desktop/2021/internship Erynn/norming study resources/normingstudyfulldatabase.csv", row.names = FALSE)

#now merge qualtrics data with games
fulldatabase <- merge(qualtricsdata,cognitivegames, by = c('ParticipantID','PlayNumber'), all= TRUE) 

# export to csv
write.csv(fulldatabase, "NormStudyNIH_Data.csv", row.names = FALSE)

#clear console and environment
#ctrl + l + 
rm(list=ls())


#brainstorm of vmac combination if still needed
vmacfull <- merge(vmacfull, vmac3a, by = "ParticipantID", all.x = TRUE) #combine and only keep columns this gets us closer but there are now only the 3a participants
vmacfull <- merge(vmacfull, vmac3a, by = "ParticipantID", all.y = TRUE)  #full dissected written out version of the Erynn's original code
vmacfull <- merge(vmacfull, vmac3a, by = "ParticipantID", all.x = TRUE, all.y = TRUE) #basically written out version of Erynn's original code

#adds a column full of zeros which is titled vmac.colour in complete raw vmac data
vmacfull['vmac.colour'] <- 0

#trial and error of creating new column using tidyverse
vmacfull <- vmacfull %>% add_column(vmac.colour = NA)
vmacfull1 <- merge(vmacfull,vmac3a, by = "ParticipantID", all = TRUE) #created two new columns of vmac.colour based on rows and columns and additional rows were added, indicating duplication of some sort
vmacfull2 <- merge(vmacfull,vmac3a, by = "ParticipantID", "vmac.colour", all = TRUE) #created new column and now there is only 1 vmac.colour column but ParticipantID shifts into that column for the 3a participants

#more brainstorm
vmacfull$vmac.colour= 0
vmacfull <- merge(vmacfull,vmac3a, by = "ParticipantID", all = TRUE)
Example <- merge(df1, df2, by = "col1", all = TRUE)
dplyr::bind_rows(vmacfull, vmac3a)