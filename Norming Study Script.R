#//NORMING STUDY//
# Created by: Erynn Christensen
# Created Date: 15/02/2021

#///////////////////////////////////////
# BEFORE WE START

#Clear environment
rm(list = ls()) 

#set working directory
setwd("~/PhD/Studies/Norming Study")

# load packages
library(readr)
library(tidyverse)
library(dplyr)

#/////////////////////////////////////
# LOADING AND PREPARING DATA

# load data into data frame
# skip = 1 will skip the first row and make the second row the header
# [-c(1),] removes the second row (do not want them added in the middle of the data set when we combine)

Ax1 <- read_csv("Online Norming Study_Ax1.csv", skip = 1)[-c(1),] 
Ax2 <- read_csv("Online Norming Study_Ax2.csv", skip = 1)[-c(1),]
Ax3 <- read_csv("Online Norming Study_Ax3.csv", skip = 1)[-c(1),]

# get rid of spaces in column names, substitute for "."
names(Ax1) <- make.names(names(Ax1), unique = TRUE) 
names(Ax2) <- make.names(names(Ax2), unique = TRUE)
names(Ax3) <- make.names(names(Ax3), unique = TRUE)

# rename column with ID as 'ParticipantID' we will need this to combine data frames
Ax1 <- rename(Ax1, ParticipantID=Please.enter.your.Prolific.ID.)
Ax2 <- rename(Ax2, ParticipantID=Please.enter.your.Prolific.ID)
Ax3 <- rename(Ax3, ParticipantID=Please.enter.your.Prolific.ID)

# load BrainPAC task data 

SST <- read.csv("SST_Norm.csv")
BART <- read.csv("BART_Norm.csv")

# select relevant columns

SST <- SST %>% select(1,8:20)
BART <- BART %>% select(1,8:27)

# remove '' from Prolific ID using gsub 
# here we have asked gsub to look for all ' and replace with blank

SST$ParticipantID <- gsub("'",'',SST$ParticipantID) 
BART$ParticipantID <- gsub("'",'',BART$ParticipantID)

#/////////////////////////////////////
# COMBINE DATASETS 
# for this to work, column name you wish to merge data with must be the same across aLL dfs
# before we can merge the data sets, first we have to address those participant IDs that occur more than once 
# (this is because participant completed game multiple times) for each data set need to create a variable that 
# counts the number of times the ParticipantID occurs e.g. 3 means the third appearence of this ID
# call this variable PlayNumber. If we did not do this for every merge made that has the same participant ID appear multiple times R will provide the cross product eg. if DDT has participant ID '35a6' 
# appear twice (A,B) and SST has the same ID appear twice (C,D) when merging R will create 4 rows: A + C, A + D, B + C, B + D

BART$PlayNumber <- ave(BART$ParticipantID, BART$ParticipantID,  FUN = seq_along)

SST$PlayNumber <- ave(SST$ParticipantID, SST$ParticipantID,  FUN = seq_along)

Ax1$PlayNumber <- ave(Ax1$ParticipantID, Ax1$ParticipantID,  FUN = seq_along)

Ax2$PlayNumber <- ave(Ax2$ParticipantID, Ax2$ParticipantID,  FUN = seq_along)

Ax3$PlayNumber <- ave(Ax3$ParticipantID, Ax3$ParticipantID,  FUN = seq_along)

# now we can merge by two columns 'ParticipantID' and 'PlayNumber'

BART_SST <- merge(BART,SST, by = c('ParticipantID','PlayNumber'), all= TRUE) #in order to merge all rows even if there is not a match 
                                                                            #(maybe a participant only played one of the two games) 
                                                                            #need to specify all = TRUE (it is false by default) 

# now merge Qualtrics data must do two at a time 

QualtricsData <- merge(Ax1,Ax2, by = c('ParticipantID','PlayNumber'), all= TRUE)

QualtricsData <- merge(QualtricsData,Ax3, by = c('ParticipantID','PlayNumber'), all= TRUE)

# now merge games with qualtrics data

Fulldatabase <- merge(QualtricsData,BART_SST, by = c('ParticipantID','PlayNumber'), all= TRUE) 

# export to csv
write.csv(Fulldatabase, "NormStudyNIH_Data.csv", row.names = FALSE) #choicetext

