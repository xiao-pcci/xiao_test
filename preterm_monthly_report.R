
# ------------------------------------------------------------------------------------------------- #
# PCHP preterm project
# this script is to run monthly report of claims-based preterm risk predictive model
# every month (~15th) after asthma report is generated or 
# required data is saved in T drive - seven R dataframes in T:\PCHPAsthma\Data\PCHP raw data
# since Apr 13, 2020, start to run report on VM
# so first need to copy and paste the following 4 RData from current month into VM - mem, elg, claims_new, rx_new
# the step index below follows original weiwei's definitaion
# since Oct 21, 2020, start to run report on PCCI Jupyter hub R environment
# no need to copy and paste data, could access T drive directly
# 
# modified by Xiao Wang
# ------------------------------------------------------------------------------------------------- #


#### STEP 0: Load libraries ####

rm(list = ls())

options(encoding = "UTF-8")

library(dplyr)  # data manipulation
library(plyr)  # spli, apply and combine data
library(tidyr)  # tidy messy data
library(data.table)  # data manipulation operations
library(lubridate)  # date manipulation
library(mondate)  # date manipulation
library(zoo)  # S3 infrastructure for regular and irregular time series
library(sqldf)  # manipulate R data frames using SQL
library(stringr)  # common string operations
library(ggplot2)  # create data visualization
library(glmnet)  # generalized linear models

#Sys.setenv(JAVA_HOME='C:/Program Files (x86)/Java/jre1.8.0_251')
#Sys.getenv("JAVA_HOME")  # expect "", not above
# need most recent updated Java installed to generate final report
library(XLConnect)   # manipulate Excel files in R 
#library(xlsx)  # programmatic control of Excel files
library(readxl)  # import Excel files into R 
# Packages XLConnect and xlsx are not compatible and can not be loaded at the same time. 
# Reason is that XLConnect (with XLConnectJars) and xlsx with (xlsxjars) ship with different versions of Apache POI. 
# Depending on which package is loaded first, the other package won't work.
cat('Libraries loaded.\n')


setwd("T:/PCHP PreTerm/Data")  # path in T drive from local
setwd("/home/michelle/T-Drive/PCHP PreTerm/Data")  # path in T drive from PCCI dev
getwd()


#### PCHP Preterm Project Step 1 & 2 ####
# Generate pregnant women cohort based by HEDIS criteria in claims data and RiskGroupID in Vital data (VDT demographics)
# Exclude outliers and pregnant women already delivered

#### STEP 1: Define pregnant women, Anchor date and Pregnancy date ####

#### 1.1 Prepare and save two accumulating claims data ####
# use accumulating claims data generated from Asthma monthly report step 1 to generate claims data
# 1) in the past 1.5 years from prediction time without multiple gestation;
# 2) in the past 5 years from prediction time without multiple gestation;

# CHANGE the months below and automate these two dates in script
start_dt <- as.Date("2020-03-01")
end_dt <- start_dt + months(12) - days(1)

# CHANGE month and folder below
#load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/Raw_Data_MONTH_YEAR/PCHP_claims_new_MONTH.RData") 
#load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/PCHP_claims_new_MONTH.RData")
# connecting to T drive loading claims data is super slow
# upload data to PCCI dev first (also slow but feasible) and load
load("/home/michelle/preterm_monthly_report/PCHP_claims_new_February.RData")
nrow(pchp_claims)
# 24748033,25050589,25244279,25476102,25641151,25850354,26125904,26345887,26589617,26732913,26950912,27247316,27449586,
#pchp_claims <- pchp_claims[!is.na(pchp_claims$DOS), ]
# expect max(DOS) to be the same month as in the name of this claims data, truncate starting date to be 2016-1-1 in Jan 2020, truncate starting date to be 2015-8-1 in Aug 2019
range(pchp_claims$DOS)  # "2016-01-01" "2021-02-23"

# convert all DGN into captical letters by xiao in Jun 23 2020
pchp_claims$DGN1 <- toupper(pchp_claims$DGN1)
pchp_claims$DGN2 <- toupper(pchp_claims$DGN2)
pchp_claims$DGN3 <- toupper(pchp_claims$DGN3)
pchp_claims$DGN4 <- toupper(pchp_claims$DGN4)
pchp_claims$DGN5 <- toupper(pchp_claims$DGN5)


# remove multiple gestations
# ----------------------------------------- OLD CRITERIA ------------------------------------------------------------------------------------------- #
# t_claim <- pchp_claims
# t_claim <- as.data.frame(t_claim)
# # takes LONG
# t_claim$multi_gest <- rowSums((t_claim[,str_detect(names(t_claim),"DGN")] >= 651 & t_claim[,str_detect(names(t_claim),"DGN")] <= 651.99) | 
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.001' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.002' |
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.003' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.009' |
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.101' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.102' |
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.103' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.109' |
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.201' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.202' |
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.203' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.209' |
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.801' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.802' |
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.803' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.809' |
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.90' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.91' | 
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.92' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O30.93' |
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O31.10X0' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O31.11X0' | 
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O31.30X0' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O31.31X0' | 
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O31.32X0' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O31.33X0' | 
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O31.8X10' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O31.8X20' | 
#                                t_claim[,str_detect(names(t_claim),"DGN")] == 'O31.8X30' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O31.8X90')
# t_claim$multi_gest <- as.numeric(t_claim$multi_gest > 0)
# prop.table(table(t_claim$multi_gest))   # 0.08-0.09% with multiple gestations
# # exclude multiple gestations
# #t_claim <- t_claim[t_claim$multi_gest == 0, ]
# t_claim <- t_claim %>% filter(multi_gest == 0)  
# pchp_claims <- t_claim
# pchp_claims$multi_gest <- NULL
# rm(t_claim)
# -------------------------------------------------------------------------------------------------------------------------------------------------- #

# NEW CRITERIA - xiao added all codes in 3 code families in Apr 3 2020 
multi_gest <- paste0(c("651","O30","O31"), collapse = "|")
pchp_claims$multi_gest <- 0
DGN_cols <- rev(names(pchp_claims)[which(substr(names(pchp_claims), 1, 3) == "DGN")])
for(col in DGN_cols){
  index <- regexpr(multi_gest, pchp_claims[, col])
  pchp_claims[, "multi_gest"] <- ifelse(index != -1, 1, pchp_claims[, "multi_gest"])     
}
table(pchp_claims$multi_gest)  # in initial change: 4552 for 651 family, 66001 for O30 family, 1513 for O31 family; 
# for all three families: 71598,72809,73033,73700,74039,74796,75190,76077,76986,77054,77010,78163,78491,
prop.table(table(pchp_claims$multi_gest))  # ~0.29%; before adding complete O30+O31 families, ratio is ~0.08-0.09% 
# exclude multiple gestations
pchp_claims <- pchp_claims[pchp_claims$multi_gest == 0,]
#pchp_claims <- pchp_claims %>% filter(multi_gest == 0)
pchp_claims$multi_gest <- NULL
rm(multi_gest,DGN_cols,col,index)

# saving (to PCCI DEV) is too slow, only keep useful columns, added in 3/26/2021
pchp_claims$prm_as <- NULL
pchp_claims$prm_sec_as <- NULL
pchp_claims$MAP <- NULL
pchp_claims$key <- NULL
pchp_claims$MEMBER_ID_2 <- NULL
pchp_claims$AuthNUmber <- NULL
pchp_claims$EOB <- NULL
pchp_claims$COBAmount <- NULL
pchp_claims$CoPayDeductAmount <- NULL
pchp_claims$WithHoldAmount <- NULL

pchp_claims_original <- pchp_claims

# ------------------------------------------------------------------------------------------------------------------ #
# [Remark] starting from 3/26/2021, save two accumulative claims data after defining pchp_pregnant & pchp_baby
# data size will be 1/2, truncating since original saving is too slow
# ------------------------------------------------------------------------------------------------------------------ #

pchp_claims <- pchp_claims %>% filter(MEMBER_ID %in% pchp_pregnant$MemberID | MEMBER_ID %in% pchp_baby$MEMBER_ID)

# save 5-year claims data and NAME it by new dates - "PCHP_claim_08022013_'date'withoutmultiple"
# changed into "PCHP_claim_08012015_'date'withoutmultiple" since Aug 2019
# changed into "PCHP_claim_01012016_'date'withoutmultiple" since Jan 2020
# where 'date' is the last day of current prediction month
#save(pchp_claims, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/Accumulating Claim Data for generating monthly report/PCHP_claim_TIME1_TIME2withoutmultiple.RData")
# connecting from PCCIdev to T might be really slow, better to save in VM and then download to local
#save(pchp_claims, file = "T:/Pre Term/Data/Accumulating Claim Data for generating monthly report/PCHP_claim_01012016_02282021withoutmultiple.RData") 
save(pchp_claims, file = "/home/michelle/preterm_monthly_report/PCHP_claim_01012016_02282021withoutmultiple.RData", ascii = FALSE, compress = TRUE, compression_level = 9) 
nrow(pchp_claims)
# 24676435,24977780,25171246,25402402,25567112,25775558,26049994,26269810,26512631,26655859,26873902,27169153,27371095->14546737(filter pregnancy/baby cohort only),
#pchp_claims5y <- pchp_claims


# look backward for 1.5 years from now in claims is - 
as.Date(format(max(pchp_claims$DOS), "%Y-%m-01")) - months(18)
as.Date(format(end_dt, "%Y-%m-01")) - months(18)
date_18m <- as.Date(format(max(pchp_claims$DOS), "%Y-%m-01")) - months(18)
pchp_claims <- pchp_claims[!is.na(pchp_claims$DOS) & pchp_claims$DOS >= date_18m,]   
range(pchp_claims$DOS)    # expect an 1.5-year range

# save 1.5-year claims data and NAME it by new dates - "PCHP_claims_date1_date2.RData"
# where date1 is the date in the filter condition above (not exactly the same, to keep consistency, we use the last day of the previous month) 
# date2 is the 23rd of current prediction month
#save(pchp_claims, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/Accumulating Claim Data for generating monthly report/PCHP_claims_DATE1_DATE2.RData")
#save(pchp_claims, file = "T:/Pre Term/Data/Accumulating Claim Data for generating monthly report/PCHP_claims_20190731_02232021.RData")
save(pchp_claims, file = "/home/michelle/preterm_monthly_report/PCHP_claims_20190731_02232021.RData", ascii = FALSE, compress = TRUE, compression_level = 9)
nrow(pchp_claims)
# 9655451,9497607,9371035,9123759,8866588,8714299,8525067,8349590,8289141,8095011,7994740,8014557,7567689->4672570(filter pregnancy/baby cohort only),
#pchp_claims18m <- pchp_claims


# reset pchp_claims to be the 5-year claims without multiple gestations
pchp_claims <- pchp_claims_original
rm(pchp_claims_original,date_18m)


#### 1.2 Generate initial pregnant candidate cohort ####
# 1) load membership data from Asthma step 1 of the current month                                 
# 2) filter women aged between 11 and 55 at the prediction time (Anchor) 
# 3) these member IDs are our initial candidate cohort

# Load membership data, CHANGE month and date below
#load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/PCHP_mem_new_MONTH.RData")
load("/home/michelle/preterm_monthly_report/PCHP_mem_new_February.RData")
nrow(pchp_mem)
# 357053,356358,355272,354426,353539,352348,351125,349750,348219,346550,344912,343658,341688,
Anchor1 <- end_dt

# use DOB cutoff to extract claims data for women in the right age range
#DOB_max <- Anchor1 - years(11)
#DOB_min <- Anchor1 - years(55) 
DOB_max <- Anchor1 - as.difftime(as.integer(365.25*11), unit = "days")
DOB_min <- Anchor1 - as.difftime(as.integer(365.25*55), unit = "days")

# filter female with a valid DOB, age 11-55
pchp_pregnant <- pchp_mem[!is.na(pchp_mem$DOB) & (pchp_mem$Gender == 'F') & (pchp_mem$DOB<=as.Date(DOB_max)) & (pchp_mem$DOB>=as.Date(DOB_min)),]  
nrow(pchp_pregnant)
# 111106,111019,110887,110936,110928,110738,110522,110297,109989,109561,109167,108903,108408,
rm(pchp_mem)


#### 1.3 Process mom-baby link file ####
# goal: to remove the cases where one babyID corresponds to multiple momIDs

# the original monthly mom-baby Link files are saved in the folder "/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna"
# updated on the 14th of each month, named "PRKLD_MBR_MOMMY_LINK_yearXX14", a txt file
# first COPY&PASTE&SAVE it as a csv file in the sub-folder '/home/michelle/T-Drive/PCHP PreTerm/Data/Mom_Baby_Link'
# named as "Link_MONTH14YEAR.csv", where MONTH is the current prediction month, YEAR is the current year

# criteria:
# if a baby matches with mom1 and mom2, we choose the mom who exists in the claims data of current month;
# if both moms exists in current month claims data, we select either one;
# if no matching mom information exists, this baby will be excluded anyway;
Mom_Baby_Link_processing <- function(month, year){
  
  if(nchar(month) == 1){  # for Jan to Sep, add 0 in front of month index
    link_file <- paste("Link_", gsub("(\\d)+", "0\\1", month), "14", toString(year), ".csv", sep = "")
  }else(  # Oct, Nov and Dec
    link_file <- paste("Link_", toString(month), "14", toString(year), ".csv", sep = "")
  )
  # in May 2020, delimiter was |
  # in Jun 2020, delimiter was ,
  # in Feb 2021, delimiter was |
  # in Mar 2021, delimiter was ,
  Link <- read.csv(paste("/home/michelle/T-Drive/PCHP PreTerm/Data/Mom_Baby_Link/", link_file, sep = ""), header = T, stringsAsFactors = FALSE, sep = ",")
  #Link <- read.csv(paste("/home/michelle/T-Drive/PCHP PreTerm/Data/Mom_Baby_Link/", link_file, sep = ""), header = T, stringsAsFactors = FALSE, sep = "|")
  
  names(Link)[2] <- "MEMBER_ID"
  # remove duplicated Mom-Baby matching pairs
  Link <- Link[!duplicated(Link[,c(2, 3)]),] 
  # filter baby IDs with multiple matching mom IDs
  Mul_ID <- unique(Link[Link$MEMBER_ID %in% Link[which(duplicated(Link$MEMBER_ID)),][,2],]$MEMBER_ID)
  cat("There are", length(Mul_ID), "baby IDs matching with multiple mom IDs.")
  
  matching_pair <- data.frame(baby_ID = numeric(),
                              mom_ID1 = numeric(), 
                              mom_ID2 = numeric(), 
                              stringsAsFactors = FALSE) 
  
  for(i in 1:length(Mul_ID)){
    matching_pair[i, 1] = Mul_ID[i]
    matching_pair[i, 2] = c(Link$MommyLink_ID[Link$MEMBER_ID==Mul_ID[i]])[1]
    matching_pair[i, 3] = c(Link$MommyLink_ID[Link$MEMBER_ID==Mul_ID[i]])[2]
  }
  
  return(matching_pair)
}

setwd('/home/michelle/T-Drive/PCHP PreTerm/Data/Mom_Baby_Link')

# CHANGE month and year below
matching_pair <- Mom_Baby_Link_processing(03, 2021)  # 30,28,29,29,28,28,34,29,28,28,28,28,

# CHANGE the month below, CHENGE deliminator if needed
#Link <- read.csv("/home/michelle/T-Drive/PCHP PreTerm/Data/Mom_Baby_Link/Link_DATE.csv", header = T, stringsAsFactors = FALSE) 
Link <- read.csv("Link_03142021.csv", header = T, stringsAsFactors = FALSE)  # , sep = "|"
nrow(Link)
# 299088,297710,297254,296500,295877,294971,294083,292835,291508,290692,289841,288668,287268,
names(Link)[2] <- "MEMBER_ID"
# remove duplicated mom-baby matching pairs
Link <- Link[!duplicated(Link[, c(2,3)]),]  
nrow(Link)
# 297806,297200,296734,295973,295338,294428,293536,292286,290953,290140,289284,288113,286704,


# added in 3/26/2021, used to filter claims
# previously delivered babies of current pregnancy cohort
pchp_baby <- Link %>% filter(MommyLink_ID %in% pchp_pregnant$MemberID)  # 63506,


matching_pair$mom_selected <- 0
# if we check momID in the entire pchp_claims, it takes so LONG, so we subset it
momID <- unique(append(matching_pair$mom_ID1, matching_pair$mom_ID2))
checking_claims <- pchp_claims[pchp_claims$MEMBER_ID %in% momID,]  
nrow(checking_claims)
# 1328,1367,1474,1485,1517,1524,2247,1727,1566,1606,1668,1722,1731,

for(i in 1:nrow(matching_pair)){
  
  babyID <- matching_pair[i,]$baby_ID
  momID1 <- matching_pair[i,]$mom_ID1
  momID2 <- matching_pair[i,]$mom_ID2
  
  #check1 <- pchp_claims[pchp_claims$MEMBER_ID==momID1,]$MEMBER_ID
  check2 <- checking_claims[checking_claims$MEMBER_ID==momID2,]$MEMBER_ID
  
  # if momID2 is not in current month claim data, no matter momID1 exists or not, we assign momID1 to this babyID
  if (length(check2) == 0) {
    Link[which(Link$MEMBER_ID==babyID),][2,3]<-momID1
    matching_pair[i,]$mom_selected <- momID1
  }
  # if momID2 is in current month claim data, no matter momID1 exists or not, we assign momID2 to this babyID
  else{
    Link[which(Link$MEMBER_ID==babyID),][2,3]<-momID2
    matching_pair[i,]$mom_selected <- momID2
  }
}

Link <- Link[!duplicated(Link[,2]),] 

# CHANGE the date 
# save(Link, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/Mom_Baby_Link/Link_DATE_processed.RData")
save(Link, file = "Link_03142021_processed.RData")
# manually copy and paste into T drive - data3
rm(i, babyID, momID1, momID2, check2, momID, checking_claims, Mom_Baby_Link_processing, matching_pair)


#### 1.4 Define Anchor date and Pregnancy date in pchp_pregnant ####
# recall that Anchor date is the current prediction date, last day of the month or first day of the next month
# Pregnancy date is 1 year before Anchor date or the last delivery date within a year if there was any delivery

# CHANGE two dates below
#t_claim <- pchp_claims[pchp_claims$DOS>=as.Date("2019-04-01") & pchp_claims$DOS<as.Date("2020-04-01"),] 
t_claim <- pchp_claims[pchp_claims$DOS>=start_dt & pchp_claims$DOS<=end_dt,]
nrow(t_claim)
# 5786985,5739361,5534194,5884702,5209244,4695387,4318974,4201911,4138496,4007171,3962247,3966097,2533034,

# extract baby's claims by DOB, CHANGE two dates below
t_claim_baby_past <- t_claim[t_claim$DOB>=start_dt & t_claim$DOB<=end_dt,]
nrow(t_claim_baby_past)
# 542633,534687,519137,576951,472897,455801,449519,439212,426320,418112,424904,448234,375855,

# link these baby claims with their moms in Link file, if applicable
# used to be join, got errer "join:: invalid value on "by" argument!" in 7/26/2020
t_claim_baby_past <- plyr::join(t_claim_baby_past, Link[, c('MEMBER_ID','MommyLink_ID')], type = 'left', by = 'MEMBER_ID')
t_claim_baby_past <- t_claim_baby_past[!duplicated(t_claim_baby_past ), ]   # take LONG
t_claim_baby_past <- t_claim_baby_past[!is.na(t_claim_baby_past$MommyLink_ID), ] 

t_claim_baby_past$key_1 <- t_claim_baby_past$MommyLink_ID   # useless
t_claim_baby_past$Delivery <- t_claim_baby_past$DOB

# filter momIDs in pchp_pregnant
t_claim_baby_past <- t_claim_baby_past[t_claim_baby_past$MommyLink_ID %in% c(pchp_pregnant$MemberID), ]  
#t_claim_baby_past <- t_claim_baby_past[!duplicated(t_claim_baby_past), ]  # redundant

# check babyDOB (mom's most recent delivery date) is not NA
length(which(is.na(t_claim_baby_past$Delivery)))   # expect 0

t_claim_mom_past <- sqldf("select MEMBER_ID as MEMBER_ID_Baby, MommyLink_ID, Delivery from t_claim_baby_past group by MEMBER_ID")  
names(t_claim_mom_past)[3] <- 'Delivery_past'

# some moms delivered two babies within the past year, this is an approximate number of such moms
length(t_claim_mom_past$MommyLink_ID) - length(unique(t_claim_mom_past$MommyLink_ID))  
# 147,153,155,180,153,150,155,153,157,146,148,158,154,

# exclude babyID column
t_claim_mom_past <- t_claim_mom_past[, !names(t_claim_mom_past) %in% c("MEMBER_ID_Baby")]
# remove duplicates in momID + last delivery date (babyDOB)
t_claim_mom_past <- unique(t_claim_mom_past)

# choose the most recent babyDOB as the most recent delivery date
t_claim_mom_past_1 <- sqldf("select MommyLink_ID, Delivery_past, max(Delivery_past) as Delivery_past_1 from t_claim_mom_past group by MommyLink_ID") 
t_claim_mom_past_1$Delivery_past_1 <- as.Date(t_claim_mom_past_1$Delivery_past_1, origin = "1970-01-01")
t_claim_mom_past_1$key <- t_claim_mom_past_1$MommyLink_ID

# key is momID 
pchp_pregnant_P <- plyr::join(pchp_pregnant, t_claim_mom_past_1[, c('MommyLink_ID', 'Delivery_past_1', 'key')], type = 'left', by = 'key') 

# create Anchor date to be current prediction date and Pregnancy date to be 1 year before Anchor date
pchp_pregnant_P$Anchor<-end_dt  
pchp_pregnant_P$Pregnancy<-start_dt 


# for moms with delivery during the past 12 months, Pregnancy date is updated to the most recent delivery date
# i.e. if Delivery_past_1 is not NA, update Pregnancy to that date
pchp_pregnant_P$Pregnancy[which(!is.na(pchp_pregnant_P$Delivery_past_1))] <- pchp_pregnant_P$Delivery_past_1[which(!is.na(pchp_pregnant_P$Delivery_past_1))]

names(pchp_pregnant_P)[1]<-'MEMBER_ID'

# CHANGE index (P suffix)
# [REPLACE] P current_index to P current_index+1 in 151->135 occurrences 
pchp_pregnant_P42_Step1<-pchp_pregnant_P
save(pchp_pregnant_P42_Step1, file = '/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Step1_P42.RData')  
#save(pchp_pregnant_P42_Step1, file = 'D:/PCHP PreTerm/monthly_report/PCHP_Pregnant_Step1_P42.RData')
rm(pchp_pregnant_P42_Step1,Link,t_claim,t_claim_baby_past,t_claim_mom_past,t_claim_mom_past_1,pchp_pregnant_P,pchp_pregnant,pchp_baby)


#### STEP 2: Use HEDIS criteria & RiskGroupID to define pregnancy in cohort ####

#### 2.1 Use HEDIS criteria to define pregnancy in time [Pregnancy, Anchor] in pchp_pregnant ####

# check current claims data is 5-year, otherwise reload
range(pchp_claims$DOS)

HEDIS_pregnant_AddDate <- function(Pregnancy1 = as.Date(start_dt), Anchor1 = as.Date(end_dt), pchp_claims, pchp_delivery_P){
  
  # to extract mom's claim data, exclude baby's claim who shares the same ID with their moms
  #DOB_cutoff <- Anchor1 - years(11)  
  DOB_cutoff <- Anchor1 - as.difftime(as.integer(365.25*11), unit = "days")
  pchp_claims <- pchp_claims[!is.na(pchp_claims$DOS) & (pchp_claims$DOS<=as.Date(Anchor1))&
                               (pchp_claims$DOS>as.Date(Pregnancy1)) & (pchp_claims$DOB<=as.Date(DOB_cutoff)), ]
  pchp_claims <- data.frame(pchp_claims)
  # to filter moms in pchp_pregnant 
  t_claim <- pchp_claims[pchp_claims$MEMBER_ID %in% pchp_delivery_P$MEMBER_ID, ]
  
  # xiao commented: added 'Z36.2', '640.00', '640.80', '640.90', "O33.7", "O34.21", "O34.51/2/3/9" in May 30, 2019
  # 680 codes for BICD1
  prenatal_BICD1 <- paste(c('Z36.2','640.00','640.80','640.90','O33.7','O34.21','O34.51','O34.52','O34.53','O34.59'
                            ,'640.03','O20.0','640.83','O20.8','640.93','O20.9','641.03','O44.01','O44.02','O44.03','O44.21','O44.22','O44.23'
                            ,'O44.41','O44.42','O44.43','641.13','O44.11','O44.12','O44.13','O44.31','O44.32','O44.33','O44.51','O44.52','O44.53'
                            ,'641.23','O45.8X1','O45.8X2','O45.8X3','O45.91','O45.92','O45.93'
                            ,'641.33','O45.001','O45.002','O45.003','O45.011','O45.012','O45.013','O45.021','O45.022','O45.023','O45.091','O45.092','O45.093','O46.001','O46.002','O46.003'
                            ,'O46.011','O46.012','O46.013','O46.021','O46.022','O46.023','O46.091','O46.092','O46.093'
                            ,'641.83','O46.8X1','O46.8X2','O46.8X3','641.93','O46.91','O46.92','O46.93','642.03','O10.011','O10.012','O10.013','O10.911','O10.912'
                            ,'O10.913','642.13','O10.411','O10.412','O10.413','642.23','O10.111','O10.112','O10.113','O10.211','O10.212','O10.213','O10.311','O10.312'
                            ,'O10.313','O11.1','O11.2','O11.3','642.33','O13.1','O13.2','O13.3','O16.1','O16.2','O16.3'
                            ,'642.43','O14.02','O14.03','O14.92','O14.93','642.53','O14.12','O14.13','O14.22','O14.23','642.63','O15.02','O15.03'
                            ,'642.73','O11.1','O11.2','O11.3','642.93','O16.1','O16.2','O16.3','643.03','O21.0','643.13','O21.1','643.23','O21.2'
                            ,'643.83','O21.8','643.93','O21.9','644.03','O60.02','O60.03','644.13','O47.02','O47.03','O47.1','645.13','O48.0','645.23','O48.1'
                            ,'646.03','O31.01X0','O31.02X0','O31.03X0','646.13','O12.01','O12.02','O12.03','O12.21','O12.22','O12.23','O26.01','O26.02','O26.03'
                            ,'646.23','O26.831','O26.832','O26.833','646.33','O26.21','O26.22','O26.23','646.43','O26.821','O26.822','O26.823','646.53','O23.41'
                            ,'O23.42','O23.43','646.63','O23.91','O23.92','O23.93','646.73','O26.611','O26.612','O26.613'
                            ,'646.83','O26.11','O26.12','O26.13','O26.41','O26.42','O26.43','O26.811','O26.812','O26.813','O26.891','O26.892','O26.893','O99.89'
                            ,'646.93','O99.89','647.03','O98.111','O98.112','O98.113','647.13','O98.211','O98.212','O98.213'
                            ,'647.23','O98.311','O98.312','O98.313','647.33','O98.011','O98.012','O98.013','647.43','O98.611','O98.612','O98.613'
                            ,'647.53','O98.511','O98.512','O98.513','647.63','647.83','O98.611','O98.612','O98.613','O98.811','O98.812','O98.813'
                            ,'647.93','O98.911','O98.912','O98.913','648.03','O24.911','O24.912','O24.913','648.13','O99.281','O99.282','O99.283'
                            ,'648.23','O99.011','O99.012','O99.013','648.33','O99.321','O99.322','O99.323','648.43','O99.341','O99.342','O99.343'
                            ,'648.53','O99.411','O99.412','O99.413','648.63','O99.411','O99.412','O99.413','648.73','O33.0','648.83','O24.415','O24.419','O99.810'
                            ,'648.93','O25.11','O25.12','O25.13','O99.281','O99.282','O99.283','649.03','O99.331','O99.332','O99.333'
                            ,'649.13','O99.211','O99.212','O99.213','649.23','O99.841','O99.842','O99.843','649.33','O99.111','O99.112','O99.113'
                            ,'649.43','O99.351','O99.352','O99.353','649.53','O26.851','O26.852','O26.853','649.63','O26.841','O26.842','O26.843'
                            ,'649.73','O26.872','O26.872','651.03','O30.001','O30.002','O30.003','651.13','O30.101','O30.102','O30.103'
                            ,'651.23','O30.201','O30.202','O30.203','651.33','O31.11X0','651.43','651.53','651.63','651.73','O31.31X0','O31.32X0','O31.32X0' 
                            ,'651.83','O30.801','O30.802','O30.803','O31.8X10','O31.8X20','O31.8X30','651.93','O30.91','O30.92','O30.93'
                            ,'652.03','O32.0XX0','652.13','O32.1XX0','652.23','O32.1XX0','652.33','O32.2XX0'
                            ,'652.43','O32.3XX0','652.53','O32.4XX0','652.63','O32.9XX0','652.73','O32.8XX0','652.83','O32.6XX0','O32.8XX0'
                            ,'652.93','O32.9XX0','653.03','O33.0','653.13','O33.1','653.23','O33.2','653.33','O33.3XX0','653.43','O33.4XX0','653.53','O33.5XX0'
                            ,'653.63','O33.6XX0','653.73','O33.7XX0','O33.7XX1','O33.7XX1','O33.7XX2','O33.7XX4','O33.7XX5','O33.7XX9'
                            ,'653.83','O33.8','653.93','O33.9','654.03','O34.01','O34.02','O34.03','654.13','O34.11','O34.12','O34.13'
                            ,'654.23','O34.211','O34.212','O34.219','654.33','O34.511','O34.512','O34.513','O34.531','O34.532','O34.533'
                            ,'654.43','O34.521','O34.522','O34.523','O34.591','O34.592','O34.593','654.53','O34.31','O34.32','O34.33' 
                            ,'654.63','O34.41','O34.42','O34.43','654.73','O34.61','O34.62','O34.63','654.83','O34.71','O34.72','O34.73'
                            ,'654.93','O34.29','O34.81','O34.82','O34.83','O34.91','O34.92','O34.93','655.03','O35.0XX0','655.13','O35.1XX0'
                            ,'655.23','O35.2XX0','655.33','O35.3XX0','655.43','O35.4XX0','655.53','O35.5XX0'
                            ,'655.63','O35.6XX0','655.73','O36.8120','O36.8130','O36.8190','655.83','O35.8XX0','655.93','O35.9XX0','656.03','O43.011'
                            ,'656.13','O36.0110','O36.0120','O36.0130','O36.0910','O36.0920','O36.0930'
                            ,'656.23','O36.1110','O36.1120','O36.1130','O36.1910','O36.1920','O36.1930','656.33','O68','656.43','O36.4XX0'
                            ,'656.53','O36.5110','O36.5120','O36.5130','O36.5910','O36.5920','O36.5930','656.63','O36.61X0','O36.62X0','O36.63X0'
                            ,'656.73','O43.101','O43.102','O43.103','O43.811','O43.812','O43.813','O43.91','O43.92','O43.93'
                            ,'656.83','O36.8310','O36.8320','O36.8330','O36.8910','O36.8920','O36.8930','O68','656.93','O36.91X0','O36.92X0','O36.93X0'
                            ,'657.03','O40.1XX0','O40.2XX0','O40.3XX0','658.03','O41.01X0','O41.02X0','O41.03X0','658.13','O42.011','O42.012','O42.013'
                            ,'658.23','O42.111','O42.112','O42.113','658.33','O75.5'
                            ,'658.43','O41.1010','O41.1020','O41.1030','O41.1210','O41.1220','O41.1230','O41.1410','O41.1420','O41.1430'
                            ,'658.83','O41.8X10','O41.8X20','O41.8X30','658.93','O41.91X0','O41.92X0','O41.93X0','659.03','O61.1','659.13','O61.0'
                            ,'659.23','O75.2','659.33','O75.3','659.43','O09.41','O09.42','O09.43','659.53','O09.511','O09.512','O09.513'
                            ,'659.63','O09.521','O09.522','O09.523','659.73','O76','659.83','O75.89','659.93','O75.9','678.03','O35.8XX0','O36.8210','O36.8220','O36.8230'
                            ,'678.13','O30.021','O30.022','O30.023','679.03','O26.891','O26.892','O26.893','679.13','O35.7XX0'
                            ,'V22.0','Z34.00','V22.1','Z34.80','Z34.90'
                            ,'V22.2','Z33.1','Z33.3','V28.0','Z36.0','V28.1','Z36.1','V28.2','Z36.89','V28.3','Z36.3','V28.4','Z36.4','V28.5','Z36.5'
                            ,'V28.6','Z36.85','V28.81','Z36.89','V28.82','Z36.89','V28.89','Z36.81','Z36.82','Z36.83','Z36.84','Z36.86','Z36.87','Z36.88','Z36.89','Z36.8A'
                            ,'V28.9','Z36.9','V23.0','O09.00','V23.1','O09.10','O09.A0','V23.2','O09.291','V23.3'
                            ,'O09.40','V23.41','O09.211','V23.42','O09.10','V23.49','O09.291','V23.5','O09.291','V23.7','O09.30','V23.81','O09.511','V23.82','O09.521'
                            ,'V23.83','O09.611','V23.84','O09.621','V23.85','O09.819','V23.86','O09.821','O09.822','O09.823','O09.829','V23.87','O36.80X0'
                            ,'V23.89','O09.891','O09.892','O09.893','O09.899','V23.9','O09.90','O09.91','O09.92','O09.93'
                            ,'88.78','BY49ZZZ','BY4CZZZ','BY4FZZZ'))
  
  prenatal_B1 <- c(unique(prenatal_BICD1)) # 644
  
  # 18 codes
  prenatal_B2 <- paste(c('76801','76805','76811','76813','76815','76816','76817','76818','80055','0500F','0501F','0502F','H1000','H1001','H1002','H1003','H1004','H1005'))
  
  prenatal_B <- c(prenatal_B1, prenatal_B2)  # 662
  
  t_claim$pregnant <- as.numeric(t_claim$DGN1%in%prenatal_B | t_claim$DGN2%in%prenatal_B | t_claim$DGN3%in%prenatal_B | t_claim$DGN4%in%prenatal_B |
                                   t_claim$DGN5%in%prenatal_B | t_claim$PROC_1%in%prenatal_B | t_claim$PROC_2%in%prenatal_B | t_claim$PROC_3%in%prenatal_B | t_claim$PROC_4%in%prenatal_B)
  
  
  # to combine eligible claims with pregnant ICD codes with eligible women
  t_claim_111 <- plyr::join(t_claim[, c('DOS','PaidAmount','MEMBER_ID','DOB','pregnant')], pchp_delivery_P[, c('MEMBER_ID','Anchor','Pregnancy')], 
                            type = 'left', by ='MEMBER_ID') 
  t_claim_1111 <- sqldf("select * from t_claim_111 WHERE DOS>Pregnancy AND DOS<=Anchor")  # NOT redundant!
  t_claim_1111 <- t_claim_1111[!duplicated(t_claim_1111), ] # check and remove duplicates before calculating # of visits
  
  # in one DOS, there might be multiple pregnant DGN/PROC codes
  # here only count whether there exists pregnant code in one DOS, exist is 1, non-exist is 0 in column pregnant_b
  t_claim_previsit <- sqldf("select MEMBER_ID, DOB, DOS, Anchor, Pregnancy, sum(pregnant)>0 as pregnant_b from t_claim_1111 group by MEMBER_ID, DOS")
  
  # to extract the most recent and the earliest hospital visit dates with time interval [Pregnancy, Anchor] for those with pregnant codes  
  t_claim_previsit_1 <- sqldf("select MEMBER_ID, DOB, Anchor, Pregnancy, max(DOS) as recent_HEDISDate, min(DOS) as first_HEDISDate 
                              from t_claim_previsit where pregnant_b == 1 group by MEMBER_ID")
  t_claim_previsit_1$recent_HEDISDate <- as.Date(t_claim_previsit_1$recent_HEDISDate, origin = "1970-01-01")
  t_claim_previsit_1$first_HEDISDate <- as.Date(t_claim_previsit_1$first_HEDISDate, origin = "1970-01-01")
  
  # whether a woman is pregnant by HEDIS criteria, pregnant_now = 1 means pregnant, = 0 means not pregnant
  t_claim_previsit_2 <- sqldf("select MEMBER_ID, DOB, Anchor, Pregnancy, sum(pregnant_b)>0 as pregnant_now from t_claim_previsit group by MEMBER_ID")
  
  Current_P <- plyr::join(pchp_delivery_P, t_claim_previsit_2[, c('MEMBER_ID','pregnant_now')], type = 'left', by ='MEMBER_ID') 
  Current_P <- plyr::join(Current_P, t_claim_previsit_1[, c('MEMBER_ID','first_HEDISDate','recent_HEDISDate')], type = 'left', by ='MEMBER_ID') 
  
  return(Current_P)
}


# CHANGE two dates and all index P
load("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Step1_P42.RData")
#load("D:/PCHP PreTerm/monthly_report/PCHP_Pregnant_Step1_P42.RData")
HEDIS_P42 <- HEDIS_pregnant_AddDate(Pregnancy1 = as.Date(start_dt), Anchor1 = as.Date(end_dt), pchp_claims, pchp_pregnant_P42_Step1)
table(HEDIS_P42$pregnant_now)  # 6764,6731,6688,6717,6629,6601,6521,6443,
save(HEDIS_P42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Step2_P42_AddDate.RData") 
#save(HEDIS_P42, file = "D:/PCHP PreTerm/monthly_report/PCHP_Pregnant_Step2_P42_AddDate.RData") 
rm(HEDIS_P42,pchp_pregnant_P42_Step1,Anchor1,DOB_max,DOB_min,HEDIS_pregnant_AddDate)


#### 2.2 Use RiskGroupID in Vital data to define pregnancy in [Pregnancy, Anchor] in pchp_pregnant ####
# generate pregnant cohort based on RiskGroupID criteria, as well as other related variables
# if RiskGroupID == 5, 20, 309 or 310, it means the woman is pregnant
# 5 means Pregnant Woman
# 20 means ST Member Pregnant Women - Qualified Alien
# 309 means Perinatal Mother <= 198% FPL
# 310 means Perinatal Mother > 198% and <= 202% FPL
# Vital data is weekly updated, we choose the one around 15th in each month
# include two more risk groups, confirmed by Yolande, added in 2/27/2021
# 305	means Perinatal; (<= 185) FPL - before birth
# 306	means Perinatal; (> 185 & <= 200) FPL - before birth
# manually "EXTRACT ALL" for Vital data file in the folder /home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/PCHP_VITALDATA_YEARxxxx
# copy and paste this file into VM - waiting for 30min, then unspecified error
# so run this part in local laptop, then copy and paste Demo_Details.RData into VM
# ------------ start local laptop processing --------------------------------------------------------------------------- #
# CONSIDER CONTINUE RUNNING THIS PART IN LOCAL LAPTOP NEXT TIME, TAKEING REALLY LONG!!!
# CHANGE all dates below depending on the Vital data date selected
# Remarks: starting from 12/22/2020, PCHP_VITALDATA_[YEARMONTHDAY].ZIP is renamed as PCHP_DataExtracts_[YEARMONTHDAY].ZIP
# [Important] However, RiskGroupID becomes NA after name change - need further check

# currently use data saved by Jade
#emo_Detail <- read.csv("/home/michelle/T-Drive/MPS/Clients/PCHP/Data/PCCI Reg Elig 010621.csv")  # 274411
library(readxl)
Demo_Detail <-  read_excel("/home/michelle/T-Drive/MPS/Clients/PCHP/Data/PCCI Reg Elig 030521.xlsx", sheet = "pcciREGEL_LB_PCHP")  # 563952
names(Demo_Detail)

#Demo_Detail$EffectiveDate <- as.Date(Demo_Detail$EffectiveDate, format = "%m/%d/%Y")
#range(Demo_Detail$EffectiveDate)  # "2009-10-07" "2020-12-24"
Demo_Detail$Enroll_Eff_Dt <- as.Date(Demo_Detail$Enroll_Eff_Dt, format = "%Y-%m-%d")
range(Demo_Detail$Enroll_Eff_Dt)  # "2009-01-01" "2021-02-22"
Demo_Detail$EffectiveDate <- Demo_Detail$Enroll_Eff_Dt 

#Demo_Detail$TerminationDate <- as.Date(Demo_Detail$TerminationDate, format = "%m/%d/%Y")
#range(Demo_Detail$TerminationDate)  # "2020-01-01" "2078-12-31"
Demo_Detail$Enroll_Term_Dt <- as.Date(Demo_Detail$Enroll_Term_Dt, format = "%Y-%m-%d")
range(Demo_Detail$Enroll_Term_Dt)  # "2018-03-01" "2078-12-31"
Demo_Detail$TerminationDate <- Demo_Detail$Enroll_Term_Dt

#Demo_Detail <- Demo_Detail %>% filter(RiskGroupID %in% c(5,20,305,306,309,310))  # 22617
#table(Demo_Detail$RiskGroupID)
##     5    20   309   310 
## 17558    35  4971    53
Demo_Detail <- Demo_Detail %>% filter(ratecode %in% c(5,20,305,306,309,310))  # 58346
table(Demo_Detail$ratecode)
#     5    20   309   310 
# 42349    93 15716   188 
Demo_Detail$RiskGroupID <- Demo_Detail$ratecode
Demo_Detail$MedicaidID <- Demo_Detail$CARRIERMEMID
# stop processing data from Jade


# setwd("/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/PCHP_VITALDATA_20201208")
# demo <- "PCHP_DEMOGRAPHICS_20201208.txt"
# setwd("/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/PCHP_DataExtracts_20210112")
# demo <- "PCHP_DEMOGRAPHICS_20210112.txt"
# Sys.time()
# Demo_Detail <- read.csv(demo, header = TRUE, quote = "\"", sep = '|', stringsAsFactors = FALSE)   # take LONG, ~30min in PCCI dev
# Sys.time()
# save(Demo_Detail, file = "Demo_Details.RData")
# # ------------ finish local laptop processing -------------------------------------------------------------------------- #
# 
# #load("Demo_Details.RData")
# 
# # clean Demo_Detail
# names(Demo_Detail)
# names(Demo_Detail) <- gsub("\\.", '', names(Demo_Detail))  
# # colnames(Demo_Detail) <-  sub(".", "", colnames(Demo_Detail))  # remove . in column names 
# length(names(Demo_Detail))    # 44 columns
# for(i in 1:length(names(Demo_Detail))){  
#   Demo_Detail[, names(Demo_Detail)[i]] <- str_trim(Demo_Detail[, names(Demo_Detail)[i]], side = "both")
# }
# 
# Demo_Detail$EffectiveDate <- as.Date(Demo_Detail$EffectiveDate)
# Demo_Detail$TerminationDate <- as.Date(Demo_Detail$TerminationDate)
# Demo_Detail$DateofBirth <- as.Date(Demo_Detail$DateofBirth)
# Demo_Detail$MedicaidID <- as.numeric(Demo_Detail$MedicaidID)     # NA introduced
# 
# # remarks: starting from 20201222, RiskGroupID becomes NULL, so use data from Jade
# table(Demo_Detail$RiskGroupID)
# #unique(Demo_Detail$RiskGroupID)
# Demo_Detail$RiskGroupID <- as.numeric(Demo_Detail$RiskGroupID)
# Demo_Detail <- Demo_Detail[Demo_Detail$RiskGroupID %in% c(5,20,305,306,309,310),]  # added in 2/27/2021
# nrow(Demo_Detail)
# # 288518,289696,291160,292365,293379,295669,296811,297842,297842(since still using previous Vital data),
# table(Demo_Detail$RiskGroupID)  # mostly 5 and 309


# CHANGE date below, not needed
end_date <- end_dt
#pchp_elg <- subset(Demo_Detail, (Demo_Detail$TerminationDate > as.Date(end_date)- years(1)) & (Demo_Detail$EffectiveDate <= end_date))
pchp_elg <- subset(Demo_Detail, (Demo_Detail$TerminationDate > as.Date(end_date)- as.difftime(as.integer(365.25*1), unit = "days")) & (Demo_Detail$EffectiveDate <= end_date))
pchp_elg$range <- as.numeric(pchp_elg$TerminationDate - pchp_elg$EffectiveDate)
range(pchp_elg$range)  # 5,28  22159
# purpose: continually enrolled after delivery (for at least 60 days by Medicaid policy)
pchp_elg <- pchp_elg[pchp_elg$TerminationDate > end_date,]
#pchp_elg <- pchp_elg[pchp_elg$EffectiveDate > as.Date(end_date) - years(1), unit = "days"),]
pchp_elg <- pchp_elg[pchp_elg$EffectiveDate > as.Date(end_date) - as.difftime(as.integer(365.25*1), unit = "days"),]

which(duplicated(pchp_elg$MedicaidID))   # check duplicates, expect 0
# pchp_elg %>% group_by(MedicaidID) %>% tally() %>% filter(n >= 2)
# # one patient has two Risk IDs, remove any one
# pchp_elg <- pchp_elg %>% group_by(MedicaidID) %>% slice(1)
pchp_elg <- pchp_elg[order(pchp_elg$EffectiveDate, decreasing = TRUE), ] 

# CHANGE all P index below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Step2_P42_AddDate.RData")
#load("D:/PCHP PreTerm/monthly_report/PCHP_Pregnant_Step2_P42_AddDate.RData")
HEDIS_P42$pregnant_now[is.na(HEDIS_P42$pregnant_now)] <- 0  # fill in NA with 0
HEDIS_P42$pregnant_now1 <- 0
HEDIS_P42$pregnant_now1[HEDIS_P42$MEMBER_ID %in% pchp_elg$MedicaidID] <- 1
HEDIS_P42$Pregnant <- as.numeric(HEDIS_P42$pregnant_now1 == 1 | HEDIS_P42$pregnant_now == 1)

# ----------------------------------------------------------------------------------------------------------------------- #
# identify pregnancy flag source - HEDIS only, RiskGroupID only, or both
table(HEDIS_P42$pregnant_now)  # 6685,6735,6649,6871,6706,6764,6731,6688,6717,6629,6601,6521,9886?,
table(HEDIS_P42$pregnant_now1)  # 6058,6418,7728,8334,8911,9229,9336,9393,9380,9413,8945,8397,6443?,
HEDIS_P42 %>% filter(pregnant_now == 1 & pregnant_now1 == 1) %>% tally()  # 4378,4501,4605,4755,4714,4661,4667,4525,4336,4019,4358,
#library(VennDiagram)
#library(limma)
#vennDiagram(vennCounts(HEDIS_P42 %>% select(pregnant_now, pregnant_now1)))  # 4176 in intersection
# ----------------------------------------------------------------------------------------------------------------------- #

Cohort_P42 <- HEDIS_P42[HEDIS_P42$Pregnant == 1,]
nrow(Cohort_P42)
# 8710,8977,9999,10704,11012,11238,11353,11420,11430,11517,11210,10899,11971,
which(duplicated(Cohort_P42$MEMBER_ID))  # check duplicates, expect 0

Cohort_P42_Add <- merge(Cohort_P42, pchp_elg[,c('MedicaidID','RiskGroupID','EffectiveDate')], by.x = 'MEMBER_ID', by.y = 'MedicaidID', all.x = TRUE, all.y = FALSE)


#### 2.3 Remove delivered women from cohort ####
# criteria:  
# babyDOB delivered within last year is after EffectiveDate or 
# babyDOB is after recent_HEDISDate

# CHANGE all P index below
Cohort_P42_Add <- Cohort_P42_Add %>% filter((Delivery_past_1 <= EffectiveDate) | is.na(Delivery_past_1))  
nrow(Cohort_P42_Add)
# 6969,7038,7225,7378,7499,7615,7493,7452,7410,7541,7036,6779,7466,
Cohort_P42_Add <- Cohort_P42_Add %>% filter((Delivery_past_1 <= recent_HEDISDate) | is.na(Delivery_past_1))  
nrow(Cohort_P42_Add)
# 6910,6977,7138,7275,7366,7568,7342,7276,7219,7339,6823,6585,7260,

save(Cohort_P42_Add, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Cohort_P42_Updated_AddDate.RData")
#save(Cohort_P42_Add, file = "D:/PCHP PreTerm/monthly_report/PCHP_Pregnant_Cohort_P42_Updated_AddDate.RData")
rm(HEDIS_P42,Cohort_P42,Cohort_P42_Add,end_date,Demo_Detail,demo,i,pchp_elg,pchp_claims)


#### PCHP Preterm Project Step 3 ####
# Generate "current" variables during current pregnancy time window  

# load 1.5 year claims data
# CHANGE two dates below
#load("/home/michelle/T-Drive/PCHP PreTerm/Data/Accumulating Claim Data for generating monthly report/PCHP_claims_DATE1_DATE2.RData")
#load("/home/michelle/T-Drive/PCHP PreTerm/Data/Accumulating Claim Data for generating monthly report/PCHP_claims_20190630_01232021.RData")
load("/home/michelle/preterm_monthly_report/PCHP_claims_20190731_02232021.RData")
range(pchp_claims$DOS)  # expect 1.5-year

Application_Claim_Current <- function(Pregnancy1 = as.Date(start_dt), Anchor1 = as.Date(end_dt), pchp_claims, pchp_delivery_P){
  
  # age between [11, 55], to filter mom's claim
  #DOB_max <- Anchor1 - years(11) 
  DOB_max <- Anchor1 - as.difftime(as.integer(365.25*11), units = "days")
  #DOB_min <- Anchor1 - years(55)  
  DOB_min <- Anchor1 - as.difftime(as.integer(365.25*55), units = "days")
  # to filter women in pregnant cohort defined in step 1&2
  t_claim <- pchp_claims[pchp_claims$MEMBER_ID %in% pchp_delivery_P$MEMBER_ID, ] 
  # to filter study time window [Pregnancy, Anchor]
  t_claim <- t_claim[!is.na(t_claim$DOS) & (t_claim$DOS <= as.Date(Anchor1)) & (t_claim$DOS > as.Date(Pregnancy1)) & 
                       (t_claim$DOB <= as.Date(DOB_max)) & (t_claim$DOB >= as.Date(DOB_min)), ]
  
  t_claim <- data.frame(t_claim)
  
  #### generate prenatal visit flag ####
  # xiao commented: added 24 codes (21 are moved from BICD, 6 are newly added) from 'V22' in Mar 15, 2020
  prenatal_BICD0<-paste(c('V22.0','V22.1','V22.2','V23.0','V23.1','V23.2','V23.3','V23.4','V23.5','V23.9','V28.0','V28.1','V28.2','V28.3',  # 14 codes
                          'V22','V23','V23.41','V23.42','V23.49','V23.7','V23.8','V23.81','V23.82','V23.83','V23.84','V23.85','V23.86','V23.87',
                          'V23.89','V28','V28.4','V28.5','V28.6','V28.8','V28.81','V28.82','V28.89','V28.9'))  # 38 codes
  
  # xiao commented: added 'Z36.2' in May 30, 2019; separated 14 codes into prenatal_BICD0, which have different meaning in ICD9 and ICD10 in May 20, 2019
  # xiao commented: separated 18 codes into prenatal_BICD0; moved 9 codes from BICD2; added ~70 codes in Mar 15, 2020
  prenatal_BICD1<-paste(c('Z33.1','Z33.3',
                          'Z34','Z34.0','Z34.00','Z34.01','Z34.02','Z34.03','Z34.8','Z34.80','Z34.81','Z34.82','Z34.83','Z34.9','Z34.90','Z34.91','Z34.92','Z34.93',
                          'Z36','Z36.0','Z36.1','Z36.2','Z36.3','Z36.4','Z36.5','Z36.8','Z36.81','Z36.82','Z36.83','Z36.84','Z36.85','Z36.86','Z36.87','Z36.88','Z36.89','Z36.8A','Z36.9',
                          'O09','O09.0','O09.00','O09.01','O09.02','O09.03','O09.1','O09.10','O09.11','O09.12','O09.13','O09.2','O09.21','O09.211','O09.212','O09.213','O09.219',
                          'O09.29','O09.291','O09.292','O09.293','O09.299','O09.3','O09.30','O09.31','O09.32','O09.33','O09.4','O09.40','O09.41','O09.42','O09.43',
                          'O09.5','O09.51','O09.511','O09.512','O09.513','O09.519','O09.52','O09.521','O09.522','O09.523','O09.529',
                          'O09.6','O09.61','O09.611','O09.612','O09.613','O09.619','O09.62','O09.621','O09.622','O09.623','O09.629',
                          'O09.7','O09.70','O09.71','O09.72','O09.73',
                          'O09.8','O09.81','O09.811','O09.812','O09.813','O09.819','O09.82','O09.821','O09.822','O09.823','O09.829','O09.89','O09.891','O09.892','O09.893','O09.899',
                          'O09.9','O09.90','O09.91','O09.92','O09.93','O09.A','O09.A0','O09.A1','O09.A2','O09.A3'))  # 122 codes
  
  # xiao commented: changed from '45.001' to 'O45.001' in Apr 11, 2019
  # xiao commented: added '640.00','640.80','640.90','O33.7','O34.21','O34.51/2/3/9' in May 30, 2019
  # xiao commented: added ? codes, removed ? in Mar 16-21, 2020
  prenatal_BICD2<-paste(c('O20','O20.0','O20.8','O20.9',
                          '640.03','640.83','640.93',
                          '640.00','640.80','640.90',  # need to check?
                          '641.03','641.13','641.23','641.33','641.83','641.93',
                          '641.00','641.10','641.20','641.30','641.80','641.90',  # need to check?
                          'O44','O44.0','O44.00','O44.01','O44.02','O44.03','O44.1','O44.10','O44.11','O44.12','O44.13','O44.2','O44.20','O44.21','O44.22','O44.23',
                          'O44.3','O44.30','O44.31','O44.32','O44.33','O44.4','O44.40','O44.41','O44.42','O44.43','O44.5','O44.50','O44.51','O44.52','O44.53',
                          'O45.0','O45.00','O45.001','O45.002','O45.003','045.009','O45.01','O45.011','O45.012','O45.013','O45.019','O45.02','O45.021','O45.022','O45.023','O45.029',
                          'O45.09','O45.091','O45.092','O45.093','O45.099','045.8','O45.8X','O45.8X1','O45.8X2','O45.8X3','O45.8X9','O45.9','O45.90','O45.91','O45.92','O45.93',
                          'O46.0','O46.00','O46.001','O46.002','O46.003','045.009','O46.01','O46.011','O46.012','O46.013','O46.019','O46.02','O46.021','O46.022','O46.023','O46.029',
                          'O46.09','O46.091','O46.092','O46.093','O46.099','045.8','O46.8X','O46.8X1','O46.8X2','O46.8X3','O46.8X9','O46.9','O46.90','O46.91','O46.92','O46.93',
                          '642.03','642.13','642.23','642.33','642.43','642.53','642.63','642.73','642.93',
                          '642.00','642.10','642.20','642.30','642.40','642.50','642.60','642.70','642.90', # need to check?
                          'O10.011','O10.012','O10.013','O10.019','O10.111','O10.112','O10.113','O10.119','O10.211','O10.212','O10.213','O10.219',
                          'O10.311','O10.312','O10.313','O10.319','O10.411','O10.412','O10.413','O10.419','O10.911','O10.912','O10.913','O10.919',
                          'O11.1','O11.2','O11.3','O11.9',
                          'O13.1','O13.2','O13.3','o13.9',
                          'O14.00','O14.02','O14.03','O14.10','O14.12','O14.13','O14.20','O14.22','O14.23','O14.90','O14.92','O14.93',
                          'O15.00','O15.02','O15.03',
                          'O15.1','O15.9',  # need to check?
                          'O16.1','O16.2','O16.3','O16.9',
                          '643.03','643.13','643.23','643.83','643.93',
                          '643.00','643.10','643.20','643.80','643.90',  # need to check?
                          'O21','O21.0','O21.1','O21.2','O21.8','O21.9',
                          '644.03','644.13',
                          'O60.0','O60.00','O60.02','O60.03',
                          'O47.0','O47.00','O47.02','O47.03','O47.1','O47.9',
                          '645.10','645.13','645.20','645.23',
                          'O48','O48.0','O48.1',
                          '646.03','646.13','646.23','646.33','646.43','646.53','646.63','646.73','646.83','646.93',
                          '646.00','646.10','646.20','646.30','646.40','646.50','646.60','646.70','646.80','646.90',  # need to check?
                          'O31.01X0','O31.02X0','O31.03X0','O31.11X0','O31.31X0','O31.32X0','O31.33X0','O31.8X10','O31.8X20','O31.8X30',
                          # O31 family missing a lot of codes, need to check? see grey ones in excel file.
                          'O12.01','O12.02','O12.03','O12.21','O12.22','O12.23',
                          'O12.00','O12.10','O12.10','P12,11','P12.12','O12.13','O12.20',  # need to check?
                          'O23.41','O23.42','O23.43','O23.91','O23.92','O23.93',
                          # O23 family missing a lot of codes, need to check? 
                          'O26.01','O26.02','O26.03','O26.11','O26.12','O26.13','O26.21','O26.22','O26.23','O26.41','O26.42','O26.43','O26.611','O26.612','O26.613',
                          'O26.811','O26.812','O26.813','O26.821','O26.822','O26.823','O26.831','O26.832','O26.833','O26.891','O26.892','O26.893',
                          'O26.851','O26.852','O26.853','O26.841','O26.842','O26.843','O26.872','O26.873',
                          # O26 family missing a lot of codes, need to check? 
                          'O99.011','O99.012','O99.013','O99.111','O99.112','O99.113','O99.211','O99.212','O99.213','O99.281','O99.282','O99.283',
                          'O99.321','O99.322','O99.323','O99.331','O99.332','O99.333','O99.341','O99.342','O99.343','O99.351','O99.352','O99.353',
                          'O99.411','O99.412','O99.413','O99.810','O99.841','O99.842','O99.843',
                          'O99.89',  # I think this should be excluded, check?                   
                          # O99 family missing a lot of codes, need to check?
                          '647.03','647.13','647.23','647.33','647.43','647.53','647.63','647.83','647.93',
                          '647.00','647.10','647.20','647.30','647.40','647.50','647.60','647.80','647.90',  # need to check? 
                          'O98.011','O98.012','O98.013','O98.111','O98.112','O98.113','O98.211','O98.212','O98.213','O98.311','O98.312','O98.313',
                          'O98.611','O98.612','O98.613','O98.511','O98.512','O98.513','O98.811','O98.812','O98.813','O98.911','O98.912','O98.913',
                          'O98.019','O98.119','O98.219','O98.319','O98.411','O98.412','O98.413','O98.419','O98.519','O98.619','O98.711','O98.712','O98.713','O98.719','O98.819','O98.919',  # need to check?
                          '648.03','648.13','648.23','648.33','648.43','648.53','648.63','648.73','648.83','648.93',   
                          '648.00','648.10','648.20','648.30','648.40','648.50','648.60','648.70','648.80','648.90',  # need to check?
                          'O24.415','O24.419','O24.911','O24.912','O24.913', 
                          'O24.01','O24.011','O24.012','O24.013','O24.019','O24.11','O24.111','O24.112','O24.113','O24.119','O24.31','O24.311','O24.312','O24.313','O24.319',  # need to check?
                          'O24.41','O24.410','O24.414','O24.81','O24.811','O24.812','O24.813','O24.819','O24.91','O24.919',  # need to check?
                          'O33.0','O33.1','O33.2','O33.3XX0','O33.4XX0','O33.5XX0','O33.6XX0','O33.7','O33.7XX0','O33.7XX1','O33.7XX2','O33.7XX3','O33.7XX4','O33.7XX5','O33.7XX9','O33.8','O33.9',  
                          # O33 family missing a lot of codes, need to check?
                          'O25.11','O25.12','O25.13',
                          'O25.1','O25.10',  # need to check?
                          '649.03','649.13','649.23','649.33','649.43','649.53','649.63','649.73',
                          '649.00','649.10','649.20','649.30','649.40','649.50','649.60','649.70',  # need to check?
                          '651.03','651.13','651.23','651.33','651.43','651.53','651.63','651.73','651.83','651.93',
                          '651.00','651.10','651.20','651.30','651.40','651.50','651.60','651.70','651.80','651.90',  # need to check?
                          'O30.001','O30.002','O30.003','O30.021','O30.022','O30.023','O30.101','O30.102','O30.103','O30.201','O30.202','O30.203','O30.801','O30.802','O30.803','O30.91','O30.92','O30.93',
                          # O30 family missing a lot of codes, need to check?
                          '652.03','652.13','652.23','652.33','652.43','652.53','652.63','652.73','652.83','652.93',
                          '652.00','652.10','652.20','652.30','652.40','652.50','652.60','652.70','652.80','652.90',  # need to check?
                          'O32.0XX0','O32.1XX0','O32.2XX0','O32.3XX0','O32.4XX0','O32.6XX0','O32.8XX0','O32.9XX0',
                          # O32 family missing some codes, need to check?
                          '653.03','653.13','653.23','653.33','653.43','653.53','653.63','653.73','653.83','653.93',
                          '653.00','653.10','653.20','653.30','653.40','653.50','653.60','653.70','653.80','653.90',  # need to check?
                          '654.03','654.13','654.23','654.33','654.43','654.53','654.63','654.73','654.83','654.93',
                          '654.00','654.10','654.20','654.30','654.40','654.50','654.60','654.70','654.80','654.90',  # need to check?
                          'O34.01','O34.02','O34.03','O34.11','O34.12','O34.13','O34.21','O34.211','O34.212','O34.219','O34.29','O34.31','O34.32','O34.33',
                          'O34.41','O34.42','O34.43','O34.51','O34.511','O34.512','O34.513','O34.52','O34.521','O34.522','O34.523','O34.53','O34.531','O34.532','O34.533','O34.59','O34.591','O34.592','O34.593',
                          'O34.61','O34.62','O34.63','O34.71','O34.72','O34.73','O34.81','O34.82','O34.83','O34.91','O34.92','O34.93',
                          'O34', 'O34.0','O34.00','O34.1','O34.10','O34.2','O34.3','O34.30','O34.4','O34.40',  # need to check?
                          'O34.5','O34.519','O34.529','O34.539','O34.599','O34.6','O34.60','O34.7','34.70','O34.8','O34.80','O34.9','O34.90',  # need to check?
                          '655.03','655.13','655.23','655.33','655.43','655.53','655.63','655.73','655.83','655.93',
                          '655.00','655.10','655.20','655.30','655.40','655.50','655.60','655.70','655.80','655.90',  # need to check?
                          'O35.0XX0','O35.1XX0','O35.2XX0','O35.3XX0','O35.4XX0','O35.5XX0','O35.6XX0','O35.7XX0','O35.8XX0','O35.9XX0',
                          # O35 family missing a lot of codes, need to check? 
                          'O36.0110','O36.0120','O36.0130','O36.0910','O36.0920','O36.0930','O36.1110','O36.1120','O36.1130','O36.1910','O36.1920','O36.1930',
                          'O36.4XX0','O36.5110','O36.5120','O36.5130','O36.5910','O36.5920','O36.5930','O36.61X0','O36.62X0','O36.63X0',
                          'O36.80X0','O36.8120','O36.8130','O36.8190','O36.8210','O36.8220','O36.8230','O36.8910','O36.8920','O36.8930','O36.91X0','O36.92X0','O36.93X0',
                          # O35 family missing a lot of codes, need to check?
                          '656.03','656.13','656.23','656.33','656.43','656.53','656.63','656.73','656.83','656.93',               
                          '656.00','656.10','656.20','656.30','656.40','656.50','656.60','656.70','656.80','656.90',  # need to check?
                          'O68', 
                          'O43.011','O43.101','O43.102','O43.103','O43.811','O43.812','O43.813','O43.91','O43.92','O43.93',
                          # O43 family missing a lot of codes, need to check?
                          '657.03',
                          '657.00',  # need to check?
                          'O40.1XX0','O40.2XX0','O40.3XX0',
                          # O40 family missing some codes, need to check?
                          '658.03','658.13','658.23','658.33','658.43','658.83','658.93',
                          '658.00','658.10','658.20','658.30','658.40','658.80','658.90',  # need to check?
                          'O41.01X0','O41.02X0','O41.03X0','O41.1010','O41.1020','O41.1030','O41.1210','O41.1220','O41.1230','O41.1410','O41.1420',
                          'O41.1430','O41.8X10','O41.8X20','O41.8X30','O41.91X0','O41.92X0','O41.93X0',
                          # O41 family missing a lot of codes, need to check?
                          'O42.011','O42.012','O42.013','O42.111','O42.112','O42.113',
                          # O42 family missing some codes, need to check?
                          'O75.2','O75.3','O75.5','O75.89','O75.9',
                          # O75 family some codes are not sure?
                          'O61.0','O61.1',
                          'O61.8','O61.9',  # need to check?
                          'O76',
                          '659.03','659.13','659.23','659.33','659.43','659.53','659.63','659.73','659.83','659.93',
                          '659.00','659.10','659.20','659.30','659.40','659.50','659.60','659.70','659.80','659.90',  # need to check?
                          '678.03','678.13',
                          '678.00','678.10',  # need to check?
                          '679.03','679.13',
                          '679.00','679.10'  # need to check?
  ))   # 563 codes
  
  prenatal_BICD <- c(unique(prenatal_BICD0), unique(prenatal_BICD1), unique(prenatal_BICD2))   # 646
  
  # xiao changed from 76810 to 76801 in Apr 11, 2019 and added 80055 in May 30, 2019
  # xiao commented: removed 80055 and added 76802,76810,76812,76814,76819,75.35,68.19 in Mar 16, 2020
  # OB ultrasound CPT codes and prenatal CPT
  prenatal_BCPT <- paste(c('76801','76802','76805','76810','76811','76812','76813','76814','76815','76816','76817','76818','76819',
                           'BY4FZZZ','BY49ZZZ','BY4CZZZ','88.78','75.35','68.19'))
  
  # Q: B category codes are much more than NCQU document? Even after converting from ICD9 to ICD10
  prenatal_B <- c(prenatal_BICD, prenatal_BCPT)
  
  # CPT codes 
  # New Patient Office, Established Patient Office or Other Outpatient Services, New or Established Patient Office or Other Outpatient Consultation
  # Q: missing UB Revenue code 0514?
  prenatal_C<-paste(c('99201','99202','99203','99204','99205','99211','99212','99213','99214','99215','99241','99242','99243','99244','99245'))  # 15 codes
  
  # CPT, HCPCS and CPT category II codes
  # first 6 are delivery or antepartum codes - generally are used on the date of delivery, not the first date for OB care
  # so these codes are useful only if the claim form indicates when prenatal care was initiated
  prenatal_A<-paste(c('59400','59425','59426','59510','59610','59618','H1000','H1001','H1002','H1003','H1004','H1005','0500F','0501F','0502F'))  # 15 codes
  
  # ------------------------------------------------------------------------------------------- #
  # quick check of HCPCS codes in claims - no HCPCS codes were found!
  #DGN_cols <- rev(names(pchp_claims)[which(substr(names(pchp_claims),1,4) == "PROC")])
  #HCPCS <- paste0(c('H1000','H1001','H1002','H1003','H1004','H1005'), collapse = "|")
  #pchp_claims$HCPCS <- 0
  #for(col in DGN_cols) {
  #  index <- regexpr(HCPCS, pchp_claims[, col])
  #  pchp_claims[, "HCPCS"] <- ifelse(index != -1, 1, pchp_claims[, "HCPCS"])     
  #}
  #table(pchp_claims$HCPCS) 
  #foo <- pchp_claims %>% filter(HCPCS == 1)
  # ------------------------------------------------------------------------------------------- #
  
  # check ICD9/10 codes difference:
  # These codes have totally different meanings in ICD9/10, so we should specify 9 or 10 in ICD_Version_Indicator column in pchp_claims,
  # for example pchp_claims$DGN1 >= 493 & pchp_claims$DGN1 < 494 & pchp_claims$ICD_Version_Indicator==9,
  # otherwise we might get the wrong cohort. They mean some pregnancy in ICD-9-CM, but some kind of traffic injury in ICD-10-CM.
  temp <- pchp_claims %>% filter(DGN1 %in% prenatal_BICD0 | DGN2 %in% prenatal_BICD0 | DGN3 %in% prenatal_BICD0 | DGN4 %in% prenatal_BICD0 | DGN5 %in% prenatal_BICD0)
  if(nrow(temp) != 0){
    print(nrow(temp))
    #print(temp)
    print("There is ICD 9/10 code discrepancy, go back and check 'V22.0' etc.")
    # if DOS is after Ocd 1, 2015, it is fine
    print(temp %>% filter(DOS < as.Date("2015-10-01")))
  }
  
  # we need A or (B and C) to define prenatal visits
  t_claim$prenatal1<-as.numeric(t_claim$DGN1%in%prenatal_A | t_claim$DGN2%in%prenatal_A | t_claim$DGN3%in%prenatal_A | t_claim$DGN4%in%prenatal_A |
                                  t_claim$DGN5%in%prenatal_A | t_claim$PROC_1%in%prenatal_A | t_claim$PROC_2%in%prenatal_A | t_claim$PROC_3%in%prenatal_A | t_claim$PROC_4%in%prenatal_A)
  
  t_claim$prenatal2<-as.numeric((t_claim$DGN1%in%prenatal_C | t_claim$DGN2%in%prenatal_C | t_claim$DGN3%in%prenatal_C | t_claim$DGN4%in%prenatal_C |
                                   t_claim$DGN5%in%prenatal_C | t_claim$PROC_1%in%prenatal_C | t_claim$PROC_2%in%prenatal_C | t_claim$PROC_3%in%prenatal_C | t_claim$PROC_4%in%prenatal_C) 
                                &
                                  (t_claim$DGN1%in%prenatal_B | t_claim$DGN2%in%prenatal_B | t_claim$DGN3%in%prenatal_B | t_claim$DGN4%in%prenatal_B |
                                     t_claim$DGN5%in%prenatal_B | t_claim$PROC_1%in%prenatal_B | t_claim$PROC_2%in%prenatal_B | t_claim$PROC_3%in%prenatal_B | t_claim$PROC_4%in%prenatal_B))
  
  t_claim$prenatal <- as.numeric(t_claim$prenatal1 == 1 | t_claim$prenatal2 == 1)
  print(table(t_claim$prenatal))  # see the end of this function
  rm(temp)
  
  #### generate medical features ####
  
  # gestational diabetes (GD)
  # xiao corrected from "O23.434" to 'O24.434' and
  # added 'O24.41',"O24.9,'O24.91',"O24.919",'O99.81' in Jun 5, 2020
  # O24.0, O24.1, other codes in O24.3 and O24.8?
  ges_diabetes <- c('648.0','648.00','648.01','648.02','648.03','648.04','648.8','648.80','648.81','648.82','648.83','648.84',
                    'O24.319','O24.32',
                    'O24.4','O24.41','O24.410','O24.414','O24.415','O24.419','O24.42','O24.420','O24.424','O24.425','O24.429','O24.43','O24.430','O24.434','O24.435','O24.439',
                    'O24.9','024.91','O24.911','O24.912','O24.913','O24.919','O24.92','O24.93',
                    'O99.81','O99.810','O99.814','O99.815')
  
  t_claim$ges_diabetes <- as.numeric(t_claim$DGN1%in%ges_diabetes|t_claim$DGN2%in%ges_diabetes|t_claim$DGN3%in%ges_diabetes |t_claim$DGN4%in%ges_diabetes|t_claim$DGN5%in%ges_diabetes)
  
  # pregnancy-induced hypertension (PIH)
  # ICD9 642.30 to ICD10 O13.9                                        
  # ICD9 642.31 to ICD10 O13.1,O13.2,O13.3,O13.4,O16.1,O16.2,O16.3    
  # ICD9 642.32 to ICD10 O13.1,O13.2,O13.3,O13.5                      
  # ICD9 642.33 to ICD10 O13.1,O13.2,O13.3,O16.1,O16.2,O16.3          
  # ICD9 642.34 to ICD10 O13.1,O13.2,O13.3,O13.5                      
  # ICD9 642.40 to ICD10 O14.00,O14.90                                
  # ICD9 642.41 to ICD10 O14.02,O14.03,O14.04,O14.92,O14.93,O14.94    
  # ICD9 642.42 to ICD10 O14.02,O14.03,O14.05,O14.95                  
  # ICD9 642.43 to ICD10 O14.02,O14.03,O14.92,O14.93                  
  # ICD9 642.44 to ICD10 O14.05,O14.95,O15.2                          
  # ICD9 642.50 to ICD10 O14.10,O14.20                                
  # ICD9 642.51 to ICD10 O14.12,O14.13,O14.14,O14.22,O14.23,O14.24    
  # ICD9 642.52 to ICD10 O14.12,O14.13,O14.15,O14.22,O14.23,O14.25    
  # ICD9 642.53 to ICD10 O14.12,O14.13,O14.22,O14.23                  
  # ICD9 642.54 to ICD10 O14.12,O14.13,O14.15,O14.22,O14.23,O14.25    
  # ICD9 642.60 to ICD10 O15.9                                        
  # ICD9 642.61 to ICD10 O15.02,O15.03,O15.1                          
  # ICD9 642.62 to ICD10 O15.2                                        
  # ICD9 642.63 to ICD10 O15.02,O15.03                                
  # ICD9 642.64 to ICD10 O15.2                                        
  # ICD9 642.70 to ICD10 O11.9                                                      
  # ICD9 642.71 to ICD10 O11.1,O11.2,O11.3,O11.4                              
  # ICD9 642.72 to ICD10 O11.4                                       
  # ICD9 642.73 to ICD10 O11.1,O11.2,O11.3                            
  # ICD9 642.74 to ICD10 O11.5                                        
  # ICD9 642.90 to ICD10 O16.9                                        
  # ICD9 642.91 to ICD10 O16.1,O16.2,O16.3,O16.4                      
  # ICD9 642.92 to ICD10 O16.5,O16.9                                  
  # ICD9 642.93 to ICD10 O16.1,O16.2,O16.3                            
  # ICD9 642.94 to ICD10 O16.1,O16.2,O16.3,O16.5                      
  # ICD9 760.0  to ICD10 P00.0                                        
  # O13, O13.1, O13.2, O13.3, O13.9, O14.XX                     
  # O15.0 (xiao added in Apr 15, 2019)
  # OLD CRITERIA
  # t_claim$pi_hypertension <- rowSums((t_claim[,str_detect(names(t_claim),"DGN")] >= 642.3 & t_claim[,str_detect(names(t_claim),"DGN")] <= 642.99) |
  #                                    (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O14.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O14.99') |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 760.0 | t_claim[,str_detect(names(t_claim),"DGN")] == 'O13'|
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O13.1' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O13.2' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O13.3' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O13.9' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O13.9' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O13.1' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O13.2' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O13.3' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O13.4' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O13.5'|
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O16.1'| t_claim[,str_detect(names(t_claim),"DGN")] == 'O16.2' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O16.3' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.00' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.90' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.02' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.03' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.04' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.05' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.92' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.93'| t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.94' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.95'| t_claim[,str_detect(names(t_claim),"DGN")] == 'O15.2' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.10' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.20' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.12' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.13' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.14' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.22' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.23' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.24' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.15' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O14.25' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O15.9'| t_claim[,str_detect(names(t_claim),"DGN")] == 'O15.02' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O15.03'| t_claim[,str_detect(names(t_claim),"DGN")] == 'O15.2' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O11.9' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O11.1' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O11.2' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O11.3' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O11.4' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O11.5' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O16.9' | t_claim[,str_detect(names(t_claim),"DGN")] == 'O16.4' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O16.5' | t_claim[,str_detect(names(t_claim),"DGN")] == 'P00.0' |
  #                                     t_claim[,str_detect(names(t_claim),"DGN")] == 'O15.0')
  #
  # CHECK STR_DETECT CORRECTNESS in Jun 3, 2020
  # t_claim$PIH <- rowSums(t_claim[,str_detect(names(t_claims),"DGN")] >= 642.3 & t_claims[,str_detect(names(t_claims),"DGN")] <= 642.99)
  # table(t_claim$PIH)  # 41547 5096 202
  # t_claims %>% dplyr::filter(PIH == 1) %>% group_by(DGN1) %>% tally() %>% arrange(desc(n))
  # t_claims$PIH <- rowSums(t_claims[,str_detect(names(t_claims),"DGN")] == "760.0")  # must have quotation
  # table(t_claims$PIH)  # 0
  # t_claims$PIH <- rowSums(t_claims[,str_detect(names(t_claims),"DGN")] == 'P00.0')
  # table(t_claims$PIH)  # 16
  # t_claims$PIH <- rowSums(t_claims[,str_detect(names(t_claims),"DGN")] >= 'O14.00' & t_claims[,str_detect(names(t_claims),"DGN")] <= 'O14.99')
  # table(t_claims$PIH)  # 66084 3871 9
  # t_claims %>% dplyr::filter(PIH == 1) %>% group_by(DGN1) %>% tally() %>% arrange(desc(n))
  # t_claims$PIH <- rowSums(t_claims[,str_detect(names(t_claims),"DGN")] >= 'O16.00' & t_claims[,str_detect(names(t_claims),"DGN")] <= 'O16.99')
  # table(t_claims$PIH)  # 22613 3394
  # t_claims %>% dplyr::filter(PIH == 1) %>% group_by(DGN1) %>% tally() %>% arrange(desc(n))
  
  # NEW CRITERIA in Jun 4, 2020
  t_claim$pi_hypertension <- rowSums((t_claim[,str_detect(names(t_claim),"DGN")] >= 642.3 & t_claim[,str_detect(names(t_claim),"DGN")] <= 642.99) | 
                                       t_claim[,str_detect(names(t_claim),"DGN")] == "760.0" | t_claim[,str_detect(names(t_claim),"DGN")] == 'P00.0' |
                                       (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O11.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O11.99') |   
                                       (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O13.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O13.99') |
                                       (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O14.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O14.99') | 
                                       (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O15.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O15.99') |
                                       (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O16.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O16.99'))
  t_claim$pi_hypertension <- as.numeric(t_claim$pi_hypertension > 0)
  
  # xiao added the last 7 codes in Apr 15, 2019
  cervix <- paste(c('649.70','649.71','649.73','654.50','O26.879','O26.872','O26.873','O34.30','654.51', '654.52', '654.53', '654.54', 'O34.31', 'O34.32', 'O34.33'))
  
  t_claim$cervix<-as.numeric(t_claim$DGN1%in%cervix| t_claim$DGN2%in%cervix| t_claim$DGN3%in%cervix | t_claim$DGN4%in%cervix| t_claim$DGN5%in%cervix)
  
  # xiao added "67.59" in Mar 29, 2019
  Cerc <- c('67.5','67.51','67.59','0UVC0CZ','0UVC0DZ','0UVC0ZZ','0UVC3CZ','0UVC3DZ','0UVC3ZZ','0UVC4CZ','0UVC4DZ','0UVC4ZZ','0UVC7DZ','0UVC7ZZ','0UVC8DZ','0UVC8ZZ','59320')
  
  t_claim$Cerc<-as.numeric(t_claim$DGN1%in%Cerc| t_claim$DGN2%in%Cerc| t_claim$DGN3%in%Cerc | t_claim$DGN4%in%Cerc| t_claim$DGN5%in%Cerc|
                             t_claim$PROC_1%in%Cerc| t_claim$PROC_2%in%Cerc| t_claim$PROC_3%in%Cerc| t_claim$PROC_4%in%Cerc)
  
  UTI <- paste(c('N39.0','599.0'))
  
  t_claim$UTI<-as.numeric(t_claim$DGN1%in%UTI| t_claim$DGN2%in%UTI| t_claim$DGN3%in%UTI | t_claim$DGN4%in%UTI| t_claim$DGN5%in%UTI)
  
  t_claim$Cyst <- rowSums((t_claim[,str_detect(names(t_claim),"DGN")] >= 'N30.00' & t_claim[,str_detect(names(t_claim),"DGN")] <='N30.99') |
                            t_claim[,str_detect(names(t_claim),"DGN")] == 595.0 | t_claim[,str_detect(names(t_claim),"DGN")] == 595.1 |
                            t_claim[,str_detect(names(t_claim),"DGN")] == 595.2 | t_claim[,str_detect(names(t_claim),"DGN")] == 595.3 |
                            t_claim[,str_detect(names(t_claim),"DGN")] == 595.82 | t_claim[,str_detect(names(t_claim),"DGN")] == 595.89|
                            t_claim[,str_detect(names(t_claim),"DGN")] == 595.4 | t_claim[,str_detect(names(t_claim),"DGN")] == 595.81 |
                            t_claim[,str_detect(names(t_claim),"DGN")] == 595.9)
  
  t_claim$Cyst <- as.numeric(t_claim$Cyst > 0)
  
  
  t_claim$Uret <- rowSums( t_claim[,str_detect(names(t_claim),"DGN")] == 'N34.0'|
                             t_claim[,str_detect(names(t_claim),"DGN")] == 'N34.1' | t_claim[,str_detect(names(t_claim),"DGN")] == 'N34.2' |
                             t_claim[,str_detect(names(t_claim),"DGN")] == 'N34.3' | t_claim[,str_detect(names(t_claim),"DGN")] == 597.0 |
                             t_claim[,str_detect(names(t_claim),"DGN")] == '099.40' | t_claim[,str_detect(names(t_claim),"DGN")] == 597.80 |
                             t_claim[,str_detect(names(t_claim),"DGN")] == 597.89 | t_claim[,str_detect(names(t_claim),"DGN")] == 597.81)
  
  t_claim$Uret <- as.numeric(t_claim$Uret > 0)
  
  Asympt <- paste(c('R82.71','791.9'))
  
  t_claim$asympt <- as.numeric(t_claim$DGN1%in%Asympt | t_claim$DGN2%in%Asympt | t_claim$DGN3%in%Asympt | t_claim$DGN4%in%Asympt | t_claim$DGN5%in%Asympt)
  
  t_claim$Bact <- rowSums(t_claim[,str_detect(names(t_claim),"DGN")] >= 'O23.00' & t_claim[,str_detect(names(t_claim),"DGN")] <='O23.999')
  
  t_claim$Bact <- as.numeric(t_claim$Bact > 0)
  
  # threatened abortion
  # xiao added 'O20' in Jun 3, 2020
  abortion <- paste(c('O20','O20.0','O20.8','O20.9','640.03','640.83','640.93'))
  
  t_claim$abortion <- as.numeric(t_claim$DGN1%in%abortion | t_claim$DGN2%in%abortion | t_claim$DGN3%in%abortion | t_claim$DGN4%in%abortion | t_claim$DGN5%in%abortion)
  
  # preterm labor history, how is it different from Prehist1?
  # supervision of pregnancy with history of preterm labor
  Prehist2 <- paste(c('Z87.51','V13.21','O60.00','O09.211','O09.212','O09.213','O09.219','V23.41'))
  
  t_claim$Prehist2 <- as.numeric(t_claim$DGN1%in%Prehist2| t_claim$DGN2%in%Prehist2| t_claim$DGN3%in%Prehist2 | t_claim$DGN4%in%Prehist2| t_claim$DGN5%in%Prehist2)
  
  # OLD CRITERIA
  # Exclude <- paste(c('Z33.2','O03','O03.0','O03.1','O03.2','O03.3','O03.30','O03.31','O03.32','O03.33','O03.34','O03.35','O03.36','O03.37','O03.38','O03.39',
  #                    'O03.4','O03.5','O03.6','O03.7','O03.8','O03.80','O03.81','O03.82','O03.83','O03.84','O03.85','O03.86','O03.87','O03.88','O03.89','O03.9','Z37.1',
  #                    '635.90','634.01','634.11','634.61','634.81','634.51','634.31','634.41','634.21','634.71','634.01','634.91','634.00','634.02','634.10','634.12',
  #                    '634.60','634.62','634.80','634.82','634.50','634.52','634.30','634.32','634.40','634.42','634.20','634.22','634.70','634.72','634.00','634.02',
  #                    '634.90','634.92','V27.1'))  # old
  # t_claim$exclude <- as.numeric(t_claim$DGN1%in%Exclude | t_claim$DGN2%in%Exclude | t_claim$DGN3%in%Exclude | t_claim$DGN4%in%Exclude | t_claim$DGN5%in%Exclude)
  
  # NEW CRITERIA - xiao modified in Jun 4, 2020
  library(readxl)
  #stillbirth <- read_excel("//sc-acs228-1.parknet-ad.pmh.org/users$/p89514/Documents/XiaoWang/Xiao_PCCI/Preterm/data/delivery_codes.xlsx", sheet = "complete_delivery") %>% filter(stillbirth == "stillbirth") %>% select(code) # 33
  #stillbirth <- read_excel("D:/PCHP PreTerm/monthly_report/delivery_codes.xlsx", sheet = "complete_delivery") %>% filter(stillbirth == "stillbirth")
  stillbirth <- read_excel("/home/michelle/T-Drive/PCHP PreTerm/Data/delivery_codes.xlsx", sheet = "complete_delivery") %>% filter(stillbirth == "stillbirth")%>% select(code) # 33
  abortion_DGN <- paste0(c("O03","632","634","635","637"), collapse = "|")
  abortion_DGN_CPT <- c('Z33.2',"59812","59820","59821","59830","59840","59841","59850","59851","59852","59855","59856","59857")  # 13
  remove_list <- c(abortion_DGN_CPT, stillbirth$code)  # 46
  
  t_claim$exclude <- 0
  DGN_cols <- rev(names(t_claim)[which(substr(names(t_claim),1,3) == "DGN")])
  for(col in DGN_cols){
    index <- regexpr(abortion_DGN, t_claim[, col])
    t_claim[, "exclude"] <- ifelse(index != -1, 1, t_claim[, "exclude"])     
  }
  t_claim$exclude <- ifelse(t_claim$exclude == 0, 
                            ifelse(t_claim$DGN1 %in% remove_list | t_claim$DGN2 %in% remove_list | t_claim$DGN3 %in% remove_list | 
                                     t_claim$DGN4 %in% remove_list | t_claim$DGN5 %in% remove_list | t_claim$PROC_1 %in% remove_list | 
                                     t_claim$PROC_2 %in% remove_list | t_claim$PROC_3 %in% remove_list | t_claim$PROC_4 %in% remove_list, 
                                   1, t_claim$exclude), t_claim$exclude)
  
  
  t_claim$out_pre <- as.numeric(t_claim$out == 1 | t_claim$prenatal == 1)
  t_claim$ED_inpt <- as.numeric(t_claim$ED == 1 | t_claim$inpt == 1)
  
  
  # to combine variables generated from current pregnancy time window in this step with results from step 1&2
  
  # generate a new date to exclude patients
  # initialize monthstart to be "2017-01-01"
  # update monthstart by the following rules -
  # if RiskID is NA, use first prenatal visit in HEDIS criteria as monthstart, start time that you have prenatal-related hospital visit (prenatal, ED, inpt, outpt)
  # if RiskID is not NA, use effective date in elg as monthstart
  pchp_delivery_P$monthstart <- as.Date("2017-01-01")
  pchp_delivery_P$monthstart[which(is.na(pchp_delivery_P$RiskGroupID))] <- pchp_delivery_P$first_HEDISDate[which(is.na(pchp_delivery_P$RiskGroupID))]
  pchp_delivery_P$monthstart[which(!is.na(pchp_delivery_P$RiskGroupID))] <- pchp_delivery_P$EffectiveDate[which(!is.na(pchp_delivery_P$RiskGroupID))]
  
  
  t_claim_111 <- plyr::join(t_claim[,c('DOS','ATT_PROV_ID','PaidAmount','MEMBER_ID','inpt','ED','outpt','prenatal','out_pre','ED_inpt','ges_diabetes','pi_hypertension',
                                       'cervix','Cerc','UTI','Cyst','Uret','asympt','Bact','abortion','Prehist2','exclude')], pchp_delivery_P, type = 'left', by ='MEMBER_ID') 
  
  
  #----------------------------------------------#  
  #### exclude patients from "exclude" column ####
  #----------------------------------------------#  
  
  t_claim_00 <- sqldf("select * from t_claim_111 WHERE DOS <= Anchor AND DOS > monthstart")
  t_claim_00 <- t_claim_00[!duplicated(t_claim_00), ]   # check duplicates, takes LONG time
  # if there exists exclude code, it is 1; otherwise, it is 0
  t_claim_exclude <- sqldf("select MEMBER_ID, Anchor, Pregnancy, max(exclude) as exclude1 from t_claim_00 group by MEMBER_ID")  # 7887 rows
  names(t_claim_exclude)[4] <- c('exclude')
  rm(t_claim_00)
  
  
  #### combine features ####
  
  # filter within study time window
  t_claim_1111 <- sqldf("select * from t_claim_111 WHERE DOS > Pregnancy AND DOS <= Anchor")
  t_claim_1111 <- t_claim_1111[!duplicated(t_claim_1111), ] # check duplicates
  
  t_claim_1111 <- t_claim_1111[order(t_claim_1111$PaidAmount, decreasing = T), ]
  
  t_claim_previsit <- sqldf("select MEMBER_ID,DOB,DOS,ATT_PROV_ID,Anchor,Pregnancy, sum(inpt)>0 as inpt_b, sum(ED)>0 as ED_b, sum(outpt)>0 as outpt_b, 
                            sum(prenatal)>0 as prenatal_b, sum(out_pre)>0 as out_pre_b, sum(ED_inpt)>0 as ED_inpt_b, sum(UTI)>0 as UTI_b, sum(Cyst)>0 as Cyst_b, 
                            sum(Uret)>0 as Uret_b, sum(asympt)>0 as asympt_b, sum(Bact)>0 as Bact_b, sum(abortion)>0 as abortion_b, sum(ges_diabetes)>0 as ges_diabetes_b, 
                            sum(pi_hypertension)>0 as pi_hypertension_b, sum(cervix)>0 as cervix_b, sum(Cerc)>0 as Cerc_b, sum(Prehist2)>0 as Prehist2_b, 
                            sum(PaidAmount * prenatal) as paidPre, sum(PaidAmount * inpt) as paidinpt, sum(PaidAmount * ED) as paided, sum(PaidAmount * outpt) as paidout 
                            from t_claim_1111 group by MEMBER_ID, DOS")
  
  t_claim_previsit1 <- sqldf("select MEMBER_ID,DOB,Anchor,Pregnancy, sum(inpt_b) as num_inpt, sum(ED_b) as num_ED, sum(outpt_b) as num_outpt, sum(prenatal_b) as num_pre, 
                             sum(out_pre_b) as num_out_pre, sum(ED_inpt_b) as num_ED_inpt, sum(UTI_b)>0 as UTI_now, sum(Cyst_b)>0 as Cyst_now, sum(Uret_b)>0 as Uret_now, 
                             sum(asympt_b)>0 as asympt_now, sum(Bact_b)>0 as Bact_now, sum(abortion_b)>0 as abortion_now, sum(ges_diabetes_b)>0 as ges_diabetes_now, 
                             sum(pi_hypertension_b)>0 as pi_hypertension_now, sum(cervix_b)>0 as cervix_now, sum(Cerc_b)>0 as Cerc_now, sum(Prehist2_b)>0 as Prehist2, 
                             sum(paidPre) as Pre_pd, sum(paidinpt) as Inpt_pd, sum(paided) as ED_pd, sum(paidout) as Out_pd 
                             from t_claim_previsit group by MEMBER_ID")
  
  
  # exclude these two codes, which are LAB TESTS instead of providers
  t_claim_previsit_new <- t_claim_previsit[!t_claim_previsit$ATT_PROV_ID %in% c('PROV0000P05180', 'PROV0000P12161'), ]
  
  #--------------------------------------------------#  
  #### extract prenatal-related visit information ####
  #--------------------------------------------------#  
  t_claim_previsit_prenatal <- t_claim_previsit_new[order(t_claim_previsit_new$MEMBER_ID, t_claim_previsit_new$DOS), ]
  t_claim_previsit_prenatal <- t_claim_previsit_prenatal[t_claim_previsit_prenatal$prenatal_b == 1, ]
  
  t_claim_previsit_recent <- t_claim_previsit_prenatal %>% group_by(MEMBER_ID) %>% arrange(MEMBER_ID, DOS) %>% filter(row_number() == n())
  #t_claim_previsit_recent <- t_claim_previsit_prenatal %>% group_by(MEMBER_ID) %>% filter(row_number() == 1)   # if sorted descending
  
  t_claim_previsit_recent <- data.frame(t_claim_previsit_recent)
  # ATT_PROV_ID associated with the most recent prenatal visit
  t_claim_previsit_recent$Recent_pre_PROV_ID <- t_claim_previsit_recent$ATT_PROV_ID
  
  t_claim_previsit11 <- sqldf("select MEMBER_ID, DOB, Anchor, Pregnancy, max(DOS) as recent_Previsit, min(DOS) as first_Previsit from t_claim_previsit where prenatal_b==1 group by MEMBER_ID")
  t_claim_previsit11$recent_Previsit <- as.Date(t_claim_previsit11$recent_Previsit, origin = "1970-01-01")
  t_claim_previsit11$first_Previsit <- as.Date(t_claim_previsit11$first_Previsit, origin = "1970-01-01")
  
  t_claim_previsit11 <- plyr::join(t_claim_previsit11, t_claim_previsit_recent[, c('MEMBER_ID','Recent_pre_PROV_ID')], type = 'left', by = 'MEMBER_ID') 
  rm(t_claim_previsit_prenatal, t_claim_previsit_recent)
  
  #----------------------------------------------------#  
  #### extract outpatient-related visit information ####
  #----------------------------------------------------#  
  t_claim_previsit_outpt <- t_claim_previsit_new[order(t_claim_previsit_new$MEMBER_ID, t_claim_previsit_new$DOS), ]
  t_claim_previsit_outpt <- t_claim_previsit_outpt[t_claim_previsit_outpt$outpt_b == 1, ]
  
  t_claim_previsit_recent_1 <- t_claim_previsit_outpt %>% group_by(MEMBER_ID) %>% arrange(MEMBER_ID, DOS) %>% filter(row_number() == n())
  
  t_claim_previsit_recent_1 <- data.frame(t_claim_previsit_recent_1)
  t_claim_previsit_recent_1$Recent_out_PROV_ID <- t_claim_previsit_recent_1$ATT_PROV_ID
  
  t_claim_outpt22 <- sqldf("select MEMBER_ID,DOB,Anchor,Pregnancy,max(DOS) as recent_outpt, min(DOS) as first_outpt from t_claim_previsit where outpt_b == 1 group by MEMBER_ID")
  t_claim_outpt22$recent_outpt <- as.Date(t_claim_outpt22$recent_outpt, origin = "1970-01-01")
  t_claim_outpt22$first_outpt <- as.Date(t_claim_outpt22$first_outpt, origin = "1970-01-01")
  
  t_claim_outpt22 <- plyr::join(t_claim_outpt22, t_claim_previsit_recent_1[, c('MEMBER_ID', 'Recent_out_PROV_ID')], type = 'left', by = 'MEMBER_ID') 
  rm(t_claim_previsit_outpt, t_claim_previsit_recent_1)
  
  #--------------------------------------------#  
  #### extract ED-related visit information ####
  #--------------------------------------------#  
  # no need to find the most recent doctor for ED visit
  t_claim_ED33 <- sqldf("select MEMBER_ID, DOB, Anchor, Pregnancy, max(DOS) as recent_ED, min(DOS) as first_ED from t_claim_previsit where ED_b == 1 group by MEMBER_ID")
  t_claim_ED33$recent_ED <- as.Date(t_claim_ED33$recent_ED, origin = "1970-01-01")
  t_claim_ED33$first_ED <- as.Date(t_claim_ED33$first_ED, origin = "1970-01-01")
  
  #---------------------------------------------------#  
  #### extract inpatient-related visit information ####
  #---------------------------------------------------#  
  # no need to find the most recent doctor for inpatient visit either
  t_claim_inpt44 <- sqldf("select MEMBER_ID, DOB, Anchor, Pregnancy, max(DOS) as recent_inpt, min(DOS) as first_inpt from t_claim_previsit where inpt_b == 1 group by MEMBER_ID")
  t_claim_inpt44$recent_inpt <- as.Date(t_claim_inpt44$recent_inpt, origin = "1970-01-01")
  t_claim_inpt44$first_inpt <- as.Date(t_claim_inpt44$first_inpt, origin = "1970-01-01")
  
  #--------------------------------------------------------#  
  #### combine prenatal/outpt/ED/inpt visit information ####
  #--------------------------------------------------------# 
  t_claim_previsit111 <- plyr::join(t_claim_previsit1, t_claim_previsit11[,c('MEMBER_ID','recent_Previsit','first_Previsit','Recent_pre_PROV_ID')], 
                                    type = 'left', by = 'MEMBER_ID') 
  
  t_claim_previsit111 <- plyr::join(t_claim_previsit111, t_claim_outpt22[,c('MEMBER_ID','recent_outpt','first_outpt','Recent_out_PROV_ID')], 
                                    type = 'left', by = 'MEMBER_ID') 
  
  t_claim_previsit111 <- plyr::join(t_claim_previsit111, t_claim_ED33[,c('MEMBER_ID','recent_ED','first_ED')],
                                    type = 'left', by = 'MEMBER_ID') 
  
  t_claim_previsit111 <- plyr::join(t_claim_previsit111, t_claim_inpt44[,c('MEMBER_ID','recent_inpt','first_inpt')],
                                    type = 'left', by = 'MEMBER_ID') 
  
  
  Current_P <- plyr::join(pchp_delivery_P, t_claim_previsit111[,c('MEMBER_ID','num_inpt','num_ED','num_outpt','num_pre','num_out_pre','num_ED_inpt','UTI_now','Cyst_now',
                                                                  'Uret_now','asympt_now','Bact_now','abortion_now','ges_diabetes_now','pi_hypertension_now','cervix_now',
                                                                  'Cerc_now','Pre_pd','Inpt_pd','ED_pd','Out_pd','recent_Previsit','first_Previsit','recent_outpt','first_outpt',
                                                                  'Recent_pre_PROV_ID','Recent_out_PROV_ID','recent_ED','first_ED','recent_inpt','first_inpt','Prehist2')],
                          type = 'left', by = 'MEMBER_ID') 
  
  
  Current_P <- plyr::join(Current_P, t_claim_exclude[, c('MEMBER_ID','exclude')], type = 'left', by = 'MEMBER_ID') 
  
  return(Current_P)
}

# CHANGE dates and P index below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Cohort_P42_Updated_AddDate.RData")
#load("D:/PCHP PreTerm/monthly_report/PCHP_Pregnant_Cohort_P42_Updated_AddDate.RData")

Cohort_Current_P42 <- Application_Claim_Current(start_dt, end_dt, pchp_claims, Cohort_P42_Add)  # might find ICD9/10 discrepency
# print(table(t_claim$prenatal))  # 29303,28933,30537,29026,28906,28278,27930,28421,27449,26637,25713,25114,
length(unique(Cohort_Current_P42$MEMBER_ID))  # expect the same as next line
length(Cohort_Current_P42$MEMBER_ID)   
# 6910,6977,7138,7275,7366,7468,7342,7276,7219,7339,6823,6585,7260,
rm(Cohort_P42_Add, Application_Claim_Current, pchp_claims)
save(Cohort_Current_P42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Current_Cohort_P42_Updated.RData")
#save(Cohort_Current_P42, file = "D:/PCHP PreTerm/monthly_report/PCHP_Current_Cohort_P42_Updated.RData")
rm(Cohort_Current_P42)


#### PCHP Preterm Project Step 4 ####
# Generate historical variables prior to pregnancy time window, behavior and medical comorbidities variables 

# load 5-year claims data
# CHANGE the dates below 
#load("/home/michelle/T-Drive/PCHP PreTerm/Data/Accumulating Claim Data for generating monthly report/PCHP_claim_01012016_DATEwithoutmultiple.RData")
load("/home/michelle/preterm_monthly_report/PCHP_claim_01012016_02282021withoutmultiple.RData")
range(pchp_claims$DOS)  # expect 5-year

Application_Claim_History <- function(Pregnancy1 = as.Date(start_dt), Anchor1 = as.Date(end_dt), pchp_claims, pchp_delivery_P){
  
  # age between [11, 55], to filter mom's claim
  #DOB_max <- Anchor1 - years(11) 
  DOB_max <- Anchor1 - as.difftime(as.integer(365.25*11), units = "days")
  #DOB_min <- Anchor1 - years(55)  
  DOB_min <- Anchor1 - as.difftime(as.integer(365.25*55), units = "days")
  # to filter women in pregnant cohort defined in step 1&2
  pchp_claims <- pchp_claims[pchp_claims$MEMBER_ID %in% pchp_delivery_P$MEMBER_ID, ] 
  # to filter study time window, no need to filter DOS >= Pregnancy condition! 
  # because some historical variables are searched before Anchor, while others are searched before Pregnancy, which we will specify later
  pchp_claims <- pchp_claims[!is.na(pchp_claims$DOS) & (pchp_claims$DOS <= as.Date(Anchor1)) & 
                               (pchp_claims$DOB <= as.Date(DOB_max)) & (pchp_claims$DOB >= as.Date(DOB_min)), ]
  t_claim <- pchp_claims
  rm(pchp_claims)
  
  #### generate medical features ####
  
  # NEW CRITERIA in Jun 5, 2020
  ges_diabetes <- c('648.0','648.00','648.01','648.02','648.03','648.04','648.8','648.80','648.81','648.82','648.83','648.84',
                    'O24.319','O24.32',
                    'O24.4','O24.41','O24.410','O24.414','O24.415','O24.419','O24.42','O24.420','O24.424','O24.425','O24.429','O24.43','O24.430','O24.434','O24.435','O24.439',
                    'O24.9','024.91','O24.911','O24.912','O24.913','O24.919','O24.92','O24.93',
                    'O99.81','O99.810','O99.814','O99.815')
  
  t_claim$ges_diabetes <- as.numeric(t_claim$DGN1%in%ges_diabetes | t_claim$DGN2%in%ges_diabetes | t_claim$DGN3%in%ges_diabetes | t_claim$DGN4%in%ges_diabetes | t_claim$DGN5%in%ges_diabetes)
  
  # NEW CRITERIA in Jun 4, 2020
  t_claim$pi_hypertension <- rowSums((t_claim[,str_detect(names(t_claim),"DGN")] >= 642.3 & t_claim[,str_detect(names(t_claim),"DGN")] <= 642.99) | 
                                       t_claim[,str_detect(names(t_claim),"DGN")] == "760.0" | t_claim[,str_detect(names(t_claim),"DGN")] == 'P00.0' |
                                       (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O11.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O11.99') |   
                                       (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O13.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O13.99') |
                                       (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O14.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O14.99') | 
                                       (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O15.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O15.99') |
                                       (t_claim[,str_detect(names(t_claim),"DGN")] >= 'O16.00' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'O16.99'))
  
  t_claim$pi_hypertension <- as.numeric(t_claim$pi_hypertension > 0)
  
  # xiao added the last 7 codes in Apr 15, 2019
  cervix <- paste(c('649.70','649.71','649.73','654.50','O26.879','O26.872','O26.873','O34.30','654.51', '654.52', '654.53', '654.54', 'O34.31', 'O34.32', 'O34.33'))
  
  t_claim$cervix <- as.numeric(t_claim$DGN1%in%cervix | t_claim$DGN2%in%cervix | t_claim$DGN3%in%cervix | t_claim$DGN4%in%cervix | t_claim$DGN5%in%cervix)
  
  # xiao added the last 7 in Apr 15, 2019
  Cerc <- c('67.5','67.51','67.59','0UVC0CZ','0UVC0DZ','0UVC0ZZ','0UVC3CZ','0UVC3DZ','0UVC3ZZ','0UVC4CZ','0UVC4DZ','0UVC4ZZ','0UVC7DZ','0UVC7ZZ','0UVC8DZ','0UVC8ZZ','59320')
  
  t_claim$Cerc <- as.numeric(t_claim$DGN1%in%Cerc | t_claim$DGN2%in%Cerc | t_claim$DGN3%in%Cerc | t_claim$DGN4%in%Cerc | 
                               t_claim$DGN5%in%Cerc | t_claim$PROC_1%in%Cerc | t_claim$PROC_2%in%Cerc | t_claim$PROC_3%in%Cerc | t_claim$PROC_4%in%Cerc)
  
  # diabetes mellitus (DM)
  # ICD10: E08-E13, no E12, diatebes mellitus
  # ICD9: 249 and 250, diabetes mellitus
  # xiao added "250.21" in Apr 15, 2019, added all missing codes in 249 and 250 families in Mar 17, 2020
  # Q: added 357.2,362.01,362.02,362.03,362.04,362.05,362.06,362.07,366.41?
  # OLD CRITERIA
  # t_claim$ho_diabetes1 <- rowSums((t_claim[,str_detect(names(t_claim),"DGN")] >= 'E08' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'E13') |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 249.00 | t_claim[,str_detect(names(t_claim),"DGN")] == 249.01 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 249.10 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.11 | 
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 249.20 | t_claim[,str_detect(names(t_claim),"DGN")] == 249.21 | 
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 249.30 | t_claim[,str_detect(names(t_claim),"DGN")] == 249.31 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 249.40 | t_claim[,str_detect(names(t_claim),"DGN")] == 249.41 | 
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 249.50 | t_claim[,str_detect(names(t_claim),"DGN")] == 249.51 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 249.60 | t_claim[,str_detect(names(t_claim),"DGN")] == 249.61 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 249.70 | t_claim[,str_detect(names(t_claim),"DGN")] == 249.71 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 249.80 | t_claim[,str_detect(names(t_claim),"DGN")] == 249.81 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 249.90 | t_claim[,str_detect(names(t_claim),"DGN")] == 249.91 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.00 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.01 | 
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.02 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.03 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.10 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.11 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.12 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.13 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.20 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.21 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.22 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.23 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.30 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.31 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.32 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.33 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.40 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.41 | 
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.42 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.43 | 
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.50 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.51 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.52 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.53 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.60 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.61 | 
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.62 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.63 | 
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.70 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.71 |                                    t_claim[,str_detect(names(t_claim),"DGN")] == 250.72 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.73 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.80 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.81 | 
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.82 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.83 | 
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.90 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.91 |
  #                                 t_claim[,str_detect(names(t_claim),"DGN")] == 250.92 | t_claim[,str_detect(names(t_claim),"DGN")] == 250.93 )
  # NEW CRITERIA in Jun 5, 2020
  t_claim$ho_diabetes1 <- rowSums((t_claim[,str_detect(names(t_claim),"DGN")] >= 'E08.0' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'E13.9') |
                                    (t_claim[,str_detect(names(t_claim),"DGN")] >= 249.00 & t_claim[,str_detect(names(t_claim),"DGN")] == 250.99))
  
  t_claim$ho_diabetes1 <- as.numeric(t_claim$ho_diabetes1 > 0)
  # remove them because they are medication/chemical induced
  ho_diabetes_delete <- paste(c('E09.9','E09.65','249.00','249.80'))
  t_claim$ho_diabetes_delete <- as.numeric(t_claim$DGN1%in%ho_diabetes_delete | t_claim$DGN2%in%ho_diabetes_delete | 
                                             t_claim$DGN3%in%ho_diabetes_delete | t_claim$DGN4%in%ho_diabetes_delete | t_claim$DGN5%in%ho_diabetes_delete)
  
  t_claim$ho_diabetes <- as.numeric(t_claim$ho_diabetes1 == 1 & t_claim$ho_diabetes_delete == 0 & t_claim$ges_diabetes == 0)
  
  # history of hypertension/chronic hypertension (CHTN)
  # ICD10: I10-I16, no I14, hypertensive diseases
  # ICD9: 401-405 hypertensive diseases
  # xiao removed 405.00 in Mar 17, 2020
  # Q: add 362.11, 437.2, I67.4, H35.03, H35.031, H35.032, H35.033, H35.039? I16 family???
  # OLD CRITERIA
  # t_claim$ho_hypertension1 <- rowSums(( t_claim[,str_detect(names(t_claim),"DGN")] >= 'I10' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'I15') |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 401.0 | t_claim[,str_detect(names(t_claim),"DGN")] == 401.1 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 401.9 | t_claim[,str_detect(names(t_claim),"DGN")] == 402.00 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 402.01 | t_claim[,str_detect(names(t_claim),"DGN")] == 402.10 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 402.11 | t_claim[,str_detect(names(t_claim),"DGN")] == 402.90 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 402.91 | t_claim[,str_detect(names(t_claim),"DGN")] == 403.00 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 403.01 | t_claim[,str_detect(names(t_claim),"DGN")] == 403.10 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 403.11 | t_claim[,str_detect(names(t_claim),"DGN")] == 403.90 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 403.91 | t_claim[,str_detect(names(t_claim),"DGN")] == 404.00 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 404.01 | t_claim[,str_detect(names(t_claim),"DGN")] == 404.02 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 404.03 | t_claim[,str_detect(names(t_claim),"DGN")] == 404.10 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 404.11 | t_claim[,str_detect(names(t_claim),"DGN")] == 404.12 | 
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 404.13 | t_claim[,str_detect(names(t_claim),"DGN")] == 404.90 |
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 404.91 | t_claim[,str_detect(names(t_claim),"DGN")] == 404.92 | 
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 404.93 | t_claim[,str_detect(names(t_claim),"DGN")] == 405.01 | 
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 405.09 | t_claim[,str_detect(names(t_claim),"DGN")] == 405.11 | 
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 405.19 | t_claim[,str_detect(names(t_claim),"DGN")] == 405.91 | 
  #                                       t_claim[,str_detect(names(t_claim),"DGN")] == 405.99)
  #
  # NEW CRITERIA in Jun 5, 2020
  t_claim$ho_hypertension1 <- rowSums((t_claim[,str_detect(names(t_claim),"DGN")] >= 'I10' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'I15.9') |
                                        (t_claim[,str_detect(names(t_claim),"DGN")] >= 401.0 & t_claim[,str_detect(names(t_claim),"DGN")] <= 405.99) )
  t_claim$ho_hypertension1 <- as.numeric(t_claim$ho_hypertension1 > 0)
  # CHTN but not PIH
  t_claim$ho_hypertension <- as.numeric(t_claim$ho_hypertension1 == 1 & t_claim$pi_hypertension == 0)
  
  
  mood <- paste(c('F06.30','293.83'))
  
  t_claim$mood <- as.numeric(t_claim$DGN1%in%mood | t_claim$DGN2%in%mood | t_claim$DGN3%in%mood | t_claim$DGN4%in%mood | t_claim$DGN5%in%mood)
  
  anxiety <- paste(c('F06.4','293.84'))
  
  t_claim$anxiety <- as.numeric(t_claim$DGN1%in%anxiety | t_claim$DGN2%in%anxiety | t_claim$DGN3%in%anxiety | t_claim$DGN4%in%anxiety | t_claim$DGN5%in%anxiety)
  
  t_claim$depression <- rowSums(         (t_claim[,str_detect(names(t_claim),"DGN")] >= 296.00 & t_claim[,str_detect(names(t_claim),"DGN")] <=296.99) |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 311 |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F30.10' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F30.11' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F30.12' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F30.13' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F30.2' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F30.3' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F30.4' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F32.0' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F32.1' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F32.2' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F32.3' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F32.4' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F32.5' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F32.9' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F33.9' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F33.0' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F33.1' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F33.2' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F33.3' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F33.41' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F33.42' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.10' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.11' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.12' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.13' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.2' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.73' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.74' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.30' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.31' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.32' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.4' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.5' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.75' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.76' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.60' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.61' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.62' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.63' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.64' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.77' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.78' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.9' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F30.8' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F32.89' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F31.81' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F39' | t_claim[,str_detect(names(t_claim),"DGN")] == 'F34.81' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == 'F34.89')
  
  t_claim$depression<-as.numeric(t_claim$depression>0)
  
  psy <- paste(c('F20.89','F22','F32.3','F33.3','295','297','298'))
  
  t_claim$psy <- as.numeric(t_claim$DGN1%in%psy | t_claim$DGN2%in%psy | t_claim$DGN3%in%psy | t_claim$DGN4%in%psy | t_claim$DGN5%in%psy)
  
  alcohol <- paste(c('303.0','303.9','303.00','303.01','303.02','303.03','303.90','303.91','303.92','303.93',
                     'F10.229','F10.20','F10.21','571.0','571.1','571.2','571.3','291.0','291.1','291.2','291.3','291.4','291.5',
                     'K70.0','K70.10','K70.30','K70.9','F10.96','F10.27','F10.951','F10.929','F10.950','F10.231'))
  
  t_claim$alcohol <- as.numeric(t_claim$DGN1%in%alcohol | t_claim$DGN2%in%alcohol | t_claim$DGN3%in%alcohol | t_claim$DGN4%in%alcohol | t_claim$DGN5%in%alcohol)
  
  Substance <- paste(c('304.00','304.0','F11.20','304.01','304.02', '304.03','F11.21','304.1','304.10','F13.20','304.11','304.12','304.13','F13.21',
                       '304.2','304.20','F14.20','304.21','304.22','304.23','F14.21','304.3','304.30','F12.20','304.31','304.32','304.33','F12.21',                                     
                       '304.4','304.40','F15.20','304.41','304.42','304.43','F15.21','304.5','304.50','F16.20','304.51','304.52','304.53','F16.21','304.6','304.60',
                       'F19.20','304.61','304.62','304.63','F19.21','304.7','304.70','F19.20','304.71','304.72','304.73','F19.21',                                     
                       '304.8','304.80','F19.20','304.81','304.82','304.83','F19.21','304.9','304.90','F19.20','304.91','304.92','304.93','F19.21','305.90','F18.10',                                     
                       '292.0','F19.939','292.1','292.11','F19.950','292.12','F19.951','292.2','F15.920','292.8','292.81','F19.921',                              
                       '292.82','F19.97','292.83','F19.96','292.84','F19.94','292.85','F11.182','F11.282','F11.982','F13.182','F13.282',                       
                       'F13.982','F14.182','F14.282','F14.982','F15.182','F15.282','F15.982','F19.182','F19.282','F19.982',                        
                       '292.89','F11.159','F11.181','F11.188','F11.222','F11.259','F11.281','F11.288','F11.922','F11.959','F11.981',    
                       'F11.988','F12.122','F12.159','F12.180','F12.188','F12.222','F12.259','F12.280','F12.288','F12.922',    
                       'F12.959','F12.980','F12.988','F13.159','F13.180','F13.181','F13.188','F13.259','F13.280','F13.281',    
                       'F13.288','F13.959','F13.980','F13.981','F13.988','F14.122','F14.159','F14.180','F14.181','F14.188',    
                       'F14.222','F14.259','F14.280','F14.281','F14.288','F14.922','F14.959','F14.980','F14.981','F14.988',    
                       'F15.122','F15.159','F15.180','F15.181','F15.188','F15.222','F15.259','F15.280','F15.281','F15.288',    
                       'F15.922','F15.959','F15.980','F15.981','F15.988','F16.122','F16.159','F16.180','F16.183','F16.188',     
                       'F16.259','F16.280','F16.283','F16.288','F16.959','F16.980','F16.983','F16.988','F17.208','F17.218',    
                       'F17.228','F17.298','F18.159','F18.180','F18.188','F18.259','F18.280','F18.288','F18.959','F18.980','F18.988','F19.122','F19.159','F19.180','F19.181',    
                       'F19.188','F19.222','F19.259','F19.280','F19.281','F19.288','F19.922','F19.959','F19.980','F19.981','F19.988','292.9','F19.99'))
  
  t_claim$substance <- as.numeric(t_claim$DGN1%in%Substance | t_claim$DGN2%in%Substance | t_claim$DGN3%in%Substance | t_claim$DGN4%in%Substance | t_claim$DGN5%in%Substance)
  
  Smoking <- paste(c('305.1','649.00','649.01','649.02','649.03','649.04','989.84','F17.200','O99.330','O99.331','O99.332','O99.333','O99.334','O99.335',
                     'T65.211A','T65.212A','T65.213A','T65.214A','T65.221A','T65.222A','T65.223A','T65.224A','T65.291A','T65.292A','T65.293A','T65.294A',
                     '99406','99407','S9075','S9453'))
  
  t_claim$smoking <- as.numeric(t_claim$DGN1%in%Smoking | t_claim$DGN2%in%Smoking | t_claim$DGN3%in%Smoking | t_claim$DGN4%in%Smoking | t_claim$DGN5%in%Smoking |
                                  t_claim$PROC_1%in%Smoking | t_claim$PROC_2%in%Smoking | t_claim$PROC_3%in%Smoking | t_claim$PROC_4%in%Smoking)
  
  uterine <- paste(c('654.00','654.01','654.02','654.03','654.04','O34.00','O34.01','O34.02','O34.03','O34.04'))
  
  t_claim$uterine <- as.numeric(t_claim$DGN1%in%uterine | t_claim$DGN2%in%uterine | t_claim$DGN3%in%uterine | t_claim$DGN4%in%uterine | t_claim$DGN5%in%uterine)
  
  t_claim$sex_trans <- rowSums(          (t_claim[,str_detect(names(t_claim),"DGN")] >= 'A50' & t_claim[,str_detect(names(t_claim),"DGN")] <='A64') |
                                           t_claim[,str_detect(names(t_claim),"DGN")] =='090.0'|t_claim[,str_detect(names(t_claim),"DGN")] == '090.1'|
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '090.2' | t_claim[,str_detect(names(t_claim),"DGN")] == '090.5' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '090.40' | t_claim[,str_detect(names(t_claim),"DGN")] == '090.41' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '090.42' | t_claim[,str_detect(names(t_claim),"DGN")] == '090.49' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '090.6' | t_claim[,str_detect(names(t_claim),"DGN")] == '090.7' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '090.9' | t_claim[,str_detect(names(t_claim),"DGN")] == '091.0'|
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '091.1'| t_claim[,str_detect(names(t_claim),"DGN")] == '091.2' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '091.3' | t_claim[,str_detect(names(t_claim),"DGN")] == '091.82' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '091.81' | t_claim[,str_detect(names(t_claim),"DGN")] == '091.89' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '091.50' | t_claim[,str_detect(names(t_claim),"DGN")] == '091.51' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '091.52' | t_claim[,str_detect(names(t_claim),"DGN")] == '091.61' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '091.62'| t_claim[,str_detect(names(t_claim),"DGN")] == '091.4' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '091.69'| t_claim[,str_detect(names(t_claim),"DGN")] == '091.7' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '092.0' | t_claim[,str_detect(names(t_claim),"DGN")] == '092.9' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '093.9' | t_claim[,str_detect(names(t_claim),"DGN")] == '093.0' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '093.1' | t_claim[,str_detect(names(t_claim),"DGN")] == '093.20' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '093.21' | t_claim[,str_detect(names(t_claim),"DGN")] == '093.22' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '093.23' | t_claim[,str_detect(names(t_claim),"DGN")] == '093.24' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '093.89'| t_claim[,str_detect(names(t_claim),"DGN")] == '093.81' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '093.82'| t_claim[,str_detect(names(t_claim),"DGN")] == '094.9' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '094.0' | t_claim[,str_detect(names(t_claim),"DGN")] == '094.89' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '094.2' | t_claim[,str_detect(names(t_claim),"DGN")] == '094.81' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '094.84' | t_claim[,str_detect(names(t_claim),"DGN")] == '094.85' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '094.86' | t_claim[,str_detect(names(t_claim),"DGN")] == '094.1' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '094.82' | t_claim[,str_detect(names(t_claim),"DGN")] == '094.83'|
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '094.3' | t_claim[,str_detect(names(t_claim),"DGN")] == '094.9' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '095.0' | t_claim[,str_detect(names(t_claim),"DGN")] == '095.1' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '095.2' | t_claim[,str_detect(names(t_claim),"DGN")] == '095.3' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '095.4' | t_claim[,str_detect(names(t_claim),"DGN")] == '095.5' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '095.6' | t_claim[,str_detect(names(t_claim),"DGN")] == '095.7' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '095.8' | t_claim[,str_detect(names(t_claim),"DGN")] == '095.9' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '096' | t_claim[,str_detect(names(t_claim),"DGN")] == '097.0' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '097.1' | t_claim[,str_detect(names(t_claim),"DGN")] == '097.9' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.0' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.2' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.11' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.31' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.15' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.35' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.19' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.12' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.32' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.13' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.14' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.33' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.34' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.16' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.36' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.49' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.40' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.41' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.43' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.42' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.49' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.59' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.53' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.50' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.51' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.52' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.6' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.7' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.82' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.89' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.83' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.84' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.85' | t_claim[,str_detect(names(t_claim),"DGN")] == '098.86' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '098.81' | t_claim[,str_detect(names(t_claim),"DGN")] == '099.1' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '099.53' | t_claim[,str_detect(names(t_claim),"DGN")] == '099.54' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '099.55' | t_claim[,str_detect(names(t_claim),"DGN")] == '099.52' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '099.51' | t_claim[,str_detect(names(t_claim),"DGN")] == '099.59' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '099.0' | t_claim[,str_detect(names(t_claim),"DGN")] == '099.2' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '131.00' | t_claim[,str_detect(names(t_claim),"DGN")] == '131.01' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '131.02' | t_claim[,str_detect(names(t_claim),"DGN")] == '131.03' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '131.09' | t_claim[,str_detect(names(t_claim),"DGN")] == '131.8' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '131.9' | t_claim[,str_detect(names(t_claim),"DGN")] == '054.10' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '054.13' | t_claim[,str_detect(names(t_claim),"DGN")] == '054.19' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '054.11' | t_claim[,str_detect(names(t_claim),"DGN")] == '054.12' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '078.11' | t_claim[,str_detect(names(t_claim),"DGN")] == '099.8' |
                                           t_claim[,str_detect(names(t_claim),"DGN")] == '099.9')
  
  t_claim$sex_trans <- as.numeric(t_claim$sex_trans > 0)
  
  t_claim$HIV <- rowSums((t_claim[,str_detect(names(t_claim),"DGN")] >= 'B20' & t_claim[,str_detect(names(t_claim),"DGN")] <= 'B24') |
                           t_claim[,str_detect(names(t_claim),"DGN")] == '042')
  
  t_claim$HIV <- as.numeric(t_claim$HIV > 0)
  
  Abor_his <- paste(c('Z87.5','Z87.51','Z87.59','V13.21','V13.1','V13.29','N96','629.81','O03.9','634.90','634.92'))
  
  t_claim$Abor_his <- as.numeric(t_claim$DGN1%in%Abor_his | t_claim$DGN2%in%Abor_his | t_claim$DGN3%in%Abor_his | t_claim$DGN4%in%Abor_his | t_claim$DGN5%in%Abor_his)
  
  # preterm labor history 
  # xiao added the last 1 code in Jun 4, 2020
  Prehist1 <- c('644.0','644.00','644.03','O60.00','O60.02','O60.03','O60')
  
  # preterm delivery history
  # xiao added the last 26 codes in Jun 4, 2020
  Prehist11 <- c('644.2','644.20','644.21','O60.10X0','O60.12X0','O60.13X0','O60.14X0','060.1','O60.2','O60.10X1','O60.10X2',
                 'O60.10X3','O60.10X4','O60.10X5','O60.10X9','O60.12X1','O60.12X2','O60.12X3','O60.12X4','O60.12X5','O60.12X9',
                 'O60.13X1','O60.13X2','O60.13X3','O60.13X4','O60.13X5','O60.13X9','O60.14X1','O60.14X2','O60.14X3','O60.14X4',
                 'O60.14X5','O60.14X9')
  
  t_claim$Prehist1 <- as.numeric(t_claim$DGN1%in%Prehist1 | t_claim$DGN2%in%Prehist1 | t_claim$DGN3%in%Prehist1 | t_claim$DGN4%in%Prehist1 | t_claim$DGN5%in%Prehist1)
  t_claim$Prehist11 <- as.numeric(t_claim$DGN1%in%Prehist11 | t_claim$DGN2%in%Prehist11 | t_claim$DGN3%in%Prehist11 | t_claim$DGN4%in%Prehist11 | t_claim$DGN5%in%Prehist11)
  
  
  # to combine above 19 historical variables generated in this step prior to pregnancy time window with results from step 1&2, not from step 3
  
  t_claim_1 <- plyr::join(t_claim[,c('DOS','MEMBER_ID','ges_diabetes','pi_hypertension','cervix','Cerc','ho_diabetes','ho_hypertension','mood','anxiety',
                                     'depression','psy','alcohol','substance','smoking','uterine','sex_trans','HIV','Abor_his','Prehist1','Prehist11')], 
                          pchp_delivery_P, type = 'left', by = 'MEMBER_ID') 
  
  # subgroup 1: among these 19 historical variables, we filter 12 of them with time window - before Anchor
  # so we only filter by DOS <= Anchor, this condition is already filtered before in line 39
  t_claim_11 <- sqldf("select * from t_claim_1 WHERE DOS <= Anchor")    # redundant
  t_claim_11 <- t_claim_11[!duplicated(t_claim_11), ]   # check duplicates, takes LONG
  
  pchp_claim_behavior_medical<-sqldf("select MEMBER_ID, Anchor, Pregnancy, max(ho_diabetes) as ho_diabetes1, max(ho_hypertension) as ho_hypertension1, 
                                     max(mood) as mood1, max(anxiety) as anxiety1, max(depression) as depression1, max(psy) as psy1, 
                                     max(alcohol) as alcohol1, max(substance) as substance1, max(smoking) as smoking1, max(uterine) as uterine1, 
                                     max(sex_trans) as sex_trans1, max(HIV) as HIV1 
                                     from t_claim_11 group by MEMBER_ID") 
  
  names(pchp_claim_behavior_medical)[4:15] <- c('ho_diabetes','ho_hypertension','mood','anxiety','depression','psy','alcohol','substance','smoking','uterine','sex_trans','HIV')
  
  # subgroup 2: among these 19 historical variables, we filter the rest 7=19-12 with time window - before Pregnancy
  # before Pregnancy is a subset of before Anchor, so we further filter by DOS <= Pregnancy
  t_claim_111 <- sqldf("select * from t_claim_1 WHERE DOS <= Pregnancy")
  t_claim_111 <- t_claim_111[!duplicated(t_claim_111), ] # check duplicates, takes LONG time
  
  pchp_claim_obstetric <- sqldf("select MEMBER_ID, DOS, Anchor, Pregnancy, sum(ges_diabetes)>0 as ges_diabetes_b, sum(pi_hypertension) as pi_hypertension_b,
                                sum(cervix) as cervix_b, sum(Cerc) as Cerc_b, sum(Abor_his)>0 as Abor_his_b, sum(Prehist1)>0 as Prehist1_b, 
                                sum(Prehist11)>0 as Prehist11_b 
                                from t_claim_111 group by MEMBER_ID, DOS")
  
  pchp_claim_obstetric1 <- sqldf("select MEMBER_ID, Anchor, Pregnancy, sum(ges_diabetes_b)>0 as ges_diabetes, sum(pi_hypertension_b)>0 as pi_hypertension,
                                 sum(cervix_b)>0 as cervix, sum(Cerc_b)>0 as Cerc, sum(Abor_his_b)>0 as Abor_his, sum(Prehist1_b)>0 as Prehist1, 
                                 sum(Prehist11_b)>0 as Prehist11 
                                 from pchp_claim_obstetric group by MEMBER_ID")
  rm(t_claim_11, t_claim_111)
  
  Current_P <- plyr::join(pchp_delivery_P, pchp_claim_behavior_medical[, c('MEMBER_ID','ho_diabetes','ho_hypertension','mood','anxiety','depression','psy','alcohol','substance',
                                                                           'smoking','uterine','sex_trans','HIV')], type = 'left', by = 'MEMBER_ID') 
  
  Current_P <- plyr::join(Current_P, pchp_claim_obstetric1[, c('MEMBER_ID','ges_diabetes','pi_hypertension','cervix','Cerc','Abor_his','Prehist1','Prehist11')], type = 'left', by='MEMBER_ID')
  
  return(Current_P)
}

# CHANGE dates and P index below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Cohort_P42_Updated_AddDate.RData")
Cohort_History_P42 <- Application_Claim_History(start_dt, end_dt, pchp_claims, Cohort_P42_Add)
save(Cohort_History_P42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_History_Cohort_P42_Updated_AddDate.RData")
rm(Cohort_P42_Add,Cohort_History_P42,Application_Claim_History)


#### PCHP Preterm Project Step 5 ####
# Generate preterm labor & delivery history variables from mom's previous delivered baby records  

range(pchp_claims$DOS)  # expect 5-year, otherwise reload

Application_Claim_Pretermhistory <- function(Pregnancy1 = as.Date(start_dt), Anchor1 = as.Date(end_dt), t_claim, pchp_delivery_P, Link1){
  
  # to filter for baby's claims by age < 11
  #DOB_cutoff <- Anchor1 - years(11)  
  DOB_cutoff <- Anchor1 - as.difftime(as.integer(365.25*11), units = "days")
  # no need to filter DOS >= Pregnancy condition!
  t_claim <- t_claim[(!is.na(t_claim$DOS)) & (t_claim$DOS <= as.Date(Anchor1)) & (t_claim$DOB > as.Date(DOB_cutoff)), ]
  
  prehist <- paste(c('765.0','765.00','P07.00','P07.10','765.01','P07.01','765.02','P07.02','765.03','P07.03','765.04','P07.14','765.05','P07.15','765.06','P07.16',                                     
                     '765.07','P07.17','765.08','P07.18','765.09','P07.30','765.1','765.10','P07.00','P07.10','765.11','P07.01','765.12','P07.02','765.13','P07.03',                                     
                     '765.14','P07.14','765.15','P07.15','765.16','P07.16','765.17','P07.17','765.18','P07.18','765.19','P07.30','765.2','765.20','P07.20','P07.30',                        
                     '765.21','P07.21','P07.22','765.22','P07.23','765.23','P07.24','P07.25','765.24','P07.26','P07.31','765.25','P07.32','P07.33','765.26','P07.34',
                     'P07.35','765.27','P07.36','P07.37','765.28','P07.38','P07.39','765.29'))   # 72 -> 60 unique
  
  t_claim$prehist <- as.numeric(t_claim$DGN1%in%prehist | t_claim$DGN2%in%prehist | t_claim$DGN3%in%prehist | t_claim$DGN4%in%prehist | t_claim$DGN5%in%prehist |
                                  t_claim$PROC_1%in%prehist | t_claim$PROC_2%in%prehist | t_claim$PROC_3%in%prehist | t_claim$PROC_4%in%prehist)
  # to filter baby claims with baby preterm ICD code == 1
  t_claim <- t_claim[t_claim$prehist == 1, ]
  # left join with Link file from step 1&2 to get the baby's matching momIDs, key is babyID
  t_claim_baby_pre <- plyr::join(t_claim, Link1[, c('MEMBER_ID', 'MommyLink_ID')], type = 'left', by = 'MEMBER_ID') 
  rm(t_claim)
  t_claim_baby_pre <- t_claim_baby_pre[!is.na(t_claim_baby_pre$MommyLink_ID), ]
  t_claim_baby_pre <- t_claim_baby_pre[!duplicated(t_claim_baby_pre), ]
  
  # to filter women in pregnant cohort defined in step 1&2
  t_claim_baby_pre <- t_claim_baby_pre[t_claim_baby_pre$MommyLink_ID %in% pchp_delivery_P$MEMBER_ID, ]
  t_claim_baby_pre <- t_claim_baby_pre[!is.na(t_claim_baby_pre$MommyLink_ID), ]  # redundant!
  t_claim_baby_pre <- t_claim_baby_pre[!duplicated(t_claim_baby_pre), ] # redundant!
  # the baby's DOB is the mom's previous delivery date for that baby
  t_claim_baby_pre$Delivery1 <- t_claim_baby_pre$DOB
  t_claim_baby_pre$key <- t_claim_baby_pre$MommyLink_ID
  
  pchp_delivery_P$key <- pchp_delivery_P$MEMBER_ID
  
  Count <- plyr::join(t_claim_baby_pre[, c('key','Delivery1')], pchp_delivery_P, type = 'left', by = 'key') 
  
  # to filter previous babyDOB before prediction time, momDOB before babyDOB
  Count1 <- sqldf("select * from Count WHERE Delivery1 < Anchor and Delivery1 > DOB")  # no rows are removed!
  Count1 <- Count1[!duplicated(Count1), ] 
  
  # to compute mom's age when delivered the previous baby
  Count1$PreAge <- floor(as.numeric(Count1$Delivery1-Count1$DOB)/365.25)
  
  # when mom delivered this previous baby, mom's age should be >= 11
  # otherwise mom's claims and this earlier baby's claims will be confused again
  # this line of code is to exclude baby's claim
  Count1 <- Count1[Count1$PreAge >= 11, ]  
  
  Prehist_ID <- unique(Count1$MEMBER_ID)  # some moms have more than one babies delivered in the past
  pchp_delivery_P$Prehist <- 0
  pchp_delivery_P$Prehist[pchp_delivery_P$MEMBER_ID %in% Prehist_ID] <- 1
  
  return(pchp_delivery_P)
}

# CHANGE dates and P index below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Cohort_P42_Updated_AddDate.RData")
# load processed Link file in step 1&2 directly, called Link, CHANGE date
load("/home/michelle/T-Drive/PCHP PreTerm/Data/Mom_Baby_Link/Link_03142021_processed.RData")
PretermHistory_P42 <- Application_Claim_Pretermhistory(start_dt, end_dt, pchp_claims, Cohort_P42_Add, Link)
table(PretermHistory_P42$Prehist)  # 200,223,215,219,209,211,205,207,220,
save(PretermHistory_P42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_PretermHistory_Cohort_P42_Updated_AddDate.RData")
#save(PretermHistory_P42, file = "D:/PCHP PreTerm/monthly_report/PCHP_PretermHistory_Cohort_P42_Updated_AddDate.RData")
rm(Cohort_P42_Add,PretermHistory_P42,Link,Application_Claim_Pretermhistory,pchp_claims)


#### PCHP Preterm Project Step 6 ####
# Generate medication 17OH use during current pregnancy time window (not before current pregnancy/in history)

# load Pharmacy data from Asthma data folder, data loaded is called pchp_rx
# CHANGE month below
#load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/PCHP_rx_new_MONTH.RData")
load("/home/michelle/preterm_monthly_report/PCHP_rx_new_February.RData")
# check data quality
range(pchp_rx$SERVICE_DT)  # "2015-01-01" "2021-02-28"
pchp_rx$MEMBER_ID <- as.numeric(pchp_rx$MEM_UNIQUE_ID)  # NA introduced

Application_DrugCurrent_Generation <- function(Pregnancy1 = as.Date(start_dt), Anchor1 = as.Date(end_dt), t_rx, Final1){
  
  # to filter women in pregnant cohort defined in step 1&2
  t_rx <- t_rx[t_rx$MEMBER_ID %in% Final1$MEMBER_ID, ]
  # to extract rx information before study starting time
  t_rx <- t_rx[t_rx$SERVICE_DT <= Anchor1, ]  
  
  t_rx <- sqldf("select MEMBER_ID, SERVICE_DT, DRUG_PRDCT_NAME, GENERIC_PRDCT_NAME, APPRVD_GROSS_DUE, MEM_BIRTH_DT from t_rx")  # NA introduced
  t_rx <- t_rx[!duplicated(t_rx), ]
  t_rx <- t_rx[!is.na(t_rx$SERVICE_DT), ]
  
  # METHOD 1
  #t_rx$OH_17<-as.numeric(str_detect(t_rx$GENERIC_PRDCT_NAME,c("MEDROXYPROGESTERONE ACETA")) | str_detect(t_rx$GENERIC_PRDCT_NAME,c("PROGEST"))
  #                       |str_detect(t_rx$DRUG_PRDCT_NAME,c("PROGESTERONE")) | str_detect(t_rx$DRUG_PRDCT_NAME,c("MEDROXYPROGESTERONE ACETA")))
  #t_rx_use<-t_rx[t_rx$OH_17==1,c('DRUG_PRDCT_NAME','GENERIC_PRDCT_NAME')]
  
  # METHOD 2
  t_rx$OH_17 <- as.numeric((str_detect(t_rx$GENERIC_PRDCT_NAME, c("PROGEST")) | str_detect(t_rx$DRUG_PRDCT_NAME, c("PROGESTERONE"))) & 
                             !(str_detect(t_rx$GENERIC_PRDCT_NAME, c("^MEDROXY"))))
  
  t_rx_1 <- plyr::join(t_rx, Final1[, c('DOB','Anchor','Pregnancy','MEMBER_ID')], type = 'left', by = 'MEMBER_ID') 
  
  # need to check this condition, these two dates are supposed to be the same
  # if not equal, it should be a mistake/typo because of data quality or confusion between mom's DOB and baby's DOB
  # if one of them is NA, the other one is not, and after checking, it is indeed a reasonable mom's DOB, then we could keep that DOB
  t_rx_1 <- t_rx_1[t_rx_1$MEM_BIRTH_DT == t_rx_1$DOB, ]  # from 69380 rows to 69328 rows, one NA + one specific date situation should NOT be removed!
  t_rx_1 <- t_rx_1[!duplicated(t_rx_1), ]  # some completely NA rows are removed
  rm(t_rx)
  
  t_rx_11 <- sqldf("select MEMBER_ID, SERVICE_DT, DOB, sum(APPRVD_GROSS_DUE) as gross, sum(OH_17) as OH_17_b 
                   from t_rx_1 where SERVICE_DT > Pregnancy AND SERVICE_DT <= Anchor group by MEMBER_ID, SERVICE_DT")
  
  # CODE CHANGE -
  # xiao changed the code below from OH_17_b = 1 into OH_17_b >= 1 since P18
  # otherwise for example, Sep2018, missing 2 patients and 8 observations are different!
  t_rx_111_17OH <- sqldf("select MEMBER_ID, max(SERVICE_DT) as max_DOS_17OH, min(SERVICE_DT) as min_DOS_17OH, DOB, sum(gross) as Totalgross_now, sum(OH_17_b) as num_OH_17_now 
                         from t_rx_11 where OH_17_b >= 1 group by MEMBER_ID")
  t_rx_111_17OH$max_DOS_17OH <- as.Date(t_rx_111_17OH$max_DOS_17OH, origin = "1970-01-01")
  t_rx_111_17OH$min_DOS_17OH <- as.Date(t_rx_111_17OH$min_DOS_17OH, origin = "1970-01-01")
  
  t_rx_111 <- sqldf("select MEMBER_ID, max(SERVICE_DT) as max_DOS, DOB, sum(gross) as Totalgross_now, sum(OH_17_b) as num_OH_17_now 
                    from t_rx_11 group by MEMBER_ID")
  t_rx_111$max_DOS <- as.Date(t_rx_111$max_DOS, origin = "1970-01-01")
  
  Final1 <- plyr::join(Final1, t_rx_111[, c('Totalgross_now','num_OH_17_now','MEMBER_ID')], type = 'left', by = 'MEMBER_ID') 
  Final1 <- Final1[!duplicated(Final1), ]
  
  Final1 <- plyr::join(Final1, t_rx_111_17OH[, c('max_DOS_17OH','min_DOS_17OH','MEMBER_ID')], type = 'left', by = 'MEMBER_ID') 
  Final1 <- Final1[!duplicated(Final1), ]
  
  return(Final1)
}

# CHANGE dates and P index below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Cohort_P42_Updated_AddDate.RData")
DrugP42 <- Application_DrugCurrent_Generation(start_dt, end_dt, pchp_rx, Cohort_P42_Add)   # NA introduced
DrugP42 %>% filter(num_OH_17_now >= 1) %>% tally()  # 125,125,137,134,140,137,149,159,160,154,151,148,
save(DrugP42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_DrugCurrent_Cohort_P42_Updated_AddDate.RData")
rm(DrugP42,Cohort_P42_Add,Application_DrugCurrent_Generation,pchp_rx)


#### PCHP Preterm Project Step 7 ####
# Generate nutrition variables in the past year

# load 1.5-year claims data
# CHANGE dates below
#load("/home/michelle/T-Drive/PCHP PreTerm/Data/Accumulating Claim Data for generating monthly report/PCHP_claims_DATE1_DATE2.RData")
load("/home/michelle/preterm_monthly_report/PCHP_claims_20190731_02232021.RData")
range(pchp_claims$DOS)  # expect 1.5-year

Application_Claim_Nutrition <- function(Pregnancy1 = as.Date(start_dt), Anchor1 = as.Date(end_dt), t_claim, pchp_delivery_P){
  
  # age between [11, 55], to filter mom's claim
  #DOB_max <- Anchor1 - years(11) 
  DOB_max <- Anchor1 - as.difftime(as.integer(365.25*11), units = "days")
  #DOB_min <- Anchor1 - years(55)  
  DOB_min <- Anchor1 - as.difftime(as.integer(365.25*55), units = "days")
  # to filter women in pregnant cohort defined in step 1&2
  t_claim <- pchp_claims[pchp_claims$MEMBER_ID %in% pchp_delivery_P$MEMBER_ID, ]  
  
  # to filter study time window, no need to filter DOS >= Pregnancy condition! 
  # filter condition in the past year later
  t_claim <- t_claim[!is.na(t_claim$DOS) & (t_claim$DOS <= as.Date(Anchor1)) & (t_claim$DOB <= as.Date(DOB_max)) & (t_claim$DOB >= as.Date(DOB_min)), ]
  t_claim <- data.frame(t_claim)
  
  Malnutrition <- paste(c('263.9','E46'))
  
  t_claim$malnutrition <- as.numeric(t_claim$DGN1%in%Malnutrition | t_claim$DGN2%in%Malnutrition | t_claim$DGN3%in%Malnutrition | t_claim$DGN4%in%Malnutrition | t_claim$DGN5%in%Malnutrition)
  
  Obe_Over <- paste(c('278.00','278.01','278.02','278.03','E66.9','E66.01','E66.3','E66.2'))
  
  t_claim$obe_over <- as.numeric(t_claim$DGN1%in%Obe_Over | t_claim$DGN2%in%Obe_Over | t_claim$DGN3%in%Obe_Over | t_claim$DGN4%in%Obe_Over | t_claim$DGN5%in%Obe_Over)
  
  t_claim_1 <- plyr::join(t_claim[, c('DOS','MEMBER_ID','malnutrition','obe_over')], pchp_delivery_P, type = 'left', by = 'MEMBER_ID') 
  # generate past 1 year timestamp
  t_claim_1$back12 <- t_claim_1$Anchor - days(365)
  
  t_claim_11 <- sqldf("select * from t_claim_1 WHERE DOS <= Anchor and DOS >= back12")
  t_claim_11 <- t_claim_11[!duplicated(t_claim_11), ]   # remove duplicates
  
  pchp_claim_nutrition <- sqldf("select MEMBER_ID, Anchor, Pregnancy, max(malnutrition) as malnutrition1, max(obe_over) as obe_over1 
                                from t_claim_11 group by MEMBER_ID") 
  names(pchp_claim_nutrition)[names(pchp_claim_nutrition) %in% c('malnutrition1','obe_over1')] <- c('malnutrition','obe_over')
  
  Final1 <- plyr::join(pchp_delivery_P, pchp_claim_nutrition[, c('malnutrition','obe_over','MEMBER_ID')], type = 'left', by = 'MEMBER_ID') 
  
  return(Final1)
}

# CHANGE dates and P index below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Cohort_P42_Updated_AddDate.RData")
Nutrition_Cohort_P42 <- Application_Claim_Nutrition(start_dt, end_dt, pchp_claims, Cohort_P42_Add)
table(Nutrition_Cohort_P42$malnutrition)  # 1,1,1,1,1,0,0,0,0,
table(Nutrition_Cohort_P42$obe_over)  # 483,504,536,527,585,616,613,605,630,
save(Nutrition_Cohort_P42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Nutrition_Cohort_P42_Updated_AddDate.RData")
rm(Cohort_P42_Add,Nutrition_Cohort_P42,Application_Claim_Nutrition,pchp_claims)


#### PCHP Preterm Project Step 8 ####
# Generate insurance and its gap related variables, i.e.
# the number of gaps and total gap days in two time windows, which might be different for the women who delivered baby in the past year -
#   [Anchor - 1 year, Anchor]
#   [Pregnancy, Anchor]
# other insurance and gap-related variables are generated as well, for example first gap day and most recent elg start date etc.

# load elgibility data, CHANGE the month below
load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/PCHP_elg_new_February.RData")
nrow(pchp_elg)
# 541159,540125,539163,537685,535830,533161,530539,527568,522673,519452,516947,513903,510185,

names(pchp_elg)
names(pchp_elg)[1] <- 'MEMBER_ID'
table(pchp_elg$Group)  # no 2500 chip-perinate
#  23000  24000 
# 470738  39443
pchp_elg <- pchp_elg[, c(1,7,16,17)] %>% distinct()
range(pchp_elg$FROM_DT_I2)  # "2009-01-01" "2021-03-11", the second date is moved forward by a month
range(pchp_elg$THRU_DT_I2)  # "2018-02-01" "2078-12-31", the first date is moved forward by a month

# weiwei's old codes were corrected by xiao into a function
# time_interval = {"year", "Pregnancy"}
Insurance_Variable_Generation <- function(pchp_delivery_P, pchp_elg, time_interval){
  
  # to filter women in pregnant cohort defined in step 1&2
  pchp_elg <- pchp_elg[pchp_elg$MEMBER_ID %in% pchp_delivery_P$MEMBER_ID,]  # 561193->13624, 541159->10271
  # from_dt <= thru_dt should be natural, already checked!
  pchp_elg1 <- plyr::join(pchp_elg[, c('MEMBER_ID','FROM_DT_I2','THRU_DT_I2')], pchp_delivery_P, type = 'left', by = 'MEMBER_ID') 
  
  if(time_interval == "year"){
    #pchp_elg1$back12 <- pchp_elg1$Anchor - months(12) + days(1)
    pchp_elg1$back12 <- pchp_elg1$Anchor - days(365)
  }
  if(time_interval == "Pregnancy"){
    pchp_elg1$back12 <- pchp_elg1$Pregnancy
  }
  pchp_elg1 <- pchp_elg1[!duplicated(pchp_elg1), ]
  
  start_dt <- pchp_elg1$back12
  end_dt <- pchp_elg1$Anchor
  
  pchp_elg <- pchp_elg1
  # to make sure the intervals [start_dt, end_dt] = [Anchor-1year, Anchor] and [from_dt, thru_dt] are not intersecting NULL
  # i.e. there exists some insurance coverage during our study window = [Anchor-1year, Anchor]
  elg_gap <- pchp_elg[(pchp_elg$THRU_DT_I2 >= start_dt) & (pchp_elg$FROM_DT_I2 <= end_dt), ]   # 14012 rows to 10901 rows
  elg_gap <- elg_gap[, c("MEMBER_ID","FROM_DT_I2","THRU_DT_I2","back12","Anchor")]
  elg_gap1 <- elg_gap[, c("MEMBER_ID","FROM_DT_I2","THRU_DT_I2")]
  
  # generate a row where from_dt = thru_dt = back12 for each ID 
  temp <- elg_gap[, c('MEMBER_ID','back12','Anchor')]
  names(temp)[2] <- 'FROM_DT_I2'
  names(temp)[3] <- 'THRU_DT_I2'
  temp$THRU_DT_I2 <- temp$FROM_DT_I2
  temp <- temp[!duplicated(temp), ]  
  elg_gap1 <- rbind(elg_gap1, temp)
  
  # generate a row where from_dt = thru_dt = Anchor for each ID
  temp <- elg_gap[,c('MEMBER_ID','back12','Anchor')]
  names(temp)[2] <- 'FROM_DT_I2'
  names(temp)[3] <- 'THRU_DT_I2'
  temp$FROM_DT_I2 <- temp$THRU_DT_I2
  temp <- temp[!duplicated(temp), ]
  elg_gap1 <- rbind(elg_gap1, temp)
  
  temp <- elg_gap[,c('MEMBER_ID','back12','Anchor')]
  elg_gap1 <- plyr::join(elg_gap1, temp, type = 'left', by = 'MEMBER_ID')
  elg_gap1 <- elg_gap1[!duplicated(elg_gap1), ]  # necessary, join above generates duplicates, now 29345 rows
  
  # update from_dt to back12, if it is < back12
  # update thru_dt to Anchor, if it is > Anchor
  elg_gap1$FROM_DT_I2 <- pmax(elg_gap1$FROM_DT_I2, elg_gap1$back12)
  elg_gap1$THRU_DT_I2 <- pmin(elg_gap1$THRU_DT_I2, elg_gap1$Anchor)
  
  # sorted by each ID, the two new generated rows are at the beginning and the end of each ID
  elg_gap1 <- elg_gap1[order(elg_gap1$MEMBER_ID, elg_gap1$FROM_DT_I2, elg_gap1$THRU_DT_I2), ]
  elg_gap1 <- elg_gap1[!duplicated(elg_gap1), ]
  
  gap_vec <- as.numeric(c(0, (elg_gap1$FROM_DT_I2[-1]) - elg_gap1$THRU_DT_I2[1:(length(elg_gap1$THRU_DT_I2)-1)])) - 1
  gap_vec[gap_vec<=0] <- 0
  elg_gap1$gap_days <- gap_vec
  
  # first gap days, the number of days in the first gap in time window [Anchor - 1 year, Anchor]
  temp1 <- elg_gap1 %>% 
    group_by(variable = factor(MEMBER_ID)) %>% 
    filter(any(gap_days==0)) %>% 
    filter(gap_days != 0) %>% 
    arrange(FROM_DT_I2, THRU_DT_I2) %>% slice(1) %>% 
    ungroup() %>% tidyr::complete(variable)
  temp1$first_gap_days <- temp1$gap_days
  
  # last gap days, the number of days in the most recent gap
  temp2 <- elg_gap1 %>% group_by(variable = factor(MEMBER_ID)) %>% 
    filter(any(gap_days==0)) %>% filter(gap_days != 0) %>% 
    arrange(FROM_DT_I2, THRU_DT_I2) %>% slice(n()) %>% ungroup() %>% tidyr::complete(variable)
  temp2$last_gap_days <- temp2$gap_days
  
  # start date and end date of the first insurance within study window [Anchor - 1 year, Anchor]
  temp3 <- elg_gap1 %>% group_by(variable = factor(MEMBER_ID)) %>% 
    filter(any(FROM_DT_I2 == THRU_DT_I2)) %>% filter(FROM_DT_I2 != THRU_DT_I2) %>% 
    arrange(FROM_DT_I2, THRU_DT_I2) %>% slice(1) %>% ungroup() %>% tidyr::complete(variable)
  temp3$first_elg_start <- temp3$FROM_DT_I2
  temp3$first_elg_end <- temp3$THRU_DT_I2   # weiwei doesn't generate this variable
  
  # start date and end date of the most recent insurance
  temp4 <- elg_gap1 %>% group_by(variable = factor(MEMBER_ID)) %>% 
    filter(any(FROM_DT_I2 == THRU_DT_I2)) %>% filter(FROM_DT_I2 != THRU_DT_I2) %>% 
    arrange(FROM_DT_I2, THRU_DT_I2) %>% slice(n()) %>% ungroup() %>% tidyr::complete(variable)
  temp4$recent_elg_start <- temp4$FROM_DT_I2
  temp4$recent_elg_end <- temp4$THRU_DT_I2 
  
  gap_table <- sqldf('select MEMBER_ID, sum(gap_days>0) as n_gaps, sum(gap_days) as gap_days from elg_gap1 group by MEMBER_ID')
  # in order for consistency, we rename n_gaps as n_gaps_correct, gap_days as gap_days_correct although we don't make any corrections here
  names(gap_table)[names(gap_table) == 'n_gaps'] <- 'n_gaps_correct'  
  names(gap_table)[names(gap_table) == 'gap_days'] <- 'gap_days_correct' 
  
  gap_table <- plyr::join(gap_table, temp1[,c('MEMBER_ID','first_gap_days')], type = 'left', by = 'MEMBER_ID') 
  gap_table <- plyr::join(gap_table, temp2[,c('MEMBER_ID','last_gap_days')], type = 'left', by = 'MEMBER_ID') 
  gap_table <- plyr::join(gap_table, temp3[,c('MEMBER_ID','first_elg_start','first_elg_end')], type = 'left', by = 'MEMBER_ID') 
  gap_table <- plyr::join(gap_table, temp4[,c('MEMBER_ID','recent_elg_start','recent_elg_end')], type = 'left', by = 'MEMBER_ID') 
  rm(temp1, temp2, temp3, temp4)
  
  Final <- plyr::join(pchp_delivery_P, gap_table[, c('MEMBER_ID','first_elg_start','recent_elg_start','recent_elg_end','last_gap_days','gap_days_correct','n_gaps_correct')], 
                      type = 'left', by = 'MEMBER_ID') 
  
  return(Final)
}

# CHANGE P index below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Cohort_P42_Updated_AddDate.RData")
Insurance_1 <- Insurance_Variable_Generation(Cohort_P42_Add, pchp_elg, "year")
Insurance_2 <- Insurance_Variable_Generation(Cohort_P42_Add, pchp_elg, "Pregnancy")
names(Insurance_2)[names(Insurance_2) %in% c('first_elg_start')] <- "first_elg_start_now"
names(Insurance_2)[names(Insurance_2) %in% c('recent_elg_start')] <- "recent_elg_start_now"
names(Insurance_2)[names(Insurance_2) %in% c('recent_elg_end')] <- "recent_elg_end_now"
Insurance_P42 <- plyr::join(Cohort_P42_Add, Insurance_1[, c('MEMBER_ID','first_elg_start','recent_elg_start','recent_elg_end','last_gap_days','gap_days_correct','n_gaps_correct')], type = 'left', by = 'MEMBER_ID')
Insurance_P42 <- plyr::join(Insurance_P42, Insurance_2[, c('MEMBER_ID','first_elg_start_now','recent_elg_start_now','recent_elg_end_now')], type = 'left', by = 'MEMBER_ID')
round(mean(Insurance_P42$n_gaps_correct, na.rm = T), 2)  # 1.04,1.03,1.02,1.01,1,0.99,0.97,0.94,0.93,
round(mean(Insurance_P42$gap_days_correct, na.rm = T), 2)  # 223,218,214,212,207,205,195,186,196,
save(Insurance_P42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Insurance_Cohort_P42_Updated_AddDate.RData")
# CHANGE P index below
rm(Insurance_P42,Cohort_P42_Add,Insurance_1,Insurance_2,Insurance_Variable_Generation,pchp_elg)


#### PCHP Preterm Project Step 9 ####
# Generate housing instability variables 

# preparation: manually update accumulating membership data files - need to automate this step
# purpose: we need the past 12 months membership data to search for different addresses
# copy PCHP_mem_new_MONTH.RData file in the folder T:\PCHPAsthma\Data\PCHP raw data (or T:\PCHPAsthma\Data\PCHP raw data\Raw_Data_MONTH_YEAR)
# paste into the folder T:\PCHPAsthma\Data\MemberOnlyData, rename it by adding "_YEAR" in the end

# CHANGE month and year below
month <- 02
year <- 2021
current_month <- month.name[month]  
current_month_abb <-month.abb[month] 
previous_month <- month(as.Date(paste(year, month, "01", sep = "-"))-months(1))
year_of_previous_month <- year(as.Date(paste(year, month, "01", sep = "-"))-months(1))
# # load accumulating data from last month - 2 files
# accu_mem_file <- ifelse(previous_month < 10, paste("/home/michelle/T-Drive/PCHPAsthma/Data/MemberOnlyData/PCHP_mem_After2017_", year_of_previous_month, "0", previous_month, ".RData", sep = ""), 
#                         paste("/home/michelle/T-Drive/PCHPAsthma/Data/MemberOnlyData/PCHP_mem_After2017_", year_of_previous_month, previous_month, ".RData", sep = ""))
# accu_mem_file
# load(accu_mem_file)
load("/home/michelle/preterm_monthly_report/PCHP_mem_After2017_202101.RData")

## load new mem data
#load(paste("/home/michelle/T-Drive/PCHPAsthma/Data/MemberOnlyData/PCHP_mem_new_", current_month, "_", year, ".RData", sep = ""))
load("/home/michelle/preterm_monthly_report/PCHP_mem_new_February.RData")
pchp_mem <- pchp_mem[, !(names(pchp_mem) %in% c('CaseManagementStatus','MAP','key','MI','NameSuffix','x1','middlename','Name_Suffix','fullname','Address2','Address3',
                                                'CaseManagement Status','MemberLinkID','x2','DateofBirth'))]
pchp_mem <- pchp_mem[!is.na(pchp_mem$MemberID), ]
pchp_mem <- pchp_mem[!duplicated(pchp_mem), ]
pchp_mem <- pchp_mem[!duplicated(pchp_mem$MemberID), ]
# add a column, indicating the creation/current month, now 12 columns
pchp_mem$Date <- as.Date(paste(year, month, "01", sep = "-"))

file_name1 <- ifelse(month < 10, paste("pchp_mem_012017to0", month, "_", year, sep = ""), paste("pchp_mem_012017to", month, "_", year, sep = ""))
file_name2 <- ifelse(previous_month < 10, paste("pchp_mem_012017to0", previous_month, "_", year_of_previous_month, sep = ""), paste("pchp_mem_012017to", previous_month, "_", year_of_previous_month, sep = ""))
assign(file_name1, rbind(eval(as.symbol(file_name2)), pchp_mem))

pchp_mem <- pchp_mem[, !names(pchp_mem) %in% c('Phone','Ethnicity')]
file_name3 <- ifelse(month < 10, paste("pchp_mem_After2017to", year, "0", month, sep = ""), paste("pchp_mem_After2017to", year, month, sep = ""))
file_name4 <- ifelse(previous_month < 10, paste("pchp_mem_After2017to", year_of_previous_month, "0", previous_month, sep = ""), paste("pchp_mem_After2017to", year_of_previous_month, previous_month, sep = ""))
assign(file_name3, rbind(eval(as.symbol(file_name4)), pchp_mem))

# remove two data frames loaded from previous month
# CHANGE months below
rm(pchp_mem_012017to01_2021, pchp_mem_After2017to202101)

# pchp_file_to_save <- ifelse(month < 10, paste("/home/michelle/T-Drive/PCHPAsthma/Data/MemberOnlyData/PCHP_mem_After2017_", year, "0", month, ".RData", sep = ""), 
#                             paste("/home/michelle/T-Drive/PCHPAsthma/Data/MemberOnlyData/PCHP_mem_After2017_", year, month, ".RData", sep = ""))
# pchp_file_to_save
## CHANGE months below
save(pchp_mem_012017to02_2021, pchp_mem_After2017to202102, file = "/home/michelle/preterm_monthly_report/PCHP_mem_After2017_202102.RData",
     ascii = FALSE, compress = TRUE, compression_level = 9)  # file = pchp_file_to_save)  # take LONG
rm(accu_mem_file,pchp_mem,file_name1,file_name2,file_name3,file_name4,current_month,current_month_abb,month,year,year_of_previous_month,previous_month,pchp_file_to_save)
# now finished updating accumulating mem data, go back to step 9


# CHANGE months below
rm(pchp_mem_012017to02_2021)
mem_updated <- pchp_mem_After2017to202102
names(mem_updated)[1] <- 'MEMBER_ID'   
names(mem_updated)[4] <- 'Address'     
names(mem_updated)[9] <- 'DOB_mem'

Application_Claim_HouseInstable <- function(pchp_delivery_P, mem_updated){
  
  # to filter women in pregnant cohort defined in step 1&2
  mem_sub <- mem_updated[mem_updated$MEMBER_ID %in% pchp_delivery_P$MEMBER_ID, ]
  # to filter momDOB < 2014-10-1, now >= 4 years old is fine, could adjust this date in the future
  #mem_sub <- mem_sub[mem_sub$DOB < as.Date("2015-10-01")-years(1), ]
  mem_sub <- mem_sub[mem_sub$DOB < as.Date("2015-10-01")-as.difftime(as.integer(365.25*1), units = "days"), ]
  
  t_claim <- plyr::join(mem_sub, pchp_delivery_P, type = 'left', by = 'MEMBER_ID') 
  t_claim <- t_claim[!duplicated(t_claim), ]
  # check memDOB == claimDOB
  t_claim <- t_claim[t_claim$DOB_mem == t_claim$DOB, ]    # from 111991 rows to 111911 rows
  # look back for the past 12 months for different addresses
  t_claim$Anchor_12back <- t_claim$Anchor - days(365)
  # Anchor is always the last day of the prediction month
  # change the date into the first day of that month
  t_claim$Anchor_12back_correct <- t_claim$Anchor_12back - as.POSIXlt(t_claim$Anchor_12back)$mday + 1
  # to filter claims in the past year
  t_claim <- t_claim[(t_claim$Date <= t_claim$Anchor) & (t_claim$Date >= t_claim$Anchor_12back_correct), ]
  t_claim <- t_claim[!duplicated(t_claim), ]  # redundant!
  
  # extract the first 8 characters in the Address
  t_claim$sub_addr1 <- substr(t_claim$Address, 1, 8)
  # number of different Addresses, number of different Addresses in the first 8 char, number of different zipcodes for each ID
  t_claim_house <- sqldf("select count(distinct(sub_addr1)) as num_house, count(distinct(Address)) as num_house_1, count(distinct(ZipCode)) as num_zip, MEMBER_ID 
                         from t_claim group by MEMBER_ID")
  
  Final1 <- plyr::join(pchp_delivery_P, t_claim_house, type = 'left', by = 'MEMBER_ID') 
  
  return(Final1)
}

# CHANGE P index below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Cohort_P42_Updated_AddDate.RData")
HouseInstable_P42 <- Application_Claim_HouseInstable(Cohort_P42_Add[,c('MEMBER_ID','DOB','Anchor','Pregnancy','pregnant_now','pregnant_now1','Pregnant')], mem_updated)
mean(HouseInstable_P42$num_house, na.rm = T)  # 1.29,1.29,1.29,1.30,1.28,1.29,1.30,1.30,1.30,
save(HouseInstable_P42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_HouseStable_Cohort_P42_Updated_AddDate.RData")
# CHANGE P index and month below
rm(HouseInstable_P42,Cohort_P42_Add,pchp_mem_After2017to202102,mem_updated,Application_Claim_HouseInstable)


#### PCHP Preterm Project Step 10 ####
# Load previous data and combine variables generated from steps 1-9  
# Generate last few new variables: social economic status, age, current address, zip and assign providers (OBGYN) 

# CHANGE P index
period <- "P42"

load(paste("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Pregnant_Cohort_", period, "_Updated_AddDate.RData", sep = ""))
#load(paste("D:/PCHP PreTerm/monthly_report/PCHP_Pregnant_Cohort_", period, "_Updated_AddDate.RData", sep = ""))
# check no duplicates in memberID
length(eval(as.symbol(paste("Cohort_", period, "_Add", sep = "")))$MEMBER_ID)
length(unique(eval(as.symbol(paste("Cohort_", period, "_Add", sep = "")))$MEMBER_ID))   # expect equal

# generate social-economic status (zip code-level)
Income <- read.table("/home/michelle/T-Drive/PCHP PreTerm/Data/Household Income and Population2010.txt", header = TRUE)
#Income <- read.table("D:/PCHP PreTerm/monthly_report/Household Income and Population2010.txt", header = TRUE)
names(Income)[1] <- 'ZipCode'
Application_Social_Economic_Status <- function(Income, cohort_P){
  
  Final<-cohort_P
  Final<-plyr::join(Final, Income, type = 'left', by = 'ZipCode')
  Final$Mean<-as.numeric(as.character(Final$Mean))
  Final$Median<-as.numeric(as.character(Final$Median))
  Final$Mean_Ratio<-Final$Mean/38000
  Final$Median_Ratio<-Final$Median/38000
  # use Ratio > 1 or <= 1 to define whether income is low
  Final$low_mean<-as.numeric(Final$Mean<0)  # initialize to be 0
  Final$low_mean[which(Final$Mean<38000)]<-1
  Final$low_median<-as.numeric(Final$Median<0)
  Final$low_median[which(Final$Median<38000)]<-1
  
  return(Final)
}

# CHANGE P index
# REPLACE Final_[current index] to Final_[current index+1], 57->53 occurences
Final_42 <- Application_Social_Economic_Status(Income, Cohort_P42_Add)  
dim(Final_42)  # 38 features
# 6910,6977,7138,7275,7366,7468,7342,7276,7219,7339,6823,6585,7260,
rm(Cohort_P42_Add,Application_Social_Economic_Status,Income)

load(paste("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Current_Cohort_", period, "_Updated.RData", sep = ""))
#load(paste("D:/PCHP PreTerm/monthly_report/PCHP_Current_Cohort_", period, "_Updated.RData", sep = ""))

current_var_list <- c('MEMBER_ID','num_inpt','num_ED','num_outpt','num_pre','num_out_pre','num_ED_inpt','UTI_now','Cyst_now','Uret_now',
                      'asympt_now','Bact_now','abortion_now','ges_diabetes_now','pi_hypertension_now','cervix_now','Cerc_now','Pre_pd',
                      'Inpt_pd','ED_pd','Out_pd','recent_Previsit','first_Previsit','recent_outpt','first_outpt','recent_ED','first_ED',
                      'recent_inpt','first_inpt','Recent_pre_PROV_ID','Recent_out_PROV_ID','Prehist2','exclude') 
# CHANGE P index
Final_42 <- plyr::join(Final_42, Cohort_Current_P42[, current_var_list], type = 'left', by = 'MEMBER_ID')  # 70 features
rm(Cohort_Current_P42, current_var_list)

load(paste("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_History_Cohort_", period, "_Updated_AddDate.RData", sep = ""))   
#load(paste("D:/PCHP PreTerm/monthly_report/PCHP_History_Cohort_", period, "_Updated_AddDate.RData", sep = ""))   

history_var_list <- c('MEMBER_ID','ho_diabetes','ho_hypertension','mood','anxiety','depression','psy','alcohol','substance','smoking',
                      'uterine','sex_trans','HIV','ges_diabetes','pi_hypertension','cervix','Cerc','Abor_his','Prehist1','Prehist11')
# CHANGE P index
Final_42 <- plyr::join(Final_42, Cohort_History_P42[, history_var_list], type = 'left', by = 'MEMBER_ID')  # 89 features
rm(Cohort_History_P42, history_var_list)

load(paste("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_PretermHistory_Cohort_", period, "_Updated_AddDate.RData", sep = "")) 
#load(paste("D:/PCHP PreTerm/monthly_report/PCHP_PretermHistory_Cohort_", period, "_Updated_AddDate.RData", sep = "")) 

# CHANGE P index
Final_42 <- plyr::join(Final_42, PretermHistory_P42[, c('MEMBER_ID','Prehist')], type = 'left', by = 'MEMBER_ID')  # 90 features
rm(PretermHistory_P42)

load(paste("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_DrugCurrent_Cohort_", period, "_Updated_AddDate.RData", sep = ""))
#load(paste("D:/PCHP PreTerm/monthly_report/PCHP_DrugCurrent_Cohort_", period, "_Updated_AddDate.RData", sep = ""))

medication_var_list <- c('MEMBER_ID','Totalgross_now','num_OH_17_now','max_DOS_17OH','min_DOS_17OH')
# CHANGE P index
Final_42 <- plyr::join(Final_42, DrugP42[, medication_var_list], type = 'left', by = 'MEMBER_ID')  # 94 features
rm(DrugP42, medication_var_list)

load(paste("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Nutrition_Cohort_", period, "_Updated_AddDate.RData", sep = ""))
#load(paste("D:/PCHP PreTerm/monthly_report/PCHP_Nutrition_Cohort_", period, "_Updated_AddDate.RData", sep = ""))

nutrition_var_list <- c('MEMBER_ID','malnutrition','obe_over')
# CHANGE P index
Final_42 <- plyr::join(Final_42, Nutrition_Cohort_P42[, nutrition_var_list], type = 'left', by = 'MEMBER_ID')  # 96 features
rm(Nutrition_Cohort_P42, nutrition_var_list)

load(paste("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_HouseStable_Cohort_", period, "_Updated_AddDate.RData", sep = ""))
#load(paste("D:/PCHP PreTerm/monthly_report/PCHP_HouseStable_Cohort_", period, "_Updated_AddDate.RData", sep = ""))

house_var_list <- c('MEMBER_ID','num_house','num_house_1','num_zip')
# CHANGE P index
Final_42 <- plyr::join(Final_42, HouseInstable_P42[, house_var_list], type = 'left', by = 'MEMBER_ID')  # 99 features
rm(HouseInstable_P42, house_var_list)

load(paste("/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_Insurance_Cohort_", period, "_Updated_AddDate.RData", sep = ""))
#load(paste("D:/PCHP PreTerm/monthly_report/PCHP_Insurance_Cohort_", period, "_Updated_AddDate.RData", sep = ""))

insurance_var_list <- c('MEMBER_ID','first_elg_start','recent_elg_start','recent_elg_end','first_elg_start_now',
                        'recent_elg_start_now','recent_elg_end_now','last_gap_days','gap_days_correct','n_gaps_correct')
# CHANGE P index
Final_42 <- plyr::join(Final_42, Insurance_P42[, insurance_var_list], type = 'left', by = 'MEMBER_ID')  # 108 features
rm(Insurance_P42, insurance_var_list)


# generate Age and Physician-Patient Matching (OBGYN) features

Final_42$Age<- floor(as.numeric(as.Date(Final_42$Anchor)- as.Date(Final_42$DOB))/365.25)   
#hist(Final_42$Age)
median(Final_42$Age, na.rm = TRUE)  # 26

# CHANGE month and P index below
load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/PCHP_elg_new_February.RData")
#load("D:/PCHP PreTerm/monthly_report/PCHP raw data/PCHP_elg_new_MONTH.RData")  
nrow(pchp_elg)
# 541159,540125,539163,537685,536830,533161,530539,527568,522673,519452,516947,513903,510185,
names(pchp_elg)[1] <- 'MEMBER_ID'

# CHANGE month below, not needed
#pchp_elg <- subset(pchp_elg, (pchp_elg$THRU_DT_I2 > as.Date(end_dt)-years(1)) & (pchp_elg$FROM_DT_I2 <= as.Date(end_dt)))
pchp_elg <- subset(pchp_elg, (pchp_elg$THRU_DT_I2 > as.Date(end_dt)-as.difftime(as.integer(365.25*1), units = "days")) & (pchp_elg$FROM_DT_I2 <= as.Date(end_dt)))
pchp_elg[, 'PCPID'] <- str_replace_all(pchp_elg[, 'PCPID'], ' ', "")
pchp_elg <- pchp_elg[order(pchp_elg$THRU_DT_I2, decreasing = TRUE), ]   # order to get the most recent PCP ID
Final_42 <- plyr::join(Final_42, pchp_elg[, c("MEMBER_ID","PCPID")], by = "MEMBER_ID", type = "left", match = "first")  
names(Final_42)[dim(Final_42)[2]] <- 'Recent_PCPID' 
rm(pchp_elg) 

save(Final_42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/Step_10_Before_PCPID_Updated_AddDate.RData")

load("T:/PCHP PreTerm/Data/Step_10_Before_PCPID_Updated_AddDate.RData")

# in 2/28/2021, using read.csv or read.table to import txt file started to fail in PCCI DEV
# data was broken after loading (not able to read in or only partially loaded), so running it locally from here
# different errors including -
# invalid input found on input connection 
# EOF within quoted string
# number of items read is not a multiple of the number of columns
# incomplete final line found by readTableHeader

# NEW FUNCTION to map from provider ID to provider Name by xiao
pchp_prov_ID_name <- function(month_year){
  
  # load provider data of current month
  #chip_prov <- paste("/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/CHIP_AIR_PROV_", month_year, ".txt", sep = '')
  #paid_prov <- paste("/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/PAID_AIR_PROV_", month_year, ".txt", sep = '')
  chip_prov <- paste("T:/PCHPAsthma/Data/ftp aetna/CHIP_AIR_PROV_", month_year, ".txt", sep = '')
  paid_prov <- paste("T:/PCHPAsthma/Data/ftp aetna/PAID_AIR_PROV_", month_year, ".txt", sep = '')
  
  pchp_prov_chip <- read.csv(chip_prov, header = T, quote = "\"", sep = '|', stringsAsFactors = FALSE)
  pchp_prov_paid <- read.csv(paid_prov, header = T, quote = "\"", sep = '|', stringsAsFactors = FALSE)
  
  # All the new chip IDs are numeric. Old alpha numerics fall out.
  pchp_prov_chip <- pchp_prov_chip[(!is.na(pchp_prov_chip$ProviderID))&!duplicated(pchp_prov_chip$ProviderID), ]
  names(pchp_prov_chip) <- gsub("\\.", '', names(pchp_prov_chip))
  pchp_prov_paid <- pchp_prov_paid[(!is.na(pchp_prov_paid$ProviderID))&!duplicated(pchp_prov_paid$ProviderID), ]
  names(pchp_prov_paid) <- gsub("\\.", '', names(pchp_prov_paid))
  pchp_prov <- rbind(pchp_prov_paid, pchp_prov_chip)      
  rm(pchp_prov_paid, pchp_prov_chip, chip_prov, paid_prov)
  
  # generate an Attend_Name column from three name-related columns in provider data as key for join
  pchp_prov[, 'ProviderID'] <- str_trim(pchp_prov[, 'ProviderID'], side = "both")
  pchp_prov <- pchp_prov[, c('ProviderID','FirstName','MI','LastName')]
  pchp_prov <- unique(pchp_prov)  
  pchp_prov$FirstName <- gsub(" ", "", pchp_prov$FirstName, fixed = TRUE)
  pchp_prov$LastName <- gsub(" ", "", pchp_prov$LastName, fixed = TRUE)
  Name <- paste(pchp_prov$FirstName, pchp_prov$MI, pchp_prov$LastName, sep = ',')  
  Name <- gsub(", ,", ",", Name, fixed = TRUE)
  Name <- gsub("^,", "", Name)
  pchp_prov$Attend_Name <- Name
  pchp_prov$key_1 <- pchp_prov$ProviderID
  
  return(pchp_prov)
}

# STOP: missing a PCP in prov table since Jan 2019!
# manually match PCPID == PROV0000P03522 to Name == "BRUCE,RAJALA"
# CHANGE month below
pchp_prov_ID_name("20210314") %>% filter(ProviderID == "PROV0000P03522")  # no output means this provider is still missing
# extract Dr. Rajala information from previous PROV table (last time appearance)
rajala <- pchp_prov_ID_name("20181114") %>% filter(ProviderID == "PROV0000P03522")  # don't change date
rajala
#     ProviderID    FirstName MI LastName    Attend_Name             key_1
# PROV0000P03522        BRUCE      RAJALA   BRUCE,RAJALA    PROV0000P03522

# CHANGE month below
pchp_prov <- pchp_prov_ID_name("20210314")  
nrow(pchp_prov)
# 42774,42893,42990,43086,43337,43538,43639,43934,44064,44137,44261,44440,44805,
pchp_prov <- rbind(pchp_prov, rajala)   # added one, Rajala
rm(rajala)


# NEW FUNCTION by Xiao to assign most recent prenatal & outpt attenting provider Name,
# then define most Recent_AttendID & Name
assign_recent_attend <- function(Final_P, pchp_prov){
  
  # join with Final_INDEX to match the most recent prenatal/outpt provider ID with Name
  Final_P[,'Recent_pre_PROV_ID'] <- str_trim(Final_P[, 'Recent_pre_PROV_ID'], side = "both")
  Final_P$key_1 <- Final_P$Recent_pre_PROV_ID
  Final_P <- plyr::join(Final_P, pchp_prov[, c('key_1','Attend_Name')], type = 'left', by = 'key_1') 
  names(Final_P)[dim(Final_P)[2]] <- 'Recent_pre_Attend_Name'
  Final_P[,'Recent_out_PROV_ID'] <- str_trim(Final_P[, 'Recent_out_PROV_ID'], side = "both")
  Final_P$key_1 <- Final_P$Recent_out_PROV_ID    # key_1 is changed from pre ID to outpt ID
  Final_P <- plyr::join(Final_P, pchp_prov[, c('key_1', 'Attend_Name')], type = 'left', by = 'key_1') 
  names(Final_P)[dim(Final_P)[2]] <- 'Recent_out_Attend_Name'
  Final_P <- Final_P[, !names(Final_P)%in%c('key_1')]  
  
  # logic: if most recent prenatal provider name is avaiable, recent_attendID = recent_prenatalID
  # otherwise, recent_attendID = recent_outptID
  Final_P$Recent_AttendID <- Final_P$Recent_pre_PROV_ID
  Final_P$Recent_AttendName <- Final_P$Recent_pre_Attend_Name
  
  #table(!is.na(Final_P$Recent_AttendName), !is.na(Final_P$Recent_AttendID))
  
  Final_P$Recent_AttendID[is.na(Final_P$Recent_AttendName)] <- Final_P$Recent_out_PROV_ID[is.na(Final_P$Recent_AttendName)]
  Final_P$Recent_AttendName[is.na(Final_P$Recent_AttendName)] <- Final_P$Recent_out_Attend_Name[is.na(Final_P$Recent_AttendName)]
  
  return(Final_P) 
}

# CHANGE P index
Final_42 <- assign_recent_attend(Final_42, pchp_prov)  # 114 features
dim(Final_42)
rm(period,pchp_prov,pchp_prov_ID_name,assign_recent_attend)  
# CHANGE P index
#save(Final_42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/Step_10_Before_PCPID_Updated_New_AddDate.RData")
save(Final_42, file = "T:/PCHP PreTerm/Data/Step_10_Before_PCPID_Updated_New_AddDate.RData")


# remove outliers

# CHANGE P index
Final_42 <- Final_42[!((is.na(Final_42$recent_elg_start))&(!is.na(Final_42$num_outpt))),] 

remove_outlier1 <- function(Final_P){
  
  # eremove outliers s.t last delivery date and current pregnancy date are too close, at least 3 months
  Final_P_1<-Final_P[is.na(Final_P$Delivery_past_1),]   
  Final_P_2<-Final_P[!is.na(Final_P$Delivery_past_1),]   
  Final_P_2<-Final_P_2[as.numeric(Final_P_2$Anchor-Final_P_2$Delivery_past_1)>=91,]  
  Final_P<-rbind(Final_P_1,Final_P_2)  
  Final_P_1<-Final_P[!is.na(Final_P$RiskGroupID),]  
  Final_P_2<-Final_P[is.na(Final_P$RiskGroupID),]  
  Final_P_2<-Final_P_2[Final_P_2$first_HEDISDate+days(9*30)>Final_P_2$Anchor,] 
  Final_P<-rbind(Final_P_1,Final_P_2)    
  rm(Final_P_1,Final_P_2)   
  return(Final_P)
}

# CHANGE P index
Final_42 <- remove_outlier1(Final_42)  
nrow(Final_42)
# 6193,6279,6466,6496,6738,6884,6772,6681,6630,6722,6189,5963,6730,
Final_42$exclude[is.na(Final_42$exclude)] <- 0
Final_42 <- Final_42[Final_42$exclude==0,]  
nrow(Final_42)
# 5857,5931,6110,6085,6324,6451,6329,6239,6179,6259,5731,5478,6214,
sum(is.na(Final_42$MEMBER_ID))  # expect 0
rm(remove_outlier1)


#### assign PCP Name & Affil ID & Affil Name, OBGYN ID & Name, OBGYN Affil ID & Affil Name and large provider ####

assign_providers <- function(patient_l, step){
  
  # check for most recent provider table in Asthma data folder
  #provider_path <- "/home/michelle/T-Drive/PCHPAsthma/Data/Provider_Table/Query_05.29.2019/Query_05.29.2019.txt"  # updated in Aug 21, 2019
  #provider_path <- "/home/michelle/T-Drive/PCHPAsthma/Data/Provider_Table/Query_Matt Bartoszek - PCHP_01-21-2020/Query_Matt Bartoszek - PCHP_01-21-2020.txt"  # updated in Feb 26, 2020
  #provider_path <- "/home/michelle/T-Drive/PCHPAsthma/Data/Provider_Table/PCP_PROVIDERFILE_JULY2020_PCHP.txt"  # updated in Sep 16, 2020, 591090, 8 columns, ~200 less providers
  provider_path <- "/home/michelle/T-Drive/PCHPAsthma/Data/Provider_Table/pcp_provider_1120_pchp.txt"  # updated in Nov 20, 2020, 801723 no header, manually assign
  #provider_path <- "T:/PCHPAsthma/Data/Provider_Table/pcp_provider_1120_pchp.txt"
  
  # in 2/28/2021, could not read in elg table correctly
  # # check for delimiter and headers
  #elg_new <- read.table(provider_path, header = F, sep = "\t", quote = "\"")  # 801723, 8 columns, much more providers than last time
  #elg_new <- read.csv(provider_path, header = F, sep = "\t", stringsAsFactors = FALSE)
  library(readr)
  elg_new <- read_delim(provider_path, delim = "\t", col_names = F)
  # warning with reading ID columns, default reading in as double type, but there are IDs starting with NB, so should specify as string
  names(elg_new) <- c("Plan_Code","Pcp_Id","Pcp_Fullname","Pcp_Affil_Id","Pcp_Affil_Fullname","carriermemid","Pcp_Eff_Dt","Pcp_Term_Dt")
  #elg_new <- read.csv(provider_path, header = TRUE, sep = "|", stringsAsFactors = FALSE)  
  
  # sorting by effective date is very important, since we need to find the MOST RECENT prenatal/outpatient visit
  # Q: when defining most recent PCPID from elg, sort by thru_dt?
  elg_new <- elg_new[order(elg_new[, 'Pcp_Eff_Dt'], decreasing = TRUE), ] 
  names(elg_new)[2] <- 'PCPID'
  elg_new[, 'PCPID'] <- str_trim(elg_new[, 'PCPID'], side = "both")
  names(elg_new)[1] <- 'Plan_Code'  # almost all "TXP"
  
  patient_l[, 'Recent_PCPID'] <- str_trim(patient_l[, 'Recent_PCPID'], side = "both")
  
  
  # assign PCP Name, PCP Affil ID & Affil Name 1/2/3
  
  # Step 1: match on most recent PCPID (Recent_PCPID) from elg and PCPID in provider table 
  # for Recent_PCPID, find PCP_Fullname, most recent (effective date) PCP_Affil_ID and PCP_Affil_Fullname
  elg_new$key1<-elg_new$PCPID
  patient_l$key1<-patient_l$Recent_PCPID 
  patient_l_1 = plyr::join(patient_l,elg_new[,c("key1",'Pcp_Affil_Id','Pcp_Fullname','Pcp_Affil_Fullname')],by="key1",type="left",match="first")
  names(patient_l_1)[(dim(patient_l_1)[2]-2):dim(patient_l_1)[2]]<-c('Pcp_Affil_Id_1','Pcp_Fullname_1','Pcp_Affil_Fullname_1')
  
  # Step 2: match on most recent attending provider ID (Recent_AttendID) from claims and PCPID in provider table
  # recall that we assign Recent_AttendID/Name as the Recent_pre_PROV_ID/Name, and if its prenatal info is NA, use outpatient instead
  # for Recent_AttendID, find PCP_Fullname, most recent PCP_Affil_ID and PCP_Affil_Fullname
  elg_new$key2<-elg_new$PCPID
  patient_l_1$key2<-patient_l$Recent_AttendID
  patient_l_1 = plyr::join(patient_l_1,elg_new[,c("key2",'Pcp_Affil_Id','Pcp_Fullname','Pcp_Affil_Fullname')],by="key2",type="left",match="first")
  names(patient_l_1)[(dim(patient_l_1)[2]-2):dim(patient_l_1)[2]]<-c('Pcp_Affil_Id_2','Pcp_Fullname_2','Pcp_Affil_Fullname_2')
  
  # Step3: match on most recent attending provider ID (Recent_AttendID) from claims and PCP affiliation ID (Pcp_Affil_Id) in provider table
  # for Recent_AttentID, find PCP_Fullname, most recent PCP_Affil_ID and PCP_Affil_Fullname
  elg_new$key3<-elg_new$Pcp_Affil_Id     # DIFFERENT! not PCPID!
  patient_l_1$key3<-patient_l$Recent_AttendID
  patient_l_1 = plyr::join(patient_l_1,elg_new[,c("key3",'Pcp_Affil_Id','Pcp_Fullname','Pcp_Affil_Fullname')],by="key3",type="left",match="first")
  names(patient_l_1)[(dim(patient_l_1)[2]-2):dim(patient_l_1)[2]]<-c('Pcp_Affil_Id_3','Pcp_Fullname_3','Pcp_Affil_Fullname_3')
  
  patient_l_1<-patient_l_1[,!names(patient_l_1)%in%c('key3','key2','key1')]
  
  
  # assign OBGYN Provider ID(OBGYN_ID), OBGYN Provider name(OBGYN_Fullname), 
  #        OBGYN Provider group ID(OBGYN_Affil_ID) and OBGYN Provider group name(OBGYN_Affil_Fullname)
  
  # initial assignment of OBGYN - by step 2 matching
  patient_l_1$OBGYN_ID<-patient_l_1$Recent_AttendID
  patient_l_1$OBGYN_Fullname<-patient_l_1$Recent_AttendName
  patient_l_1$OBGYN_Affil_ID<-patient_l_1$Pcp_Affil_Id_2
  patient_l_1$OBGYN_Affil_Fullname<-patient_l_1$Pcp_Affil_Fullname_2
  
  # update OBGYN affil ID & Name, if it is NA - by step 3 matching
  patient_l_1$OBGYN_Affil_ID[is.na(patient_l_1$OBGYN_Affil_ID)]<- patient_l_1$Pcp_Affil_Id_3[is.na(patient_l_1$OBGYN_Affil_ID)]
  patient_l_1$OBGYN_Affil_Fullname[is.na(patient_l_1$OBGYN_Affil_Fullname)]<- patient_l_1$Pcp_Affil_Fullname_3[is.na(patient_l_1$OBGYN_Affil_Fullname)]
  
  ## [TO ADD] IN NEW MODEL 
  ## if current OBGYN ID is NA, i.e. no previous prenatal or outpatient visit (Recent_AttendID = NA)
  ## use their PCPIDs in elg to be OBGYN IDs - by step 1 matching
  ## but in original srcipt, we only label OBGYN ID is NA or not, not actually assign PCPID to the OBGYN ID
  ## should ADD following assignment to apply step 1 matching - haven't added yet!!!
  # patient_l_1$OBGYN_ID[is.na(patient_l_1$OBGYN_ID)] <- patient_l_1$Recent_PCPID[is.na(patient_l_1$OBGYN_ID)]
  # patient_l_1$OBGYN_Fullname[is.na(patient_l_1$OBGYN_ID)] <- patient_l_1$Pcp_Fullname_1[is.na(patient_l_1$OBGYN_ID)]
  # patient_l_1$OBGYN_Affil_ID[is.na(patient_l_1$OBGYN_ID)] <- patient_l_1$Pcp_Affil_Id_1[is.na(patient_l_1$OBGYN_ID)]
  # patient_l_1$OBGYN_Affil_Fullname[is.na(patient_l_1$OBGYN_ID)] <- patient_l_1$Pcp_Affil_Fullname_1[is.na(patient_l_1$OBGYN_ID)]
  
  # after applying step 1, if OBGYN is still missing, assign "NA"
  patient_l_1$OBGYN_ID_Missing<-'No'
  patient_l_1$OBGYN_ID_Missing[which(is.na(patient_l_1$OBGYN_ID))]<-'Yes'
  ID<-which(is.na(patient_l_1$OBGYN_ID))
  patient_l_1$OBGYN_ID[ID]<-'NA'
  patient_l_1$OBGYN_Fullname[ID]<-'NA'
  patient_l_1$OBGYN_Affil_ID <- as.character(patient_l_1$OBGYN_Affil_ID)  # added in 3/1/2021, couldn't assign new value to factor
  patient_l_1$OBGYN_Affil_Fullname <- as.character(patient_l_1$OBGYN_Affil_Fullname)  # added in 3/1/2021
  patient_l_1$OBGYN_Affil_ID[ID]<-'NA' 
  patient_l_1$OBGYN_Affil_Fullname[ID]<-'NA'  
  
  ## for those with OBGYN_ID and corresponding full name, but no OBGYN_Affil_ID and OBGYN_Affil_Fullname, use their assigned provider in elg
  #ID1<-which(is.na(patient_l_1$OBGYN_Affil_ID))
  #patient_l_1$OBGYN_Affil_ID[ID1]<-patient_l_1$Pcp_Affil_Id_1[ID1]
  #patient_l_1$OBGYN_Affil_Fullname[ID1]<-patient_l_1$Pcp_Affil_Fullname_1[ID1]
  
  # for those with OBGYN_ID and corresponding full name, but no OBGYN_Affil_ID and OBGYN_Affil_Fullname, use their OBGYN_Fullname as OBGYN_Affil_Fullname
  # but there exists situation that patients have OBGYN ID but no OBGYN Name, what to do?
  patient_l_1$OBGYN_Affil_Fullname[(patient_l_1$OBGYN_ID_Missing=='No')&(is.na(patient_l_1$OBGYN_Affil_Fullname))] <- 
    patient_l_1$OBGYN_Fullname[(patient_l_1$OBGYN_ID_Missing=='No')&(is.na(patient_l_1$OBGYN_Affil_Fullname))]
  
  patient_l <- patient_l_1
  patient_l[,'OBGYN_Affil_Fullname'] <- str_trim(patient_l[,'OBGYN_Affil_Fullname'], side = "both")
  rm(patient_l_1)
  
  # assign large provider
  # Q: where are these logics from? how do we know whether they are updated and who should we ask for updated list?
  
  # initial assignment of large provider is just OBGYN affiliation name
  large_provider <- patient_l$OBGYN_Affil_Fullname   
  #length(unique(large_provider))  # 456 unique names
  
  #### COPC provider and large provider assignment & update ####
  #-------------------------------------------------------------------------------#
  # COPC Provider Site Update, fix master list by looking at updated copc list
  # update OBGYN_Affil_Fullname
  # [Q] should this list only be used for asthma? all pediatrics? no OBGYN at all?
  #-------------------------------------------------------------------------------#
  # don't see a lot of updates actually happening here - 
  # 8 patients with the PCPID in copc_list and changed OBGYN_Affil_Fullname in 7 of them in 2020 March
  
  #copc_list <- read.csv("/home/michelle/T-Drive/PCHPAsthma/Data/Analysis/Initial Asthma Analysis/PCHP Working/provider assigning 1.14.2015/COPC_UPDATE.csv", stringsAsFactors=FALSE)  # Feb 2015
  #copc_list <- read.csv("/home/michelle/T-Drive/PCHPAsthma/Data/Analysis/Initial Asthma Analysis/PCHP Working/provider assigning 1.14.2015/COPC_UPDATE_0223.csv", stringsAsFactors=FALSE)  # 93, Feb 2018
  #green <- copc_list[1:54,]
  #red <- copc_list[56:93,]
  copc_list <- read.csv("/home/michelle/T-Drive/PCHPAsthma/Data/Analysis/Initial Asthma Analysis/PCHP Working/provider assigning 1.14.2015/COPC_UPDATE_20200120.csv", stringsAsFactors=FALSE)  # 96, added 4 PCP from Youth & Family Centers, Feb 2020
  #copc_list <- read.csv("T:/PCHPAsthma/Data/Analysis/Initial Asthma Analysis/PCHP Working/provider assigning 1.14.2015/COPC_UPDATE_20200120.csv", stringsAsFactors=FALSE)
  green <- copc_list[c(1:54,93:96),]
  red <- copc_list[55:92,]
  
  #dallas.county <- which(patient_l$Pcp_Affil_Fullname=="DALLAS COUNTY HOSPITAL DISTRICT")
  
  # green assignment
  copcs <- levels(factor(green$Provider.site))  # 14
  
  deharo <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[1]])
  east_dallas <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[2]])
  epo <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[3]])
  floater <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[4]])
  garland <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[5]])
  grand.prairie <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[6]])
  homes <-  which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[7]])
  irving <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[8]])
  oak.west <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[9]])
  ppcc <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[10]])
  southeast.dallas <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[11]] | patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[12]])
  vickery <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[13]])
  youth.fam.center <- which(patient_l$OBGYN_ID %in% green$PCPID[green$Provider.site==copcs[14]])
  
  patient_l[deharo, 'OBGYN_Affil_Fullname'] <- "DEHARO SALDIVAR HEALTH CENTER"
  patient_l[east_dallas, 'OBGYN_Affil_Fullname'] <- "EAST DALLAS HEALTH CENTER"
  patient_l[epo, 'OBGYN_Affil_Fullname'] <- "EMPLOYEE PHYSICIAN OFFICE (EPO)"
  patient_l[floater, 'OBGYN_Affil_Fullname'] <- "FLOATER"
  patient_l[garland, 'OBGYN_Affil_Fullname'] <- "GARLAND HEALTH CENTER"
  patient_l[grand.prairie, 'OBGYN_Affil_Fullname'] <- "GRAND PRAIRIE HEALTH CENTER"
  patient_l[homes, 'OBGYN_Affil_Fullname'] <- "HOMES"
  patient_l[irving, 'OBGYN_Affil_Fullname'] <- "IRVING HEALTH CENTER"
  patient_l[oak.west, 'OBGYN_Affil_Fullname'] <- "OAK WEST HEALTH CENTER"
  patient_l[ppcc, 'OBGYN_Affil_Fullname'] <- "PPCC HEALTH CENTER"
  patient_l[southeast.dallas, 'OBGYN_Affil_Fullname'] <- "SOUTHEAST DALLAS HEALTH CENTER"
  patient_l[vickery, 'OBGYN_Affil_Fullname'] <- "VICKERY HEALTH CENTER"
  patient_l[youth.fam.center, 'OBGYN_Affil_Fullname'] <- "YOUTH & FAMILY CENTERS"
  
  # red assignment
  copcs2 <- levels(factor(red$Provider.site))  # 4
  
  fam_med <- which(patient_l$OBGYN_ID %in% red$PCPID[red$Provider.site==copcs2[1]])
  int_med <- which(patient_l$OBGYN_ID %in% red$PCPID[red$Provider.site==copcs2[2]])
  no_peds <- which(patient_l$OBGYN_ID %in% red$PCPID[red$Provider.site==copcs2[3]])
  wom_hea <- which(patient_l$OBGYN_ID %in% red$PCPID[red$Provider.site==copcs2[4]])
  
  patient_l[fam_med, 'OBGYN_Affil_Fullname'] <- "FAMILY MEDICINE"
  patient_l[int_med, 'OBGYN_Affil_Fullname'] <- "INTERNAL MEDICINE"
  patient_l[no_peds, 'OBGYN_Affil_Fullname'] <- "NO PEDIATRICS"
  patient_l[wom_hea, 'OBGYN_Affil_Fullname'] <- "WOMEN'S HEALTH"
  
  #-----------------------------------------------------------#
  # DALLAS COUNTY HOSPITAL DISTRICT and COPC 
  #-----------------------------------------------------------#
  
  index <- grep("HATCHER STATION WOMEN'S HEALTH CENTER", patient_l$OBGYN_Affil_Fullname)
  patient_l$OBGYN_Affil_Fullname[index] <- "HATCHER STATION WOMENS HEALTH CENTER"
  
  # PHHS WHC clinics + other
  copc<-paste(c('DALLAS COUNTY HOSPITAL DISTRICT','DALLAS COUNTY HOSPITAL', 
                'DALLAS COUNTY HOSPITAL DISTRICT (CLINIC)',  # xiao added in Apr 20, 2020
                'Parkland Health & Hospital System','PARKLAND MEMORIAL HOSPITAL','PARKLANDMEMORIALHOSPITAL',
                'SOUTHEAST PARKLAND WOMENS','SOUTHEAST DALLAS HEALTH CENTER','SOUTHEAST DALLAS HEALTH',
                'SOUTHEASTPARKLANDWOMENS',  # xiao added in Apr 20, 2020
                'GARLAND PARKLAND WOMENS HEALTHCARE','GARLAND WOMENS HEALTH CENTER','GARLAND HEALTH CENTER',
                'GARLAND WHC',  # xiao added in Apr 20, 2020
                'HATCHER STATION HEALTH CENTER','HATCHER STATION WOMENS HEALTH CENTER',
                "HATCHER - EAST DALLAS WHC",  # xiao added in Apr 20, 2020
                'OAK WEST PARKLAND WOMENS HEAL','OAK WEST PARKLAND WOMENS HEALTH','OAKWEST WOMENS HEALTH CENTER','OAK WEST HEALTH CENTER',
                "OAK WEST WHC",  # xiao added in Apr 20, 2020
                'DEHARO SALDIVAR-PARKLAND WOMEN','DEHARO SALDIVAR HEALTH CENTER',
                'GRAND PRAIRIE WOMENS HEALTH CENTER','GRAND PRAIRIE HEALTH CENTER',
                'GRAND PRAIRIE WHC',  # xiao added in Apr 20, 2020
                # PHHS clinic was renamed to E. Carlyle Smith, Jr. Health Center - Women's Health Center?
                'VICKERY WOMENS HEALTH CENTER','VICKERY HEALTH CENTER',
                'EAST DALLAS HEALTH CENTER',
                'BLUITT-FLOWERS',
                'FLOATER',
                'HOMES',
                'PPCC HEALTH CENTER',
                'EMPLOYEE PHYSICIAN OFFICE (EPO)',
                'IRVING HEALTH CENTER',
                'YOUTH & FAMILY CENTERS',
                "LAKE WEST WHC","LAKEWEST WOMENS HEALTH CENTER","MAPLEWHC","MAPLEWOMENSHEALTHCENTER"  # xiao added in Apr 20, 2020
  ))
  
  copc_ind <- which(patient_l$OBGYN_Affil_Fullname %in% copc)  # 932 
  large_provider[copc_ind] <- "ParklandOB"
  #table(large_provider[copc_ind])
  #table(patient_l$OBGYN_Affil_Fullname[copc_ind])
  
  # Q: this section is partially duplicated?
  large_provider[which(large_provider%in%c("DALLAS COUNTY HOSPITAL DISTRICT"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("DALLASCOUNTYHOSPITALDISTRICT"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("DALLAS COUNTY HOSPITAL DISTRICT DBA MAPLE WOMENS HEALTH CEN"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("DALLAS COUNTY HOSPITAL DISTRICT DBA IRVING WOMENS HEALTH CEN"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("DALLAS COUNTY HOSPITAL DISTRICT DBA PARKLAND AIDS CLINIC"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("DALLAS COUNTY HOSPITAL DISTRICT DBA WYNNEWOOD ACUTE RESPONSE"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("DALLASCOUNTYHEALTHDEPT"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("SOUTHEAST PARKLAND WOMENS"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("GARLAND PARKLAND WOMENS HEALTHCARE"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("HATCHER STATION HEALTH CENTER"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("HATCHER STATION WOMENS HEALTH CENTER"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("OAK WEST PARKLAND WOMENS HEAL"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("DEHARO SALDIVAR-PARKLAND WOMEN"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("GRAND PRAIRIE WOMENS HEALTH CENTER"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("IRVING HEALTH CENTER"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("BRIAN,M,CASEY"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("JODI,S,DASHE"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("MORRIS,R,BRYANT"))]<-"ParklandOB"
  large_provider[which(large_provider%in%c("WISH CAMPUS"))]<-"ParklandOB"
  
  
  # THR - Texas Health Presbyterian Hospital Dallas
  thr_index <- grep("MEDICAL EDGE|TEXAS HEALTH (PRESBYTERIAN|HARRIS METHODIST|PRESBY|SURGERY CENTER DENTON|HUGULEY|WNJ|ARLINGTON|WOMENS HEALTH|
                    PEDIATRIC|PHYSICIAN GROUP|CARE PLLC|HEART AND VASCULAR HOPS|PHYSICIANS GROUP)", patient_l$OBGYN_Affil_Fullname)
  large_provider[thr_index] <- "THR"
  
  # patient place
  pat.place <- grep("PATIENT PLACE", patient_l$OBGYN_Affil_Fullname)
  large_provider[pat.place] <- "PATIENT PLACE"
  
  # UTSW
  # Q: UT SOUTHWESTERN / MSP?
  utsw <- grep("YEO, NANCY|UT SOUTHWESTERN MEDICAL CENTER|UT (SOUTHWESTERN|SW)", patient_l$OBGYN_Affil_Fullname)
  large_provider[utsw] <- "UTSW"
  # Q: how do I know this list needs to be updated?
  # Q: at least Arias-Franklin is not in the list?
  added_UTSW<-paste(c('EVELINA,V,ALCALEN','FRANCIS,CUNNINGHAM','SHENA,J,DILLON','ELAINE,L,DURYEA','JOSIAH,Z,HAWKINS','BARBARA,L,HOFFMAN',
                      'STEVEN,A,HOFFMAN','DEANA,HUSSAMY','ALVIN,T,HYSLOP','FARNAZ,JAHANGIRI','DEBRA,K,KNIPE','XERCERLA,A,LITTLES','JULIE,Y,LO',
                      'ANN,LUTICH','JAMIE,L,MORGAN','ELYSIA,MOSCHOS','DAVID,B,NELSON','DAVIDMOWENSMD','SHIVANI,R,PATEL','SHELLEY,B,RAMOS',
                      'SCOTT,W,ROBERTS','CONNIE,S,ROBERTS','VANESSA,ROGERS','DAVID,E,ROGERS','PATRICIA,SANTIAGO-MUNOZ','JENNIFER,L,STAUD',
                      'ROBERT,D,STEWART','SUSAN,STORRY','TAM,C,TRUONG','PATRICK,M,WEIX','CHET,WELLS','CLAUDIA,L,WERNER','TIFFANY,N,WOODUS',
                      'SOUTHWESTERNWILLIAMPCLEMENTSJR'))
  added_UTSW_ind <- which(patient_l$OBGYN_Affil_Fullname %in% added_UTSW)  # 115
  #patient_l$OBGYN_Affil_Fullname[added_UTSW_ind]<-"UT Southwestern Medical Center"
  large_provider[added_UTSW_ind] <- "UTSW"
  
  # BROCK,L,PIERCE
  PIERCE <- grep("BROCK,L,PIERCE|^LAWRENCESCOTTPIERCEMD|LAWRENCESCOTTPIERCEMD|BROCKLPIERCEMD|^BROCKLPIERCEMD", patient_l$OBGYN_Affil_Fullname)
  large_provider[PIERCE] <- "Pierce Group"
  
  # MI Doctor
  mi_doctor <- grep("HIGHWAY 30|PLEASANT GROVE|DENTON FAMILY HEALTH|FAMILY MEDICAL ASSOC OF NORTH DALLAS|TOPCARE MEDICAL GROUP INC|MI DOCTOR|
                    ^PEDIATRICS OF DALLAS", patient_l$OBGYN_Affil_Fullname)
  large_provider[mi_doctor] <- "MI DOCTOR"
  
  # # hoffman - Steven Hoffman, M.D. (merged to MI Doctor in August 2018)
  # hoffman <- grep("HIGHWAY 30|PLEASANT GROVE|DENTON FAMILY HEALTH|FAMILY MEDICAL ASSOC OF NORTH DALLAS", patient_l$OBGYN_Affil_Fullname)
  # large_provider[hoffman] <- "DR HOFFMAN"
  
  # pedcare
  ped_care <- grep("PEDCARE PA|PEDIATRIC CLINIC OF MESQUITE",patient_l$OBGYN_Affil_Fullname)
  large_provider[ped_care] <- "PEDCARE"
  
  # PCA
  pca <- grep("PCA PRIMARY CARE ASSOCIATES|PCA GREENVILLE|PCA - GREENVILLE|PRIMARY CARE ASSOCIATES", patient_l$OBGYN_Affil_Fullname)
  large_provider[pca] <- "PCA"
  #levels(factor(patient_l$OBGYN_Affil_Fullname[pca]))
  
  # Childrens
  childrens <- grep("CHARLES TURNER LEWIS III|KIDS HEALTHCARE", patient_l$OBGYN_Affil_Fullname)
  large_provider[childrens] <- "KID'S HEALTHCARE (Charles Lewis)"
  
  # My childrens 
  mychild <- grep("PHYSICIANS FOR CHILDREN|CHILDRENS HEALTH PEDIATRIC GRP|CHILDRENS HEALTH GROUP|CHILDRENS HEALTH PEDIATRIC GROUP", patient_l$OBGYN_Affil_Fullname)
  large_provider[mychild] <- "CHILDRENS HEALTH PEDIATRIC GRP"
  
  # Presbyterian Hospital
  large_provider[which(large_provider=="TEXASHEALTHPRESBYTERIANHOSPITAL")]<-"PRESBYTERIAN HOSPITAL"
  large_provider[which(large_provider=="THE WOMENS HEALTH CENTER OF DALLAS PRESBYTERIAN")]<-"PRESBYTERIAN HOSPITAL"
  
  # MacArthur Medical Center
  # Q: how to check this list is updated? - on their website?
  # Richard Wagner, Peter Sakovich, Swati Joshipura, Jeffrey Wang, Jeff Livingston(already included), Colette Dominique, Elia Fanous(already included),
  # Rebecca Gray(already included), Reshma Patel(already included), Brian Enggano(already included), Andrea Arguello(already included), 
  # James Carleo(already included), Teri Forney(already included), Stacey A Thomas, Kim Sakovich(already included),
  # Summer Migoni(one of large provider to generat report),
  # Erica Hutchins, Mary Lela Vonder Haar, Melissa Dishman, Clare Ruvalcaba, Mary Ann Faucher
  # Q: Stephen Sakovich is MACARTHUR MEDICAL CENTER PLLC, the same?
  MacArthur<-paste(c('ELIA,FANOUS','TERI,D,FORNEY','NEHAL,MASALAWALA','ADRIANA,Y,LOPEZ','ANDREA,ARGUELLO','BRIAN,M,ENGGANO','JAMES,P,CARLEO',
                     'JEFF M LIVINGSTON MD','KEVIN,ONEIL','KIMBERLY,SAKOVICH','MODENA,C,RUVALCABA','REBECCA,GRAY','RESHMA,N,PATEL','STEPHEN,P,SAKOVICH',
                     'WIYATTA,B,FREEMAN'))
  MacArthur_ind <- which(patient_l$OBGYN_Affil_Fullname %in% MacArthur)  # 252
  #patient_l$OBGYN_Affil_Fullname[MacArthur_ind]<-"MacArthur Medical Center"
  large_provider[MacArthur_ind] <- "MacArthur Medical Center"
  
  # Advanced OB/Gyn Associates
  # Q: Dr Holcomb now is not in this clinic? checked website, current providers are -
  # Richard Salter, Anu Gupta, Arden Moulin, Shefali Pappu
  large_provider[which(large_provider=="DENISSE,HOLCOMB")]<-"Advanced OBGyn Associates"
  
  # Advanced Women's Healthcare
  # Q: the four providers are still there, added Maria Reyes, she has a lot of patients in patient list?
  Adv<-paste(c('HEIDI,HARMS','MICHELLE,L,HEINTGES','AMIE,NAPIER','AMY,G,SIGMAN'))  # 43
  Adv_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Adv)
  #patient_l$OBGYN_Affil_Fullname[Adv_ind ]<-"Advanced Women's Healthcare"
  large_provider[Adv_ind] <- "Advanced Womens Healthcare"
  
  # Carlos & Parnell, MD, PA
  # checked all 5 providers stay there
  CP<-paste(c('WINFRED,PARNELL','JOSEPH,J,CARLOS','MONICA,M,DIAZ','RACHEL,C,GUNDERSON','WENDY,C,PARNELL'))
  CP_ind <- which(patient_l$OBGYN_Affil_Fullname%in%CP)
  #patient_l$OBGYN_Affil_Fullname[CP_ind]<-"Carlos & Parnell, MD, PA"
  large_provider[CP_ind] <- "Carlos AND Parnell,MD,PA"
  
  # Bernard Adami, MD P.A.
  # xiao added "BERNARD F ADAMI MD" in Apr 15, 2020
  large_provider[which(large_provider %in% c("BERNARDFADAMIMD","BERNARD F ADAMI MD"))]<-"Bernard Adami,MD P.A."  # 129
  
  # Comprehensive OB/GYN
  # Q: Maria is not there (went to advanced womens health), added Yara Ramirez, the other three are still there?
  Com<-paste(c('RENEE,L,CHAN','GERALD,D,LUCIANI','CESAR,A,REYES','MARIA,T,REYES','SEAN,A,SADLER'))
  Com_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Com)  # 71
  #patient_l$OBGYN_Affil_Fullname[Com_ind]<-"Comprehensive OB/GYN"
  large_provider[Com_ind] <- "Comprehensive OBGYN"
  
  # Contemporary Women's Care
  # Q: added Mary Ann Franken? these two stay there
  Con<-paste(c('SHAHEEN,F,JACOB','NEDRA,L,RICE'))
  Con_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Con)
  #patient_l$OBGYN_Affil_Fullname[Con_ind]<-"Contemporary Women??s Care"
  large_provider[Con_ind] <- "Contemporary Womens Care"
  
  # Craig Ranch OB/Gyn
  # Q: added Christine Baidwan etc, and only add MD or also add other titles(midwife etc)?
  Craig<-paste(c('HEATHER,S,DERRICK','ROBIN,EVENSON','SHEA,JOYNER','BARBARA,LARSEN','SARA,ROBERT','ANDREW,T,SHIMER'))
  Craig_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Craig)
  #patient_l$OBGYN_Affil_Fullname[Craig_ind]<-"Craig Ranch OB/Gyn"
  large_provider[Craig_ind] <- "Craig Ranch OBGyn"
  
  # Dr. Deleon Women's Healthcare Clinic
  # Q: added Luis Usuga, Zandomeni and Jose are gone? Jose had a new website
  # xiao added "DR JOSE FRANCISCO DE LEON MD" in Apr 15, 2020
  Deleon<-paste(c('ISAMARIE,L,ALCANTARA','GABRIELA,M,ZANDOMENI','JOSE F DE LEON MD PA',"DR JOSE FRANCISCO DE LEON MD")) 
  Deleon_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Deleon)  # 104
  #patient_l$OBGYN_Affil_Fullname[Deleon_ind]<-"Dr. Deleon Women's Healthcare Clinic"
  large_provider[Deleon_ind] <- "Dr Deleon Womens Healthcare Clinic"
  large_provider[which(large_provider%in%c("DR DELEONS WOMANS HEALTH CARE"))]<-"Dr Deleon Womens Healthcare Clinic"
  
  # F. Thomas Dean, MD
  # xiao added "FRANKTHOMASDEANMD" in Apr 15, 2020
  large_provider[which(large_provider %in% c("FTHOMASDEANMD","FRANKTHOMASDEANMD"))]<-"F.Thomas Dean,MD"  # 59
  
  # Fetal Care Consultants
  # Q: Rubeo is not there? 8 providers from website
  large_provider[which(large_provider=="ZACHARY,S,RUBEO")]<-"Fetal Care Consultants"
  
  # Flower Mound Women's Care
  # Q: these two are there, new providers - Van Dell, Arax Nazarian?
  Flo<-paste(c('ELIZABETH,L,DICKENS','SALY,THOMAS'))
  Flo_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Flo)
  #patient_l$OBGYN_Affil_Fullname[Flo_ind]<-"Flower Mound Women's Care"
  large_provider[Flo_ind] <- "Flower Mound Womens Care"
  
  # Grace Women's Health Center
  # checked, correct
  large_provider[which(large_provider=="ABRAHAM, MINIJA A")]<-"Grace Womens Health Center"  # 33
  
  # Grand Prairie Women's Health Center
  patient_l$OBGYN_Affil_Fullname[patient_l$OBGYN_Affil_Fullname=='CATHERINE,E,MCCAFFITY']<-"Grand Prairie Women's Health Center"
  large_provider[which(large_provider=="CATHERINE,E,MCCAFFITY")]<-"ParklandOB"
  
  # Health Central, P.A.
  # the list below all stays there
  # Q: Dallas new provider - Francesca Perugigi, Liesl B Smith, Stephanie Slocum, Tracy Speight, Colleen Metzler, Melissa Wright?
  # Q: Frisco new provider - P Clay Alexander, Phyllis J Gee, Rena Griffin?
  Hea<-paste(c('ARMINDA,CALLEJAS-WEINTZ','TRACY,H,ELLIOTT','ALAN,GREENBERG','CLARK,W,GRIFFITH','ELISE,HARPER','NATALIE,C,LIGHT','A,J,STAUB',
               'FELICIA,M,TILLMAN','ANN,R,WOODBRIDGE'))
  Hea_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Hea)  # 67
  #patient_l$OBGYN_Affil_Fullname[Hea_ind]<-"Health Central, P.A."
  large_provider[Hea_ind] <- "Health Central, PA"
  
  # Lake Pointe Medical Partners
  # Q: relation to Rubeo above?
  large_provider[which(large_provider=="MONIKA,HEARNE")]<-"Fetal Care Consultants"
  
  # Lake Pointe Women's Centre
  # Q: Sudela is gone, new provider - Lily F Primo, Ashley Dressen, Cheryl A Millender?
  # Q: Center or Centre?
  Lake<-paste(c('JEFFERY,NELSON','THOMAS,S,SUDELA'))
  Lake_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Lake)
  #patient_l$OBGYN_Affil_Fullname[Lake_ind]<-"Lake Pointe Women's Centre"
  large_provider[Lake_ind] <- "Lake Pointe Womens Centre"
  
  # Las Colinas OB/GYN
  # Q: Garcia is still there, new provider - John J Zavaleta, Annie Saldana, Cynthia Mace-Motta, Stephanie Heliot?
  large_provider[which(large_provider=="GONZALO,H,GARCIA")]<-"Las Colinas OBGYN"
  
  # Matlock Obstetrics & Gynecology Associates, PA
  # Q: Froeschke is gone, new provider - Amra Uzoma, Omar R Sarmini, Noelle R Niemand, Stacy Oommen?
  Matlock<-paste(c('JANIS,R,CORNWELL','TAMRA,FORTENBERRY','HARRY,P,FROESCHKE','ROBERT,A,GREVE'))
  Matlock_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Matlock)
  #patient_l$OBGYN_Affil_Fullname[Matlock_ind]<-"Matlock Obstetrics & Gynecology Associates, PA"
  large_provider[Matlock_ind] <- "Matlock Obstetrics and Gynecology Associates, PA"
  
  # Methodist ObGyn
  # Q: GUNBY is not with Methodist, went to SWISS OBGYN
  # Narris has a new address, not on the website
  # Arzac stays there
  # complete OBGYN on website is so long
  Methodist<-paste(c('JORGE ARTURO ARZAC M.D','ROBERT,T,GUNBY','PATRICIA,N,HARRIS'))
  Methodist_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Methodist)  # 45
  #patient_l$OBGYN_Affil_Fullname[Methodist_ind]<-"Methodist ObGyn"
  large_provider[Methodist_ind] <- "Methodist ObGyn"
  
  # OB Hospitalists of Texas
  # Q: couldn't find this large provider website? 
  # Bakos, Browning is now with Baylor Scott & White Women's Health Group
  # Bakos is a separate large provider count - affiliation name change
  # so xiao added "DR.SHARONGRIMESBAKOSM.D" in Apr 16, 2020
  # Browning is at 3600 Gaston Ave Ste 1158, Dallas, TX 75246
  # Payne is now with SWISS obygn
  # Havemann is at 3600 Gaston Ave Ste 651, Dallas, TX 75246
  OB<-paste(c('SHARON,BAKOS','DR.SHARONGRIMESBAKOSM.D','ADRIANNE,C,BROWNING','PAUL,B,PAYNE','LESLIEHAVEMANNMD'))
  OB_ind <- which(patient_l$OBGYN_Affil_Fullname%in%OB)
  #patient_l$OBGYN_Affil_Fullname[OB_ind]<-"OB Hospitalists of Texas"
  large_provider[OB_ind] <- "OB Hospitalists of Texas"
  
  #OBHG Texas Holdings, PC
  OBHG<-paste(c('THERESA,CONYAC','CHARLES,R,DOWNEY','MARIE,E,HOLLIS'))
  OBHG_ind <- which(patient_l$OBGYN_Affil_Fullname%in%OBHG)
  #patient_l$OBGYN_Affil_Fullname[OBHG_ind]<-"OBHG Texas Holdings, PC"
  large_provider[OBHG_ind] <- "OBHG Texas Holdings, PC"
  
  #Obstetrics & Gynecology Associates of Dallas
  OBs<-paste(c('LINDEN,COLLINS','GLEN,W,HECKMAN'))
  OBs_ind <- which(patient_l$OBGYN_Affil_Fullname%in%OBs)
  #patient_l$OBGYN_Affil_Fullname[OBs_ind]<-"Obstetrics & Gynecology Associates of Dallas"
  large_provider[OBs_ind] <- "Obstetrics and Gynecology Associates of Dallas"
  
  # Omega OB/GYN Associates
  # Q: added Melodi N Reese-Holley?
  # John Pickel seems to be retired
  Omega<-paste(c('CHARMAINE,OLADELL','STEPHANIE,PICKEL','KEVIN,R,GORDON','MINDY,B,LUCK','JEFFREY,L,MORGAN','JOHN,A,PICKELJR','STEVEN,J,SELIGMAN',
                 'KEITH,R,STORTS'))
  Omega_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Omega)
  #patient_l$OBGYN_Affil_Fullname[Omega_ind]<-"Omega OB/GYN Associates"
  large_provider[Omega_ind] <- "Omega OBGYN Associates"
  
  #Parkland Health & Hospital System
  Park<-paste(c('SARAH,B,JENSEN','LINDSAY,D,MOUSER','SHAYLA,SIMPSON','KAREN,M,ZELMAN'))
  Park_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Park)
  patient_l$OBGYN_Affil_Fullname[Park_ind]<-"Parkland Health & Hospital System"
  large_provider[Park_ind] <- "ParklandOB"
  
  # Premier OB/GYN Center
  # Q: both stay there, don't know whether there are more now? 
  Premier<-paste(c('ANU,F,OGUNLARI','DR.MICHAELC.YANGM.D.'))
  Premier_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Premier)  # 76
  #patient_l$OBGYN_Affil_Fullname[Premier_ind]<-"Premier OB/GYN Center"
  large_provider[Premier_ind] <- "Premier OBGYN Center"
  
  #Questcare Obstetrics
  Questcare<-paste(c('REBECCA,L,CRAWFORD','CHARLES,E,WHITE'))
  Questcare_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Questcare)
  #patient_l$OBGYN_Affil_Fullname[Questcare_ind]<-"Questcare Obstetrics"
  large_provider[Questcare_ind] <- "Questcare Obstetrics"
  
  #Southeast Dallas Women's Health Center
  patient_l$OBGYN_Affil_Fullname[patient_l$OBGYN_Affil_Fullname%in%c('MELISSA,A,JONES')]<-"Southeast Dallas Women's Health Center"
  large_provider[which(large_provider%in%c("MELISSA,A,JONES"))]<-"ParklandOB"
  
  # Texas Regional Womens Health Center
  # checked, only one provider there
  large_provider[which(large_provider=="SAMI,E,CONSTANTINE")]<-"Texas Regional Womens Health Center"
  
  #Trinity Women's Center
  Trinity<-paste(c('DONALD,BLAIR','JOY,M,SMITH'))
  Trinity_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Trinity)
  #patient_l$OBGYN_Affil_Fullname[Trinity_ind]<-"Trinity Women's Center"
  large_provider[Trinity_ind] <- "Trinity Womens Center"
  
  #Waxahachie Women's Health
  Wax<-paste(c('JASON,P,BROWN','ROBIN,P,KINDRED','DAVID,B,MOREHEAD','KRISTIN,N,WILLIAMS'))
  Wax_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Wax)
  #patient_l$OBGYN_Affil_Fullname[Wax_ind]<-"Waxahachie Women's Health"
  large_provider[Wax_ind] <- "Waxahachie Womens Health"
  
  # White Rock OB/GYN
  # Q: JAVIER is still there, the only provider, Perugnini is now in Health Central Women's Care?
  White<-paste(c('JAVIER,GARCIARUIZDESOMOCURCIO','FRANCESCA,PERUGINI'))
  White_ind <- which(patient_l$OBGYN_Affil_Fullname%in%White)
  #patient_l$OBGYN_Affil_Fullname[White_ind]<-"White Rock OB/GYN"
  large_provider[White_ind] <- "White Rock OBGYN"
  
  # WOMENS SPECIALTY CENTER
  # Q: couldn't find these providers in website, but Ripp practices at 3 locations for women's specialty center? 
  # providers form website - Gonzalo Venegas, Jeffrey Sandate, Alfredo G Antonetti
  WOMENS<-paste(c('ELIZABETH,A,RAMIREZ','ZACHARY,RIPP'))
  WOMENS_ind <- which(patient_l$OBGYN_Affil_Fullname%in%WOMENS)
  #patient_l$OBGYN_Affil_Fullname[WOMENS_ind]<-"WOMENS SPECIALTY CENTER"
  large_provider[WOMENS_ind] <- "WOMENS SPECIALTY CENTER"
  
  #Women's Wellness Center
  large_provider[which(large_provider=="TERESA,D,KOWALCZYK")]<-"Womens Wellness Center"
  
  #HEALTHTEXASPROVIDERNETWORK
  healthtexas <- grep("HEALTH TEXAS PROVIDER NETWORK  DBA PCA OF GUN BARREL CITY|^HEALTHTEXAS PROVIDER NETWORK|HEALTHTEXAS PROVIDER NETWORK|HEALTHTEXASPROVIDERNETWORK|HEALTHTEXASPROVIDERNETWORKDBA", patient_l$OBGYN_Affil_Fullname)
  large_provider[healthtexas] <- "HEALTH TEXAS PROVIDER NETWORK"
  
  #METHODIST
  healthtexas <- grep("METHODISTCHARLTONMEDICALCTR|METHODISTHOSPITALSOFDALLASDBA|METHODISTRICHARDSONMEDICALCENTER", patient_l$OBGYN_Affil_Fullname)
  large_provider[healthtexas] <- "Methodist ObGyn"
  
  #OB Hospitalists of Texas
  large_provider[which(large_provider=="OB HOSPITALISTS OF TEXAS PA")]<-"OB Hospitalists of Texas"
  large_provider[which(large_provider=="OBHG Texas Holdings, PC")]<-"OB Hospitalists of Texas"
  
  #NA
  allmiss<-which(((large_provider=="NA")|(is.na(large_provider)))&(is.na(patient_l$Recent_PCPID)))
  large_provider[allmiss]<-"Missing PCP"
  
  #NA OBGYN
  # Q: there are cases who have OBGYN ID, but no OBGYN name, thus no large provider?
  naobgyn<-which(((large_provider=="NA")|(is.na(large_provider)))&(!is.na(patient_l$Recent_PCPID)))
  large_provider[naobgyn]<-"Missing OBGYN And Have PCP"
  
  
  patient_l <- cbind(patient_l, large_provider)  # append large_provider information to each patient
  
  # xiao combined for step 10 and step 10.5 in May 26, 2020
  if (step == 10){
    # summarize large provider counts
    y <- as.data.frame(table(large_provider))
    y <- y[order(y[,2], decreasing = TRUE),]    # ordered by count
    x <- as.data.frame(table(patient_l$OBGYN_Affil_Fullname))
    x <- x[order(x[,2], decreasing = TRUE),]
    
    unmatch <- which(patient_l$Pcp_Affil_Fullname == "DALLAS COUNTY HOSPITAL DISTRICT" && patient_l$large_provider == "COPC")
    print(nrow(unmatch))  # expect NULL
    
    assign("patient_l", patient_l, envir=.GlobalEnv)
    assign('Large_Provider_Count', y, envir=.GlobalEnv)
    #write.table(y, file = '/home/michelle/T-Drive/PCHP PreTerm/Data/Large_provider_count.txt', sep = '\t', row.names = F)
    write.table(y, file = 'T:/PCHP PreTerm/Data/Large_provider_count.txt', sep = '\t', row.names = F)
    assign('Raw_Provider_Count', x, envir=.GlobalEnv)
    #write.table(x, file = '/home/michelle/T-Drive/PCHP PreTerm/Data/Affliation_provider_count.txt', sep = '\t', row.names = F)
    write.table(x, file = 'T:/PCHP PreTerm/Data/Affliation_provider_count.txt', sep = '\t', row.names = F)
  }else if (step == 10.5){
    return(patient_l)
  }
}

# already fixed "CLINICALPATHOLOGYLABSINC" and "LABORATORYCORPORATIONOFAMER" problem

# CHANGE P index
assign_providers(Final_42, step = 10)
patient_l %>% group_by(large_provider) %>% tally() %>% arrange(desc(n))
Final_42 <- patient_l
dim(Final_42)  # 129 variables
rm(patient_l,Large_Provider_Count,Raw_Provider_Count)
# CHANGE year and month below, month is not abbreviation
#save(Final_42, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/PCHP_March_2021_Raw.RData")
save(Final_42, file = "T:/PCHP PreTerm/Data/PCHP_March_2021_Raw.RData")

# could go back to PCCI DEV and continue running reports


#### PCHP Preterm Project Step 10.5 ####
# Auxiliary code to generate retrospective statistics in the past year
# Generate actual preterm (<35w) rate for all providers in the past year, as well as other numbers
# This rate will be included as a figure into the final monthly report 

#rm(list = ls())
rm(Final_42, period)

# load past 1.5 year claim data
# CHANGE month below
#load("/home/michelle/T-Drive/PCHP PreTerm/Data/Accumulating Claim Data for generating monthly report/PCHP_claims_20190731_02232021.RData")
load("/home/michelle/preterm_monthly_report/PCHP_claims_20190731_02232021.RData")
range(pchp_claims$DOS)  # expect 1.5-year

t_claim <- as.data.frame(pchp_claims)
rm(pchp_claims)

# define different stages/levels of preterm and full-term deliveries
# extremely premature - gestational age <= 28 weeks, i.e. in 24-28 weeks
# xiao added last 15 codes, removed P07.31 in May 26, 2020
ext_premature<-paste(c('P07.21','P07.22','P07.23','P07.24','P07.25','P07.26','765.21','765.22','765.23','765.24',
                       'P05.01','P05.02','P05.03','P05.11','P05.12','P05,13','P07.01','P07.02','P07.03','765.01','765.02','765.03','765.11','765.12','765.13'))

t_claim$ext_premature<-as.numeric(t_claim$DGN1%in%ext_premature | t_claim$DGN2%in%ext_premature |
                                    t_claim$DGN3%in%ext_premature | t_claim$DGN4%in%ext_premature | t_claim$DGN5%in%ext_premature)

# very premature - gestational age in 28 1/7-32 weeks  
# xiao added last 15 codes and P07.31, removed P07.35 in May 26. 2020
very_premature<-paste(c('P07.31','P07.32','P07.33','P07.34','765.25','765.26',
                        'P05.04','P05.05','P05.06','P05.14','P05.15','P05.16','P07.14','P07.15','P05.16','765.04','765.05','765.06','765.14','765.15','765.16'))

t_claim$very_premature<-as.numeric(t_claim$DGN1%in%very_premature | t_claim$DGN2%in%very_premature |
                                     t_claim$DGN3%in%very_premature | t_claim$DGN4%in%very_premature | t_claim$DGN5%in%very_premature)

# moderate premature - gestational age in 32 1/7-34 weeks 
# xiao added last 5 codes and P07.35, removed P07.37 in May 26, 2020
moder_premature<-paste(c('P07.35','P07.36','765.27',
                         'P05.07','P05.17','P07.17','765.07','765.17'))

t_claim$moder_premature<-as.numeric(t_claim$DGN1%in%moder_premature | t_claim$DGN2%in%moder_premature |
                                      t_claim$DGN3%in%moder_premature | t_claim$DGN4%in%moder_premature | t_claim$DGN5%in%moder_premature)

# late premature - gestational age in 34 1/7-36 6/7 weeks 
# xiao added last 10 codes in May 26, 2020
late_premature<-paste(c('P07.38','P07.39','765.28',
                        'P05.09','P05.19','765.09','765.19',
                        'P07.37','P05.08','P05.18','P07.18','765.08','765.18'))  # late < 35w

t_claim$late_premature<-as.numeric(t_claim$DGN1%in%late_premature | t_claim$DGN2%in%late_premature |
                                     t_claim$DGN3%in%late_premature | t_claim$DGN4%in%late_premature | t_claim$DGN5%in%late_premature)

# premature/immature - gestational age < 37 weeks
# paste all codes above, xiao added some .1 & .10 codes in May 26, 2020
prem_imm <- paste(c('P05.01','P05.02','P05.03','P05.04','P05.05','P05.06','P05.07','P05.08','P05.09',
                    'P05.11','P05.12','P05.13','P05.14','P05.15','P05.16','P05.17','P05.18','P05.19',
                    'P07.0','P07.00','P07.01','P07.02','P07.03',
                    'P07.1','P07.10','P07.14','P07.15','P07.16','P07.17','P07.18',
                    'P07.2','P07.20','P07.21','P07.22','P07.23','P07.24','P07.25','P07.26',
                    'P07.3','P07.30','P07.31','P07.32','P07.33','P07.34','P07.35','P07.36','P07.37','P07.38','P07.39',
                    '765.0','765.00','765.01','765.02','765.03','765.04','765.05','765.06','765.07','765.08','765.09',
                    '765.1','765.10','765.11','765.12','765.13','765.14','765.15','765.16','765.17','765.18','765.19',
                    '765.21','765.22','765.23','765.24','765.25','765.26','765.27','765.28'))

t_claim$prem_imm<-as.numeric(t_claim$DGN1%in%prem_imm | t_claim$DGN2%in%prem_imm |
                               t_claim$DGN3%in%prem_imm | t_claim$DGN4%in%prem_imm | t_claim$DGN5%in%prem_imm)

# full-term delivery (updated)     
# xiao added "Z37.0" in Dec 4, 2019
# xiao added last 16 codes in May 25, 2020
full_term <- paste(c('Z37.0','Z38.0','Z38.00','Z38.01','Z38.1','Z38.2','765.29',
                     'P08.2','P08.21','P08.22','766.2','766.21','766.22','V27.0','V30','V30.0','V30.00','V30.01','V30.1','V30.2','O80','O82','650'))

t_claim$full_term<-as.numeric(t_claim$DGN1%in%full_term | t_claim$DGN2%in%full_term |
                                t_claim$DGN3%in%full_term | t_claim$DGN4%in%full_term | t_claim$DGN5%in%full_term)

dim(t_claim[t_claim$prem_imm == 1,])[1]  # 81659,92480,91790,90985,90708,90082,91342,92776,92980,93867,94140,81950,
if ((sum(t_claim[t_claim$prem_imm == 1,]$ext_premature)) == (sum(t_claim$ext_premature)) &&
    (sum(t_claim[t_claim$prem_imm == 1,]$very_premature)) == (sum(t_claim$very_premature)) &&
    (sum(t_claim[t_claim$prem_imm == 1,]$moder_premature)) == (sum(t_claim$moder_premature)) &&
    (sum(t_claim[t_claim$prem_imm == 1,]$late_premature)) == (sum(t_claim$late_premature))){
  cat("Yes, prem_imm variable includes all preterm and immature information!")
} else {
  cat("No, some preterm or immature information is not reflected in prem_imm variable!")
}

# check preterm/immature information doesn't conflict with full-term information 
sum(t_claim[t_claim$full_term == 1,]$prem_imm)
sum(t_claim[t_claim$prem_imm == 1,]$full_term)
print(paste("There are", table(t_claim$prem_imm, t_claim$full_term)[2,2], 
            "records whose full_term and premature_immature variables are simultaneously 1."))  
# 25698,25738,35101,34970,35082,35151,35315,35891,36745,37099,37662,38437,36645,


# process preterm/immature deliveries in the past year

# CHANGE month below
#t_claim_baby_preimm<-t_claim[(t_claim$DOB<=(as.Date("2019-10-31")))&(t_claim$DOB>(as.Date("2019-10-31")-years(1)))&(t_claim$prem_imm==1),]
#t_claim_baby_preimm<-t_claim[(t_claim$DOB<=(as.Date("2020-03-31")))&(t_claim$DOB>(as.Date("2020-03-31")-as.difftime(as.integer(365.25*1), units = "days")))&(t_claim$prem_imm==1),]
t_claim_baby_preimm<-t_claim[(t_claim$DOB<=end_dt)&(t_claim$DOB>=start_dt)&(t_claim$prem_imm==1),]

# load processed Link file in step 1&2 directly
# CHANGE month below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/Mom_Baby_Link/Link_03142021_processed.RData")

t_claim_baby_preimm <- plyr::join(t_claim_baby_preimm, Link[, c('MEMBER_ID', 'MommyLink_ID')], type = 'left', by = 'MEMBER_ID')   
t_claim_baby_preimm <- t_claim_baby_preimm[!duplicated(t_claim_baby_preimm ), ]
t_claim_baby_preimm_nomom <- t_claim_baby_preimm[is.na(t_claim_baby_preimm$MommyLink_ID), ]   
t_claim_baby_preimm <- t_claim_baby_preimm[!is.na(t_claim_baby_preimm$MommyLink_ID), ]  
length(which(is.na(t_claim_baby_preimm$Delivery)))   # expect 0
t_claim_baby_preimm$key_1 <- t_claim_baby_preimm$MommyLink_ID   
t_claim_baby_preimm$Delivery <- t_claim_baby_preimm$DOB
t_claim_mom_preterm <- sqldf("select MEMBER_ID as MEMBER_ID_Baby, min(DOS) as recent_DOS_baby, max(ext_premature) as ext_premature1, 
                              max(very_premature) as very_premature1, max(moder_premature) as moder_premature1, max(late_premature) as late_premature1,
                              max(prem_imm) as prem_imm1, max(full_term) as full_term1, MommyLink_ID, key_1, Delivery from t_claim_baby_preimm group by MEMBER_ID")      
t_claim_mom_preterm$recent_DOS_baby <- as.Date(t_claim_mom_preterm$recent_DOS_baby, origin = "1970-01-01")
t_claim_mom_preterm <- t_claim_mom_preterm[, !names(t_claim_mom_preterm) %in% c("key_1")]
length(which(is.na(t_claim_mom_preterm$MommyLink_ID)))    # expect 0
t_claim_mom_preterm <- t_claim_mom_preterm[!is.na(t_claim_mom_preterm$MEMBER_ID_Baby), ]
t_claim_mom_preterm <- t_claim_mom_preterm[!is.na(t_claim_mom_preterm$Delivery), ]

# IMPORTANT CHANGE!!! 
# as long as there is any preterm code of any stage associated with a delivery, it is not a full-term, even though there might exist a full term code.
# we manually change full_term variable into 0 in such scenarios
table(t_claim_mom_preterm$full_term1)  # 648,663,1138,1248,1126,1123,1126,1131,1132,1097,1094,1124,1086,
t_claim_mom_preterm$full_term1 <- 0

t_claim_delivery_preterm <- t_claim_mom_preterm 
rm(t_claim_mom_preterm)


# process full-term deliveries in the past year

# CHANGE month below
#t_claim_baby_full<-t_claim[(t_claim$DOB<=(as.Date("2019-10-31")))&(t_claim$DOB>(as.Date("2019-10-31")-years(1)))&(t_claim$full_term==1),]
#t_claim_baby_full<-t_claim[(t_claim$DOB<=(as.Date("2020-03-31")))&(t_claim$DOB>(as.Date("2020-03-31")-as.difftime(as.integer(365.25*1), units = "days")))&(t_claim$full_term==1),]
t_claim_baby_full<-t_claim[(t_claim$DOB<=end_dt)&(t_claim$DOB>start_dt)&(t_claim$full_term==1),]

t_claim_baby_full <- plyr::join(t_claim_baby_full, Link[, c('MEMBER_ID', 'MommyLink_ID')], type = 'left', by = 'MEMBER_ID')
t_claim_baby_full <- t_claim_baby_full[!duplicated(t_claim_baby_full), ]
t_claim_baby_full_nomom <- t_claim_baby_full[is.na(t_claim_baby_full$MommyLink_ID), ] 
t_claim_baby_full <- t_claim_baby_full[!is.na(t_claim_baby_full$MommyLink_ID), ]
length(which(is.na(t_claim_baby_full$Delivery)))  # expect 0
t_claim_baby_full$key_1 <- t_claim_baby_full$MommyLink_ID  
t_claim_baby_full$Delivery <- t_claim_baby_full$DOB
t_claim_mom_full <- sqldf("select MEMBER_ID as MEMBER_ID_Baby, min(DOS) as recent_DOS_baby, max(ext_premature) as ext_premature1, max(very_premature) as very_premature1,
                           max(moder_premature) as moder_premature1, max(late_premature) as late_premature1, max(prem_imm) as prem_imm1,max(full_term) as full_term1, 
                           MommyLink_ID, key_1, Delivery from t_claim_baby_full group by MEMBER_ID")  
t_claim_mom_full$recent_DOS_baby <- as.Date(t_claim_mom_full$recent_DOS_baby, origin = "1970-01-01")
t_claim_mom_full <- t_claim_mom_full[, !names(t_claim_mom_full) %in% c("key_1")]
t_claim_mom_full <- t_claim_mom_full[!is.na(t_claim_mom_full$MEMBER_ID_Baby),]
t_claim_mom_full <- t_claim_mom_full[!is.na(t_claim_mom_full$Delivery),]
t_claim_mom_full <- t_claim_mom_full[!is.na(t_claim_mom_full$MommyLink_ID),]

# IMPORTANT CHANGE!!! 
# as long as there is any preterm code of any stage associated with a delivery, it is not a full term, even though there might exist a full term code.
# we manually change full_term variable into 0 in such scenarios
table(t_claim_mom_full$prem_imm1)  # 648,660,1133,1243,1121,1118,1126,1125,1129,1094,1094,1119,1085,
table(t_claim_mom_full$full_term1)
t_claim_mom_full$full_term1[which(t_claim_mom_full$prem_imm1 == 1)] <- 0  

# exclude babyIDs existing in both preterm and full term categories from full term category
t_claim_delivery_full <- t_claim_mom_full   
rm(t_claim_mom_full)
ID_overlap <- intersect(t_claim_delivery_full$MEMBER_ID_Baby, t_claim_delivery_preterm$MEMBER_ID_Baby)
length(ID_overlap)
# 734,755,1219,1337,1215,1208,1220,1222,1218,1185,1189,1216,1174,
t_claim_delivery_full <- t_claim_delivery_full[!(t_claim_delivery_full$MEMBER_ID_Baby%in%ID_overlap),]  


# combine deliveries from full term + preterm/immature and decide final status

pchp_delivery <- as.data.frame(rbindlist(list(t_claim_delivery_full, t_claim_delivery_preterm))) 
nrow(pchp_delivery)
# 7877,7911,8062,8825,7992,7880,7958,7943,7895,7754,7813,7831,7668,
#names(pchp_delivery)
names(pchp_delivery) <- c('MEMBER_ID_Baby','recent_DOS_baby','ext_premature','very_premature','moder_premature','late_premature','prem_imm','full_term','MommyLink_ID','Delivery')

pchp_delivery$very_premature[which(pchp_delivery$ext_premature==1)]<-0
pchp_delivery$moder_premature[which(pchp_delivery$ext_premature==1)]<-0
pchp_delivery$late_premature[which(pchp_delivery$ext_premature==1)]<-0
pchp_delivery$prem_imm[which(pchp_delivery$ext_premature==1)]<-1

pchp_delivery$moder_premature[which(pchp_delivery$very_premature==1)]<-0
pchp_delivery$late_premature[which(pchp_delivery$very_premature==1)]<-0
pchp_delivery$prem_imm[which(pchp_delivery$very_premature==1)]<-1

pchp_delivery$late_premature[which(pchp_delivery$moder_premature==1)]<-0
pchp_delivery$prem_imm[which(pchp_delivery$moder_premature==1)]<-1

pchp_delivery <- pchp_delivery[, !names(pchp_delivery)%in%c("recent_DOS_baby")]
pchp_delivery <- unique(pchp_delivery)  
nrow(pchp_delivery)
# 7877,7911,8062,8825,7992,7880,7958,7943,7895,7754,7813,7831,7668,

pchp_claims <- t_claim     # already removed multiple gestations from DGNs in step 1
rm(Link,t_claim_baby_full,t_claim_baby_preimm,t_claim_delivery_full,t_claim_delivery_preterm,ext_premature,full_term,late_premature,moder_premature,
   prem_imm,very_premature,ID_overlap,t_claim,t_claim_baby_full_nomom,t_claim_baby_preimm_nomom)


# check and delete outliers - multiple gestations and deliveries

# Check1: whether multiple gestation exists - same day, same mom, multiple babies
Check_mom <- sqldf("select MEMBER_ID_Baby, MommyLink_ID, Delivery, COUNT(distinct(MEMBER_ID_Baby)) as num_baby, min(MEMBER_ID_Baby) as Min_MEMBER_ID_Baby,
                    max(MEMBER_ID_Baby) as Max_MEMBER_ID_Baby from pchp_delivery group by MommyLink_ID, Delivery")
pchp_delivery1 <- pchp_delivery[!pchp_delivery$MommyLink_ID %in% c(Check_mom[Check_mom$num_baby >= 2,]$MommyLink_ID), ]  

# Check2: whether multiple deliveries exist - same mom, multiple babies
Check_mom2 <- sqldf("select MEMBER_ID_Baby, MommyLink_ID, Delivery, COUNT(distinct(MEMBER_ID_Baby)) as num_baby, min(MEMBER_ID_Baby) as Min_MEMBER_ID_Baby,
                     max(MEMBER_ID_Baby) as Max_MEMBER_ID_Baby from pchp_delivery1 group by MommyLink_ID")  # Min and Max columns are not useful 
write.csv(pchp_delivery1[pchp_delivery1$MommyLink_ID%in%Check_mom2[Check_mom2$num_baby>=2, ]$MommyLink_ID, ], 
          "/home/michelle/T-Drive/PCHP PreTerm/Data/multipleDelivery.csv")

multiple_delivery <- Check_mom2[Check_mom2$num_baby>=2,]  # moms with more than one deliveries
pchp_delivery_1baby <- pchp_delivery1[!pchp_delivery1$MommyLink_ID%in%multiple_delivery$MommyLink_ID, ]  # moms with only 1 delivery
pchp_delivery_1baby <- pchp_delivery_1baby[!duplicated(pchp_delivery_1baby),]

two_delivery <- Check_mom2[Check_mom2$num_baby==2,]  # moms with exactly two deliveries, we remove >2 cases anyway
pchp_delivery_2baby <- pchp_delivery1[pchp_delivery1$MommyLink_ID %in% two_delivery$MommyLink_ID, ]   # moms with 2 deliveries
# check delivery time difference: if >= 6 months, keep; otherwise, remove
pchp_delivery_2baby <- sqldf("select MommyLink_ID, Delivery, min(Delivery) as first_delivery, max(Delivery) as second_delivery 
                              from pchp_delivery_2baby group by MommyLink_ID") 
pchp_delivery_2baby$first_delivery <- as.Date(pchp_delivery_2baby$first_delivery, origin = "1970-01-01")
pchp_delivery_2baby$second_delivery <- as.Date(pchp_delivery_2baby$second_delivery, origin = "1970-01-01")
pchp_delivery_2baby$delivery_difference <- pchp_delivery_2baby$second_delivery - pchp_delivery_2baby$first_delivery
pchp_delivery_2baby_reasonable <- pchp_delivery_2baby[pchp_delivery_2baby$delivery_difference >= 183,]
pchp_delivery_2baby_reasonable <- pchp_delivery1[pchp_delivery1$MommyLink_ID %in% pchp_delivery_2baby_reasonable$MommyLink_ID, ]
pchp_delivery_2baby_reasonable <- pchp_delivery_2baby_reasonable[!duplicated(pchp_delivery_2baby_reasonable), ]

pchp_delivery <- rbind(pchp_delivery_1baby, pchp_delivery_2baby_reasonable)
pchp_delivery <- pchp_delivery[, !names(pchp_delivery) %in% c('MEMBER_ID_Baby')]
pchp_delivery$delivery_ID <- paste0(pchp_delivery$MommyLink_ID, pchp_delivery$Delivery)

pchp_delivery <- unique(pchp_delivery)  
nrow(pchp_delivery)
# 7698,7724,7849,8598,7789,7681,7857,7737,7680,7544,7598,7599,7450,
rm(pchp_delivery1,Check_mom,Check_mom2,multiple_delivery,pchp_delivery_1baby,two_delivery,pchp_delivery_2baby,pchp_delivery_2baby_reasonable)


# create pregnancy date from delivery date and save complete delivery cohort for last year

pchp_delivery$Pregnancy <- pchp_delivery$Delivery - weeks(40)
table(is.na(pchp_delivery$Pregnancy))  # expect all F
#length(which(is.na(pchp_delivery$Pregnancy)))  # expect 0
pchp_delivery$MEMBER_ID <- pchp_delivery$MommyLink_ID
# CHANGE month below, month is not abbreviation, month is when you supposed to run the report
save(pchp_delivery, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/March_1_year_lookback.RData")

# generate current variables - up to 4 weeks prior to delivery - Q: where is this filter condition in claims data?

# need 1.5-year claims data, otherwise reload
range(pchp_claims$DOS)

t_claim <- pchp_claims
# filter claims for moms in cohort, who delivered babies in the past year
t_claim <- t_claim[t_claim$MEMBER_ID%in%pchp_delivery$MEMBER_ID,] 
# NOT changing this date
#t_claim <- t_claim[t_claim$DOB < as.Date("2015-10-01")-years(1),] 
t_claim <- t_claim[t_claim$DOB < as.Date("2015-10-01")-as.difftime(as.integer(365.25*1), units = "days"),] 

# Variable Generation - prenantal visit indicator, preterm history
# prenatal visits 
# added 2017/2018 ICD10 codes effective since 10/02/2017

# xiao commented: added 'Z36.2' in May 30, 2019
prenatal_BICD1 <- paste(c('Z36.2','V22.0','Z34.00','V22.1','Z34.80','Z34.90','V22.2','Z33.1','Z33.3','V23.0','O09.00','V23.1','O09.10','O09.A0','V23.2','O09.291',                                                                                                   'V23.3','O09.40','V23.4','V23.41','O09.211','V23.42','O09.10',                                                                  
                          'V23.49','O09.291','V23.5','O09.291','V23.7','O09.30','V23.8','V23.81','O09.511','V23.82','O09.521','V23.83','O09.611',                                                             
                          'V23.84','O09.621','V23.85','O09.819','V23.86','O09.821','O09.822','O09.823','O09.829','V23.87','O36.80X0',                                                           
                          'V23.89','O09.891','O09.892','O09.893','O09.899','V23.9','O09.90','O09.91','O09.92','O09.93',                                         
                          'V28.0','Z36','V28.1','V28.2','V28.3','V28.4','V28.5','V28.6','V28.8','V28.81','V28.82','V28.89','V28.9',
                          'Z36.0','Z36.1','Z36.89','Z36.3','Z36.4','Z36.5','Z36.85','Z36.89','Z36.89',
                          'Z36.81','Z36.82','Z36.83','Z36.84','Z36.86','Z36.87','Z36.88','Z36.89','Z36.8A','Z36.9'))

# DELETED some duplicated codes, not all
# xiao commented: changed from '45.001' to 'O45.001' in Apr 15, 2019
# xiao commented: added '640.00', '640.80', '640.90', "O33.7", "O34.21", "O34.51/2/3/9" in May 30, 2019
prenatal_BICD2 <- paste(c('640.00','640.80','640.90','O33.7','O34.21','O34.51','O34.52','O34.53','O34.59',
                          '640.03','O20.0', '640.83','O20.8', '640.93', 'O20.9',                                                              
                          '641.03','O44.01','O44.02','O44.03','O44.21','O44.22','O44.23','O44.41','O44.42','O44.43',     
                          '641.13','O44.11','O44.12','O44.13','O44.31','O44.32','O44.33','O44.51','O44.52','O44.53',     
                          '641.23','O45.8X1','O45.8X2','O45.8X3','O45.91','O45.92','O45.93',                       
                          '641.33','O45.001','O45.002','O45.003','O45.011','O45.012','O45.013','O45.021','O45.022',    
                          'O45.023','O45.091','O45.092','O45.093','O46.001','O46.002','O46.003','O46.011',   
                          'O46.012','O46.013','O46.021','O46.022','O46.023','O46.091','O46.092','O46.093',    
                          '641.83','O46.8X1','O46.8X2','O46.8X3', '641.93','O46.91','O46.92','O46.93',                                                                            
                          '642.03','O10.011','O10.012','O10.013','O10.911','O10.912','O10.913','642.13','O10.411','O10.412','O10.413',                                            
                          '642.23','O11.1','O11.2','O11.3','O10.311','O10.312','O10.313','O10.211',                 
                          'O10.212','O10.213','O10.111','O10.112','O10.113','642.33','O13.1','O13.2','O13.3','O16.1','O16.2','O16.3',                                          
                          '642.43','O14.02','O14.03','O14.92','O14.93','642.53','O14.12','O14.13','O14.22','O14.23',                                        
                          '642.63','O15.02', 'O15.03','642.73','O11.1','O11.2','O11.3',                                                  
                          '642.93','O16.1','O16.2','O16.3','643.03','O21.0','643.13','O21.1','643.23','O21.2',                                                              
                          '643.83','O21.8','643.93','O21.9','644.03','O60.02','O60.03','644.13','O47.02','O47.03','O47.1',                                                
                          '645.13','O48.0','645.23','O48.1','646.03','O31.01X0','O31.02X0','O31.03X0',                                         
                          '646.13','O12.01','O12.02','O12.03','O12.21','O12.22','O12.23','O26.01','O26.02','O26.03',     
                          '646.23','O26.831','O26.832','O26.833','646.33','O26.21','O26.22','O26.23',                                               
                          '646.43','O26.821','O26.822','O26.823','646.53','O23.41','O23.42','O23.43',                                               
                          '646.63','O23.91','O23.92','O23.93','646.73','O26.611','O26.612','O26.613',                                            
                          '646.83','O26.11','O26.12','O26.13','O26.41','O26.42','O26.43','O26.811','O26.812','O26.813',  
                          'O26.891','O26.892','O26.893','O99.89','646.93','O99.89','647.03','O98.111','O98.112','O98.113',                                            
                          '647.13','O98.211','O98.212','O98.213','647.23','O98.311','O98.312','O98.313','647.33','O98.011','O98.012','O98.013',                                            
                          '647.43','O98.611','O98.612','O98.613','647.53','O98.511','O98.512','O98.513','647.63','O98.511','O98.512','O98.513',                                            
                          '647.83','O98.611','O98.612','O98.613','O98.811','O98.812','O98.813','647.93','O98.911','O98.912','O98.913',                                            
                          '648.03','O24.911','O24.912','O24.913','648.13','O99.281','O99.282','O99.283','648.23','O99.011','O99.012','O99.013',                                  
                          '648.33','O99.321','O99.322','O99.323','648.43','O99.341','O99.342','O99.343','648.53','O99.411','O99.412','O99.413',                                            
                          '648.63','O99.411','O99.412','O99.413','648.73','O33.0','648.83','O24.415','O24.419','O99.810',                                            
                          '648.93','O25.11','O25.12','O25.13','O99.281','O99.282','O99.283','649.03','O99.331','O99.332','O99.333',                                            
                          '649.13','O99.211','O99.212','O99.213','649.23','O99.841','O99.842','O99.843','649.33','O99.111','O99.112','O99.113',                                            
                          '649.43','O99.351','O99.352','O99.353','649.53','O26.851','O26.852','O26.853','649.63','O26.841','O26.842','O26.843',                                            
                          '649.73','O26.872','O26.873','651.03','O30.001','O30.002','O30.003','651.13','O30.101','O30.102','O30.103',                                            
                          '651.23','O30.201','O30.202','O30.203', '651.33','O31.11X0','651.43','651.53','651.63','651.73','O31.31X0','O31.32X0','O31.33X0',                                         
                          '651.83','O30.801','O30.802','O30.803','O31.8X10','O31.8X20','O31.8X30','651.93','O30.91','O30.92','O30.93',                                               
                          '652.03','O32.0XX0','652.13','O32.1XX0','652.23','O32.1XX0','652.33','O32.2XX0','652.43','O32.3XX0','652.53','O32.4XX0',                                                          
                          '652.63','O32.9XX0','652.73','O32.8XX0','652.83','O32.6XX0','O32.8XX0','652.93','O32.9XX0','653.03','O33.0',                                                              
                          '653.13','O33.1','653.23','O33.2','653.33','O33.3XX0','653.43','O33.4XX0','653.53','O33.5XX0','653.63','O33.6XX0',                                                           
                          '653.73','O33.7XX0','O33.7XX1','O33.7XX2','O33.7XX3','O33.7XX4','O33.7XX5','O33.7XX9',     
                          '653.83','O33.8','653.93','O33.9','654.03','O34.01','O34.02','O34.03','654.13','O34.11','O34.12','O34.13',                                               
                          '654.23','O34.211','O34.212','O34.219','654.33','O34.511','O34.512','O34.513','O34.531','O34.532','O34.533',                    
                          '654.43','O34.521','O34.522','O34.523','O34.591','O34.592','O34.593','654.53','O34.31','O34.32','O34.33',                                               
                          '654.63','O34.41','O34.42','O34.43','654.73','O34.61','O34.62','O34.63','654.83','O34.71','O34.72','O34.73',                                               
                          '654.93','O34.29','O34.81','O34.82','O34.83','O34.91','O34.92','O34.93','655.03','O35.0XX0','655.13','O35.1XX0',                                                           
                          '655.23','O35.2XX0','655.33','O35.3XX0','655.43','O35.4XX0','655.53','O35.5XX0','655.63','O35.6XX0',                                                           
                          '655.73','O36.8120','O36.8130','O36.8190','655.83','O35.8XX0','655.93','O35.9XX0','656.03','O43.011',                                                            
                          '656.13','O36.0110','O36.0120','O36.0130','O36.0910','O36.0920','O36.0930',                  
                          '656.23','O36.1110','O36.1120','O36.1130','O36.1910','O36.1920','O36.1930','656.33','O68',                                                                
                          '656.43','O36.4XX0','656.53','O36.5110','O36.5120','O36.5130','O36.5910','O36.5920','O36.5930',              
                          '656.63','O36.61X0','O36.62X0','O36.63X0','656.73','O43.101','O43.102','O43.103','O43.811','O43.812','O43.813',                   
                          'O43.91','O43.92','O43.93','656.83','O36.8910','O36.8920','O36.8930','O68',                                     
                          '656.93','O36.91X0','O36.92X0','O36.93X0','657.03','O40.1XX0','O40.2XX0','O40.3XX0',                                         
                          '658.03','O41.01X0','O41.02X0','O41.03X0','658.13','O42.011','O42.012','O42.013',                                            
                          '658.23','O42.111','O42.112','O42.113','658.33','O75.5',                                                              
                          '658.43','O41.1010','O41.1020','O41.1030','O41.1210','O41.1220','O41.1230','O41.1410','O41.1420','O41.1430',                                         
                          '658.83','O41.8X10','O41.8X20','O41.8X30','658.93','O41.91X0','O41.92X0','O41.93X0','659.03','O61.1',                                                              
                          '659.13','O61.0','659.23','O75.2','659.33','O75.3',                                                              
                          '659.43','O09.41','O09.42','O09.43','659.53','O09.511','O09.512','O09.513','659.63','O09.521','O09.522','O09.523',                                            
                          '659.73','O76','659.83','O75.89', '659.93','O75.9','678.03','O35.8XX0','O36.8210','O36.8220','O36.8230',                                
                          '678.13','O30.021','O30.022','O30.023','679.03','O26.891','O26.892','O26.893','679.13','O35.7XX0'))                                                        

prenatal_BICD <- c(unique(prenatal_BICD1), unique(prenatal_BICD2))  # 646

# xiao changed from 76810 to 76801 in Apr 11, 2019 and added 80055 in May 30, 2019
# xiao commented: removed 80055 and added 76802,76810,76812,76814,76819,75.35,68.19 in Mar 16, 2020
# OB ultrasound CPT codes and prenatal CPT
prenatal_BCPT <- paste(c('76801','76802','76805','76810','76811','76812','76813','76814','76815','76816','76817','76818','76819',
                         'BY4FZZZ','BY49ZZZ','BY4CZZZ','88.78','75.35','68.19'))
prenatal_B <- c(prenatal_BICD, prenatal_BCPT)

prenatal_C <- paste(c('99201','99202','99203','99204','99205','99211','99212','99213','99214','99215','99241','99242','99243','99244','99245'))

prenatal_A <- paste(c('59400','59425','59426','59510','59610','59618','H1000','H1001','H1002','H1003','H1004','H1005','0500F','0501F','0502F'))

t_claim$prenatal1 <- as.numeric(t_claim$DGN1%in%prenatal_A | t_claim$DGN2%in%prenatal_A | t_claim$DGN3%in%prenatal_A | t_claim$DGN4%in%prenatal_A | t_claim$DGN5%in%prenatal_A |
                                  t_claim$PROC_1%in%prenatal_A | t_claim$PROC_2%in%prenatal_A | t_claim$PROC_3%in%prenatal_A | t_claim$PROC_4%in%prenatal_A)

t_claim$prenatal2 <- as.numeric((t_claim$DGN1%in%prenatal_C | t_claim$DGN2%in%prenatal_C | t_claim$DGN3%in%prenatal_C | t_claim$DGN4%in%prenatal_C | t_claim$DGN5%in%prenatal_C |
                                   t_claim$PROC_1%in%prenatal_C | t_claim$PROC_2%in%prenatal_C | t_claim$PROC_3%in%prenatal_C | t_claim$PROC_4%in%prenatal_C) &
                                  (t_claim$DGN1%in%prenatal_B | t_claim$DGN2%in%prenatal_B | t_claim$DGN3%in%prenatal_B | t_claim$DGN4%in%prenatal_B | t_claim$DGN5%in%prenatal_B |
                                     t_claim$PROC_1%in%prenatal_B | t_claim$PROC_2%in%prenatal_B | t_claim$PROC_3%in%prenatal_B | t_claim$PROC_4%in%prenatal_B))

t_claim$prenatal <- as.numeric(t_claim$prenatal2 == 1 | t_claim$prenatal1 == 1)
table(t_claim$prenatal)  # 72035,71202,71284,76879,69492,66124,62550,59190,57007,55654,55386,56224,55217,

# preterm labor history, how is it different from Prehist1?
# supervision of pregnancy with history of preterm labor
Prehist2 <- paste(c('Z87.51','V13.21','O60.00','O09.211','O09.212','O09.213','O09.219','V23.41'))

t_claim$Prehist2 <- as.numeric(t_claim$DGN1%in%Prehist2 | t_claim$DGN2%in%Prehist2 | t_claim$DGN3%in%Prehist2 | t_claim$DGN4%in%Prehist2 | t_claim$DGN5%in%Prehist2)

t_claim_111 <- plyr::join(t_claim[, c('DOS','DOB','ATT_PROV_ID','PaidAmount','MEMBER_ID','inpt','ED','outpt','prenatal','Prehist2')], pchp_delivery, type = 'left', by = 'MEMBER_ID')     

# calculate from Pregnancy to 4 weeks prior to delivery  
t_claim_1111 <- sqldf("select * from t_claim_111 WHERE DOS > Pregnancy AND DOS < Delivery")   
t_claim_1111 <- t_claim_1111[!duplicated(t_claim_1111), ] 
t_claim_1111 <- t_claim_1111[order(t_claim_1111$PaidAmount, decreasing = TRUE), ]
t_claim_previsit <- sqldf("select MEMBER_ID, delivery_ID, DOB, DOS, ATT_PROV_ID, Pregnancy, sum(inpt)>0 as inpt_b, sum(ED)>0 as ED_b, sum(outpt)>0 as outpt_b, 
                           sum(prenatal)>0 as prenatal_b, sum(Prehist2)>0 as Prehist2_b, sum(PaidAmount * prenatal) as paidPre, sum(PaidAmount * inpt) as paidinpt, 
                           sum(PaidAmount * ED) as paided, sum(PaidAmount * outpt) as paidout 
                           from t_claim_1111 group by delivery_ID, DOS")    
t_claim_previsit1 <- sqldf("select MEMBER_ID, delivery_ID, DOB, Pregnancy, sum(inpt_b) as num_inpt, sum(ED_b) as num_ED, sum(outpt_b) as num_outpt,
                            sum(prenatal_b) as num_pre, sum(Prehist2_b)>0 as Prehist2, sum(paidPre) as Pre_pd, sum(paidinpt) as Inpt_pd, 
                            sum(paided) as ED_pd, sum(paidout) as Out_pd 
                            from t_claim_previsit group by delivery_ID")
t_claim_previsit_new <- t_claim_previsit[!t_claim_previsit$ATT_PROV_ID %in% c('PROV0000P05180','PROV0000P12161'), ]

# extract prenatal-related visit information 
t_claim_previsit_prenatal<-t_claim_previsit_new[order(t_claim_previsit_new$delivery_ID,t_claim_previsit_new$DOS),]
t_claim_previsit_prenatal<-t_claim_previsit_prenatal[t_claim_previsit_prenatal$prenatal_b==1,]
t_claim_previsit_recent<-t_claim_previsit_prenatal%>%group_by(delivery_ID) %>%filter(row_number()==n())
t_claim_previsit_recent<-data.frame(t_claim_previsit_recent)
t_claim_previsit_recent$Recent_pre_PROV_ID<-t_claim_previsit_recent$ATT_PROV_ID
t_claim_previsit11<-sqldf("select MEMBER_ID,delivery_ID,Pregnancy, max(DOS) as recent_Previsit, min(DOS) as first_Previsit 
                           from t_claim_previsit where prenatal_b==1 group by delivery_ID")
t_claim_previsit11$recent_Previsit <- as.Date(t_claim_previsit11$recent_Previsit, origin = "1970-01-01")
t_claim_previsit11$first_Previsit <- as.Date(t_claim_previsit11$first_Previsit, origin = "1970-01-01")
t_claim_previsit11 <- plyr::join(t_claim_previsit11,t_claim_previsit_recent[, c('delivery_ID','Recent_pre_PROV_ID')], type = 'left', by = 'delivery_ID') 
rm(t_claim_previsit_prenatal, t_claim_previsit_recent)

# extract outpatient-related visit information 
t_claim_previsit_outpt<-t_claim_previsit_new[order(t_claim_previsit_new$delivery_ID,t_claim_previsit_new$DOS),]
t_claim_previsit_outpt<-t_claim_previsit_outpt[t_claim_previsit_outpt$outpt_b==1,]
t_claim_previsit_recent_1<-t_claim_previsit_outpt%>%group_by(delivery_ID) %>%filter(row_number()==n())
t_claim_previsit_recent_1<-data.frame(t_claim_previsit_recent_1)
t_claim_previsit_recent_1$Recent_out_PROV_ID<-t_claim_previsit_recent_1$ATT_PROV_ID
t_claim_outpt22<-sqldf("select MEMBER_ID,delivery_ID, Pregnancy,max(DOS) as recent_outpt, min(DOS) as first_outpt 
                        from t_claim_previsit where outpt_b==1 group by delivery_ID")
t_claim_outpt22$recent_outpt <- as.Date(t_claim_outpt22$recent_outpt, origin = "1970-01-01")
t_claim_outpt22$first_outpt <- as.Date(t_claim_outpt22$first_outpt, origin = "1970-01-01")
t_claim_outpt22 <- plyr::join(t_claim_outpt22, t_claim_previsit_recent_1[, c('delivery_ID','Recent_out_PROV_ID')], type = 'left', by = 'delivery_ID') 
rm(t_claim_previsit_recent_1, t_claim_previsit_outpt)


t_claim_previsit111 <- plyr::join(t_claim_previsit1, t_claim_previsit11[, c('delivery_ID','recent_Previsit','first_Previsit','Recent_pre_PROV_ID')], type = 'left', by = 'delivery_ID') 
t_claim_previsit111 <- plyr::join(t_claim_previsit111, t_claim_outpt22[, c('delivery_ID','recent_outpt','first_outpt','Recent_out_PROV_ID')], type = 'left', by = 'delivery_ID') 
Current_P <- plyr::join(pchp_delivery,t_claim_previsit111[,c('delivery_ID','MEMBER_ID','DOB','num_inpt','num_ED','num_outpt','num_pre','Pre_pd','Inpt_pd','ED_pd','Out_pd',
                                                             'recent_Previsit','first_Previsit','recent_outpt','first_outpt','Recent_pre_PROV_ID','Recent_out_PROV_ID','Prehist2')], 
                        type = 'left', by = 'delivery_ID')  
nrow(Current_P)
# 7698,7849,8598,7789,7681,7759,7737,7680,7544,7598,7599,7450,

rm(Prehist2,prenatal_A,prenatal_B,prenatal_BCPT,prenatal_BICD,prenatal_BICD1,prenatal_BICD2,prenatal_C)
rm(t_claim_111,t_claim_1111,t_claim_outpt22,t_claim_previsit,t_claim_previsit_new,t_claim_previsit1,t_claim_previsit11,t_claim_previsit111)


# complete DOB info from mem data, when DOB in claims is NA

# add DOB info using mem data after 2017 to now 
# CHANGE month in the following three lines, recall that these two mem data files are generated in step 9
#load("/home/michelle/T-Drive/PCHPAsthma/Data/MemberOnlyData/PCHP_mem_After2017_202102.RData")
load("/home/michelle/preterm_monthly_report/PCHP_mem_After2017_202102.RData")
mem <- pchp_mem_After2017to202102
rm(pchp_mem_012017to02_2021, pchp_mem_After2017to202102)

mem_subset <- mem[mem$MemberID %in% Current_P[is.na(Current_P$DOB),]$MEMBER_ID,]  
#mem_subset <- mem_subset[mem_subset$DOB < as.Date("2015-10-01")-years(1),]
mem_subset <- mem_subset[mem_subset$DOB < as.Date("2015-10-01")-as.difftime(as.integer(365.25*1), units = "days"),]
names(mem_subset)[1] <- 'MEMBER_ID'
mem_subset <- unique(mem_subset[, c('MEMBER_ID','DOB')])
print(nrow(mem_subset))   # 153,155,154,171,155,167,173,185,191,180,171,177,169,
Current_P <- plyr::join(Current_P, mem_subset, type = 'left', by = 'MEMBER_ID') 
names(Current_P)[length(names(Current_P))] <- 'DOB_fill'
Current_P[is.na(Current_P$DOB),]$DOB <- Current_P[is.na(Current_P$DOB),]$DOB_fill
Current_P <- Current_P[,!names(Current_P) %in% c('DOB_fill')]
Current_P <- Current_P[,!names(Current_P) %in% c('MEMBER_ID.1')]  
rm(mem, mem_subset)

# add DOB info using mem data before 2017
# DO NOT CHANGE this file's name!
#load("/home/michelle/T-Drive/PCHPAsthma/Data/MemberOnlyData/PCHP_mem_2014to2017Before.RData")
load("/home/michelle/preterm_monthly_report/PCHP_mem_2014to2017Before.RData")  # 10457198
range(pchp_mem_2014to2017Before$Date)  # "2014-10-01" "2016-12-01"
mem <- pchp_mem_2014to2017Before
mem_sub <- mem[pchp_mem_2014to2017Before$MemberID %in% Current_P[is.na(Current_P$DOB),]$MEMBER_ID, ]
#mem_sub <- mem_sub[mem_sub$DOB < as.Date("2015-10-01")-years(1), ]
mem_sub <- mem_sub[mem_sub$DOB < as.Date("2015-10-01")-as.difftime(as.integer(365.25*1), units = "days"), ]
names(mem_sub)[1] <- 'MEMBER_ID'
mem_sub <- unique(mem_sub[, c('MEMBER_ID','DOB')])
print(nrow(mem_sub))  # 3,3,2,1,1,2,1,2,2,1,1,0,
Current_P <- plyr::join(Current_P, mem_sub, type = 'left', by = 'MEMBER_ID') 
names(Current_P)[length(names(Current_P))] <- 'DOB_fill'
Current_P[is.na(Current_P$DOB),]$DOB <- Current_P[is.na(Current_P$DOB),]$DOB_fill
Current_P <- Current_P[, !names(Current_P)%in%c('DOB_fill')]
Current_P <- Current_P[, !names(Current_P)%in%c('MEMBER_ID.1')]  
rm(pchp_mem_2014to2017Before, mem, mem_sub)


# Generate variables - insurance and its gaps

# Generate insurance varaibles within the past 12 months

# CHANGE month below
#load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/Raw_Data_January_2021/PCHP_elg_new_January.RData")
load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/PCHP_elg_new_February.RData")

names(pchp_elg)[1]<-'MEMBER_ID'

pchp_elg <- pchp_elg[pchp_elg$MEMBER_ID %in% Current_P$MEMBER_ID, ]
pchp_elg1 <- plyr::join(pchp_elg[,c('MEMBER_ID','FROM_DT_I2','THRU_DT_I2')], Current_P, type = 'left', by = 'MEMBER_ID') 
pchp_elg1$Anchor<-pchp_elg1$Delivery
pchp_elg1$back12<-pchp_elg1$Anchor-days(365)
#pchp_elg1$key<-pchp_elg1$delivery_ID
pchp_elg1<-pchp_elg1[!duplicated(pchp_elg1), ]

start_dt<-pchp_elg1$back12
end_dt<-pchp_elg1$Anchor
pchp_elg<-pchp_elg1
elg_gap <- pchp_elg[(pchp_elg$THRU_DT_I2 >= start_dt) & (pchp_elg$FROM_DT_I2 <= end_dt), ] 
elg_gap<-elg_gap[, c("delivery_ID","FROM_DT_I2","THRU_DT_I2","back12","Anchor")]
elg_gap1<-elg_gap[, c("delivery_ID","FROM_DT_I2","THRU_DT_I2")]

temp <- elg_gap[, c('delivery_ID','back12','Anchor')]
names(temp)[2] <- 'FROM_DT_I2'
names(temp)[3] <- 'THRU_DT_I2'
temp$THRU_DT_I2 <- temp$FROM_DT_I2
temp <- temp[!duplicated(temp), ]
elg_gap1 <- rbind(elg_gap1, temp)

temp <- elg_gap[,c('delivery_ID','back12','Anchor')]
names(temp)[2] <- 'FROM_DT_I2'
names(temp)[3] <- 'THRU_DT_I2'
temp$FROM_DT_I2 <- temp$THRU_DT_I2
temp <- temp[!duplicated(temp), ]
elg_gap1 <- rbind(elg_gap1, temp)

temp <- elg_gap[, c('delivery_ID','back12','Anchor')]
elg_gap1 <- plyr::join(elg_gap1, temp, type = 'left', by = 'delivery_ID') 
elg_gap1 <- elg_gap1[!duplicated(elg_gap1), ]

elg_gap1$FROM_DT_I2 <- pmax(elg_gap1$FROM_DT_I2, elg_gap1$back12)
elg_gap1$THRU_DT_I2 <- pmin(elg_gap1$THRU_DT_I2, elg_gap1$Anchor)

elg_gap1 <- elg_gap1[order(elg_gap1$delivery_ID, elg_gap1$FROM_DT_I2, elg_gap1$THRU_DT_I2), ]
elg_gap1 <- elg_gap1[!duplicated(elg_gap1), ]

gap_vec <- as.numeric(c(0, (elg_gap1$FROM_DT_I2[-1]) - elg_gap1$THRU_DT_I2[1:(length(elg_gap1$THRU_DT_I2)-1)]))-1
gap_vec[gap_vec<=0] <- 0

elg_gap1$gap_days <- gap_vec

last_timegap_s<-elg_gap1%>%group_by(delivery_ID) %>%filter(row_number()==n()-1)
last_timegap_s<-data.frame(last_timegap_s)
last_timegap_s$lasttime<-as.numeric(last_timegap_s$THRU_DT_I2-last_timegap_s$FROM_DT_I2)
names(last_timegap_s)[7]<-'last_gap_days'
last_timegap_s$recent_elg_start<-last_timegap_s$FROM_DT_I2
last_timegap_s$recent_elg_end<-last_timegap_s$THRU_DT_I2

first_timegap<-elg_gap1%>%group_by(delivery_ID) %>%filter(row_number()==1)
first_timegap<-data.frame(first_timegap)
names(first_timegap)[6]<-'first_gap_days'

second_timegap<-elg_gap1%>%group_by(delivery_ID) %>%filter(row_number()==2)
second_timegap<-data.frame(second_timegap)
second_timegap$first_elg_start<-second_timegap$FROM_DT_I2

gap_table <- sqldf('select delivery_ID, sum(gap_days>0) as n_gaps, sum( gap_days) as gap_days from elg_gap1 group by delivery_ID')
gap_table <- plyr::join(gap_table ,second_timegap[,c('delivery_ID','first_elg_start')],type = 'left', by='delivery_ID') 
gap_table <- plyr::join(gap_table ,last_timegap_s[,c('delivery_ID','recent_elg_end','recent_elg_start','last_gap_days')],type = 'left', by='delivery_ID') 
gap_table <- plyr::join(gap_table ,first_timegap[,c('delivery_ID','first_gap_days')],type = 'left', by='delivery_ID') 
gap_table$correct<-0
gap_table$correct[gap_table$first_gap_days!=0]<-1
gap_table$n_gaps_correct<-gap_table$n_gaps-gap_table$correct
gap_table$gap_days_correct<-gap_table$gap_days-gap_table$first_gap_days

Current_P_Full<-plyr::join(Current_P,gap_table[,c('delivery_ID','first_elg_start','recent_elg_start','recent_elg_end','last_gap_days','gap_days_correct','n_gaps_correct')], 
                           type = 'left', by = 'delivery_ID')  
nrow(Current_P_Full)
# 7698,7724,7849,8598,7789,7681,7759,7737,7680,7544,7598,7599,7450,


# Generate insurance variables since Pregnancy date 

# CHANGE month below
#load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/Raw_Data_January_2021/PCHP_elg_new_January.RData")
load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/PCHP_elg_new_February.RData")

names(pchp_elg)[1]<-'MEMBER_ID'

pchp_elg <- pchp_elg[pchp_elg$MEMBER_ID%in%Current_P$MEMBER_ID,]
pchp_elg1 <- plyr::join(pchp_elg[,c('MEMBER_ID','FROM_DT_I2','THRU_DT_I2')], Current_P, type = 'left', by='MEMBER_ID') 
pchp_elg1$Anchor<-pchp_elg1$Delivery
pchp_elg1$back12<-pchp_elg1$Pregnancy
#pchp_elg1$key<-pchp_elg1$delivery_ID
pchp_elg1<-pchp_elg1[!duplicated(pchp_elg1),]

start_dt<-pchp_elg1$back12
end_dt<-pchp_elg1$Anchor
pchp_elg<-pchp_elg1
elg_gap = pchp_elg[(pchp_elg$THRU_DT_I2 >= start_dt) & (pchp_elg$FROM_DT_I2 <= end_dt),] 
elg_gap<-elg_gap[,c("delivery_ID","FROM_DT_I2","THRU_DT_I2","back12","Anchor")]
elg_gap1<-elg_gap[,c("delivery_ID","FROM_DT_I2","THRU_DT_I2")]

temp = elg_gap[,c('delivery_ID','back12','Anchor')]
names(temp)[2]='FROM_DT_I2'
names(temp)[3]='THRU_DT_I2'
temp$THRU_DT_I2=temp$FROM_DT_I2
temp=temp[!duplicated(temp),]
elg_gap1 = rbind( elg_gap1, temp)

temp = elg_gap[,c('delivery_ID','back12','Anchor')]
names(temp)[2]='FROM_DT_I2'
names(temp)[3]='THRU_DT_I2'
temp$FROM_DT_I2=temp$THRU_DT_I2
temp=temp[!duplicated(temp),]
elg_gap1 = rbind( elg_gap1, temp)

temp = elg_gap[,c('delivery_ID','back12','Anchor')]
elg_gap1<- plyr::join(elg_gap1,temp,type = 'left', by='delivery_ID') 
elg_gap1<-elg_gap1[!duplicated(elg_gap1),]

elg_gap1$FROM_DT_I2 = pmax( elg_gap1$FROM_DT_I2, elg_gap1$back12 )
elg_gap1$THRU_DT_I2 = pmin( elg_gap1$THRU_DT_I2, elg_gap1$Anchor )

elg_gap1<-elg_gap1[order(elg_gap1$delivery_ID,elg_gap1$FROM_DT_I2,elg_gap1$THRU_DT_I2),]
elg_gap1<-elg_gap1[!duplicated(elg_gap1),]

gap_vec = as.numeric( c( 0, (elg_gap1$FROM_DT_I2[-1]) - elg_gap1$THRU_DT_I2[1:(length(elg_gap1$THRU_DT_I2)-1)]) )-1
gap_vec[gap_vec<=0] = 0

elg_gap1$gap_days = gap_vec

last_timegap_s<-elg_gap1%>%group_by(delivery_ID) %>%filter(row_number()==n()-1)
last_timegap_s<-data.frame(last_timegap_s)
last_timegap_s$lasttime<-as.numeric(last_timegap_s$THRU_DT_I2-last_timegap_s$FROM_DT_I2)
names(last_timegap_s)[7]<-'last_gap_days'
last_timegap_s$recent_elg_start_now<-last_timegap_s$FROM_DT_I2
last_timegap_s$recent_elg_end_now<-last_timegap_s$THRU_DT_I2

first_timegap<-elg_gap1%>%group_by(delivery_ID) %>%filter(row_number()==1)
first_timegap<-data.frame(first_timegap)
names(first_timegap)[6]<-'first_gap_days'

second_timegap<-elg_gap1%>%group_by(delivery_ID) %>%filter(row_number()==2)
second_timegap<-data.frame(second_timegap)
second_timegap$first_elg_start_now<-second_timegap$FROM_DT_I2

gap_table <-sqldf('select delivery_ID, sum(gap_days>0) as n_gaps, sum( gap_days) as gap_days from elg_gap1 group by delivery_ID')
gap_table <-plyr::join(gap_table ,second_timegap[,c('delivery_ID','first_elg_start_now')],type = 'left', by='delivery_ID') 
gap_table <-plyr::join(gap_table ,last_timegap_s[,c('delivery_ID','recent_elg_end_now','recent_elg_start_now','last_gap_days')], type = 'left', by = 'delivery_ID') 
gap_table <-plyr::join(gap_table ,first_timegap[,c('delivery_ID','first_gap_days')],type = 'left', by='delivery_ID') 
gap_table$correct<-0
gap_table$correct[gap_table$first_gap_days!=0]<-1
gap_table$n_gaps_correct<-gap_table$n_gaps-gap_table$correct
gap_table$gap_days_correct<-gap_table$gap_days-gap_table$first_gap_days

Current_P_Full <- plyr::join(Current_P_Full,gap_table[,c('delivery_ID','first_elg_start_now','recent_elg_start_now','recent_elg_end_now')], type = 'left', by = 'delivery_ID')

Current_P <- Current_P_Full

rm(start_dt,end_dt,Current_P_Full,elg_gap,elg_gap1,first_timegap,gap_table,gap_vec,last_timegap_s,pchp_elg,pchp_elg1,second_timegap,temp)


# Assign the most recent PCPID

# CHANGE month below
#load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/Raw_Data_January_2021/PCHP_elg_new_January.RData")
load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/PCHP_elg_new_February.RData")

names(pchp_elg)[1] <- 'MEMBER_ID'
pchp_elg[, 'PCPID'] <- str_replace_all(pchp_elg[, 'PCPID'], ' ', "")
pchp_elg <- pchp_elg[pchp_elg$MEMBER_ID %in% Current_P$MEMBER_ID, ]
pchp_elg <- pchp_elg[order(pchp_elg$THRU_DT_I2, decreasing = TRUE), ] 

Current_P_Full_PCP <- data.frame(MEMBER_ID = as.numeric(),
                                 Delivery = as.Date(character()),
                                 delivery_ID = character(), 
                                 #Anchor = as.Date(character()), 
                                 Pregnancy = as.Date(character()),
                                 PCPID = character(),
                                 stringsAsFactors = FALSE)

for (i in 1:dim(Current_P)[1]){
  end_dt <- Current_P$Delivery[i]
  # elg info during the last year prior to delivery, time window is different for different women
  pchp_elg_sub <- subset(pchp_elg, (pchp_elg$THRU_DT_I2 >= as.Date(end_dt)-days(365)) & (pchp_elg$FROM_DT_I2 <= as.Date(end_dt))) 
  pchp_elg_sub <- pchp_elg_sub[pchp_elg_sub$MEMBER_ID == Current_P$MEMBER_ID[i], ]
  Current_P_Full_PCP[i, ] <- plyr::join(Current_P[i, c('Delivery','delivery_ID','Pregnancy','MEMBER_ID')], pchp_elg_sub[, c("MEMBER_ID","PCPID")],
                                        by = "MEMBER_ID", type = "left", match = "first")  # Difference between first and merge ???
}

Current_P_Full_PCP <- unique(Current_P_Full_PCP[!is.na(Current_P_Full_PCP$PCPID), c("MEMBER_ID","PCPID", "delivery_ID")])  
Current_P_Full <- plyr::join(Current_P, Current_P_Full_PCP, by = "delivery_ID", type = "left")
names(Current_P_Full)[dim(Current_P_Full)[2]] <- 'Recent_PCPID'
Current_P <- Current_P[!is.na(Current_P$DOB), ]
Current_P_Full <- Current_P_Full[!is.na(Current_P_Full$DOB), ] 
rm(end_dt,pchp_elg,Current_P_Full_PCP,pchp_elg_sub,i)

# CHANGE month below
save(Current_P, Current_P_Full, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/March_1_year_lookback_assignPCP.RData")

# generate historical variables

rm(list = ls())

# CHANGE month below
#load("/home/michelle/T-Drive/PCHP PreTerm/Data/Accumulating Claim Data for generating monthly report/PCHP_claim_01012016_DATEwithoutmultiple.RData")
load("/home/michelle/preterm_monthly_report/PCHP_claim_01012016_02282021withoutmultiple.RData")
range(pchp_claims$DOS)  # expect 5-year

# this is just a list of all 12 months, year is not relevant
NIH_quarters <- seq.Date(as.Date('2017-01-01'), as.Date('2017-12-01'), "months")
NIH_quarters <- months(NIH_quarters)

History_pre <- function(t_claim, i){
  
  chip_prov <- paste("/home/michelle/T-Drive/PCHP PreTerm/Data/", NIH_quarters[i], "_1_year_lookback_assignPCP.RData", sep = '')   # data generated in previous chunk
  print(paste("loading data: ", chip_prov, sep = ""))
  load(chip_prov)
  
  # to filter moms in delivery cohort and filter DOB
  t_claim <- t_claim[t_claim$MEMBER_ID %in% Current_P_Full$MommyLink_ID, ] 
  #t_claim <- t_claim[t_claim$DOB < as.Date("2015-10-01")-years(1), ]
  t_claim <- t_claim[t_claim$DOB < as.Date("2015-10-01")-as.difftime(as.integer(365.25*1), units = "days"), ]
  
  # preterm labor history 
  # xiao added the last 1 code in Jun 4, 2020
  Prehist1 <- c('644.0','644.00','644.03','O60.00','O60.02','O60.03','O60')
  # preterm delivery history
  # xiao added the last 26 codes in Jun 4, 2020
  Prehist11 <- c('644.2','644.20','644.21','O60.10X0','O60.12X0','O60.13X0','O60.14X0','060.1','O60.2','O60.10X1','O60.10X2',
                 'O60.10X3','O60.10X4','O60.10X5','O60.10X9','O60.12X1','O60.12X2','O60.12X3','O60.12X4','O60.12X5','O60.12X9',
                 'O60.13X1','O60.13X2','O60.13X3','O60.13X4','O60.13X5','O60.13X9','O60.14X1','O60.14X2','O60.14X3','O60.14X4',
                 'O60.14X5','O60.14X9')
  
  t_claim$Prehist1 <- as.numeric(t_claim$DGN1%in%Prehist1| t_claim$DGN2%in%Prehist1| t_claim$DGN3%in%Prehist1| t_claim$DGN4%in%Prehist1| t_claim$DGN5%in%Prehist1)
  t_claim$Prehist11 <- as.numeric(t_claim$DGN1%in%Prehist11| t_claim$DGN2%in%Prehist11| t_claim$DGN3%in%Prehist11| t_claim$DGN4%in%Prehist11| t_claim$DGN5%in%Prehist11)
  t_claim_1 <- plyr::join(t_claim[,c('DOS','MEMBER_ID','Prehist1','Prehist11')], Current_P_Full[,c(1:13)], type = 'left', by = 'MEMBER_ID') 
  t_claim_111 <- sqldf("select * from t_claim_1 WHERE DOS <= Pregnancy")
  t_claim_111 <- t_claim_111[!duplicated(t_claim_111), ]    # check duplicates before calculating # of visits
  
  pchp_claim_Obstetric <- sqldf("select MEMBER_ID, delivery_ID, DOS, Pregnancy, sum(Prehist1)>0 as Prehist1_b, sum(Prehist11)>0 as Prehist11_b 
                                 from t_claim_111 group by delivery_ID, DOS")
  pchp_claim_Obstetric1 <- sqldf("select MEMBER_ID, delivery_ID, Pregnancy, sum(Prehist1_b)>0 as Prehist1, sum(Prehist11_b)>0 as Prehist11 
                                  from pchp_claim_Obstetric group by delivery_ID")
  Current_P_Full <- plyr::join(Current_P_Full, pchp_claim_Obstetric1[, c('delivery_ID','Prehist1','Prehist11')], type = 'left', by = 'delivery_ID')
  
  files <- paste("/home/michelle/T-Drive/PCHP PreTerm/Data/", NIH_quarters[i], "_1_year_lookback_assignPCP_pretermhis.RData", sep = '')
  save(Current_P_Full, file = files)
  
  rm(t_claim_1, t_claim_111, pchp_claim_Obstetric, pchp_claim_Obstetric1)
}

# CHANGE month below
History_pre(pchp_claims, 03)
rm(NIH_quarters, History_pre)


# generate preterm history variable from babies

range(pchp_claims$DOS)  # expect 5-year, otherwise reload
t_claim <- pchp_claims

# Generate preterm history from babies
prehist <- paste(c('765.0','765.00','P07.00','P07.10','765.01','P07.01','765.02','P07.02','765.03','P07.03','765.04','P07.14','765.05','P07.15',                                     
                   '765.06','P07.16','765.07','P07.17','765.08','P07.18','765.09','P07.30','765.1','765.10','P07.00','P07.10','765.11','P07.01',                                     
                   '765.12','P07.02','765.13','P07.03','765.14','P07.14','765.15','P07.15','765.16','P07.16','765.17','P07.17','765.18','P07.18',                                     
                   '765.19','P07.30','765.2','765.20','P07.20','P07.30','765.21','P07.21','P07.22','765.22','P07.23','765.23','P07.24','P07.25',                              
                   '765.24','P07.26','P07.31','765.25','P07.32','P07.33','765.26','P07.34','P07.35',                              
                   '765.27','P07.36','P07.37','765.28','P07.38','P07.39','765.29'))

t_claim$prehist <- as.numeric(t_claim$DGN1%in%prehist | t_claim$DGN2%in%prehist | t_claim$DGN3%in%prehist | t_claim$DGN4%in%prehist | t_claim$DGN5%in%prehist |
                                t_claim$PROC_1%in%prehist | t_claim$PROC_2%in%prehist | t_claim$PROC_3%in%prehist | t_claim$PROC_4%in%prehist)

t_claim <- t_claim[t_claim$prehist == 1,]

# CHANGE months below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/Mom_Baby_Link/Link_03142021_processed.RData")
load("/home/michelle/T-Drive/PCHP PreTerm/Data/March_1_year_lookback_assignPCP_pretermhis.RData")

Current_P_Full$Anchor <- Current_P_Full$Delivery - weeks(4)
table(is.na(Current_P_Full$Anchor))  # expect all F
t_claim_baby_pre <- plyr::join(t_claim, Link[, c('MEMBER_ID','MommyLink_ID')], type = 'left', by = 'MEMBER_ID') 
t_claim_baby_pre <- t_claim_baby_pre[!is.na(t_claim_baby_pre$MommyLink_ID),]
t_claim_baby_pre <- t_claim_baby_pre[!duplicated(t_claim_baby_pre),]
t_claim_baby_pre <- t_claim_baby_pre[t_claim_baby_pre$MommyLink_ID %in% Current_P_Full$MommyLink_ID,] 
t_claim_baby_pre <- t_claim_baby_pre[!is.na(t_claim_baby_pre$MommyLink_ID),]   # redundant!
t_claim_baby_pre <- unique(t_claim_baby_pre)   # redundant!
t_claim_baby_pre$Delivery1 <- t_claim_baby_pre$DOB
Count <- plyr::join(t_claim_baby_pre[, c('MommyLink_ID','Delivery1')], Current_P_Full, type = 'left', by = 'MommyLink_ID') 
Count1 <- sqldf("select * from Count WHERE Delivery1 < Anchor and Delivery > DOB")  # DOB is momDOB, Delivery1 is babyDOB
Count1 <- Count1[!duplicated(Count1), ]  
Count1$PreAge <- floor(as.numeric(Count1$Delivery1-Count1$DOB)/365.25)   # TODO: merge into one filter with Delivery1 > DOB into Delivery1 > DOB + 11 years?
Count1 <- Count1[Count1$PreAge>=11, ]  
nrow(Count1)
# 216,220,231,249,225,220,222,221,218,217,225,214,228,
Prehist_ID <- unique(Count1$delivery_ID)
length(Prehist_ID)
# 207,212,224,241,216,211,215,215,210,210,216,206,218,
Current_P_Full$Prehist <- 0
Current_P_Full$Prehist[Current_P_Full$delivery_ID%in%Prehist_ID] <- 1

# CHANGE month below
files <- paste("/home/michelle/T-Drive/PCHP PreTerm/Data/March_1_year_lookback_assignPCP_pretermhis_Final.RData", sep = '')
save(Current_P_Full, file = files)
rm(Count,Count1,Link,t_claim_baby_pre,Prehist_ID,prehist,files,Current_P_Full)


# add 17OH medication variables to delivery cohort

#rm(list = ls())

## load pharmarcy data
#load("/home/michelle/T-Drive/PCHPAsthma/Data/PCHP raw data/Raw_Data_January_2021/PCHP_rx_new_January.RData")
load("/home/michelle/preterm_monthly_report/PCHP_rx_new_February.RData") 
nrow(pchp_rx)
# 5991595,6077840,6190703,?,6309013,6436723,6513100,6577746,6644264,6709469,
range(pchp_rx$SERVICE_DT)  # "2015-01-01" "2021-01-31"
pchp_rx$MEMBER_ID <- as.numeric(pchp_rx$MEM_UNIQUE_ID)   # NA introduced

# CHANGE month below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/March_1_year_lookback_assignPCP_pretermhis_Final.RData")

# filter momID in delivery cohort
t_rx <- pchp_rx[pchp_rx$MEMBER_ID %in% Current_P_Full$MEMBER_ID,]  
nrow(t_rx)
# 90939,91735,94239,103179,94407,94378,95066,95037,95612,95360,97708,96824,95722,
rm(pchp_rx)

#t_rx <- t_rx[t_rx$SERVICE_DT<=end_dt, ]  # no need

t_rx <- sqldf("select MEMBER_ID, SERVICE_DT, DRUG_PRDCT_NAME, GENERIC_PRDCT_NAME, APPRVD_GROSS_DUE, MEM_BIRTH_DT from t_rx")  # NAs introduced
t_rx <- t_rx[!duplicated(t_rx), ]   
t_rx <- t_rx[!is.na(t_rx$SERVICE_DT), ]
# extract 17OH information
t_rx$OH_17<-as.numeric((str_detect(t_rx$GENERIC_PRDCT_NAME, c("PROGEST")) | str_detect(t_rx$DRUG_PRDCT_NAME, c("PROGESTERONE"))) & !(str_detect(t_rx$GENERIC_PRDCT_NAME, c("^MEDROXY"))))
table(t_rx$OH_17)  # 1050,1083,1095,1155,997,969,956,921,856,859,877,891,861,
# combine with claims data (Current_P_Full)
t_rx_1 <- plyr::join(t_rx, Current_P_Full[, c('MEMBER_ID','delivery_ID','DOB','Delivery','Pregnancy')], type = 'left', by = 'MEMBER_ID') 
t_rx_1 <- t_rx_1[t_rx_1$MEM_BIRTH_DT==t_rx_1$DOB, ]
t_rx_1 <- t_rx_1[!duplicated(t_rx_1), ]  
t_rx_11<-sqldf("select MEMBER_ID, delivery_ID, SERVICE_DT, DOB, sum(APPRVD_GROSS_DUE) as gross, sum(OH_17)>0 as OH_17_b 
                from t_rx_1 where SERVICE_DT>Pregnancy AND SERVICE_DT<Delivery group by delivery_ID, SERVICE_DT")
t_rx_111_17OH<-sqldf("select MEMBER_ID, delivery_ID, max(SERVICE_DT) as max_DOS_17OH, min(SERVICE_DT) as min_DOS_17OH,DOB, sum(gross) as Totalgross_now, 
                      sum(OH_17_b) as num_OH_17_now from t_rx_11 where OH_17_b=1 group by delivery_ID")
t_rx_111_17OH$max_DOS_17OH <- as.Date(t_rx_111_17OH$max_DOS_17OH, origin = "1970-01-01")
t_rx_111_17OH$min_DOS_17OH <- as.Date(t_rx_111_17OH$min_DOS_17OH, origin = "1970-01-01")
t_rx_111<-sqldf("select MEMBER_ID, delivery_ID, max(SERVICE_DT) as max_DOS, DOB, sum(gross) as Totalgross_now, sum(OH_17_b) as num_OH_17_now 
                 from t_rx_11 group by delivery_ID")
t_rx_111$max_DOS <- as.Date(t_rx_111$max_DOS, origin = "1970-01-01")
Current_P_Full <- plyr::join(Current_P_Full, t_rx_111[, c('Totalgross_now','num_OH_17_now','delivery_ID')], type = 'left', by = 'delivery_ID') 
Current_P_Full <- Current_P_Full[!duplicated(Current_P_Full),]
Current_P_Full <- plyr::join(Current_P_Full, t_rx_111_17OH[, c('max_DOS_17OH','min_DOS_17OH','delivery_ID')], type = 'left', by = 'delivery_ID') 
Current_P_Full <- Current_P_Full[!duplicated(Current_P_Full),]
rm(t_rx,t_rx_1,t_rx_11,t_rx_111,t_rx_111_17OH)


# assign Provider IDs (most recent prenatal/outpt before delivery) to delivery cohort

# CHANGE month below
NIH_quarters_start <- seq.Date(as.Date('2020-02-01'), as.Date('2021-02-01'), "months")

Current_P_Full <- Current_P_Full[order(Current_P_Full$Delivery),] 
Current_P_Full_Final0 <- list()


# starting from 3/1/2021, could not load provider data correctly in PCCI DEV
# so run this for loop all the way until the end of the script in local machine
# save intermediate data needed
setwd("/home/michelle/T-Drive/PCHP PreTerm/Data")
save(Current_P_Full,Current_P_Full_Final0,NIH_quarters_start, file = "step10_5.rdata")
load("T:/PCHP PreTerm/Data/step10_5.rdata")


# run the for-loop to generate different provider names for deliveries in different months
# NOTE: THIS LOOP BREAKS FOR 2019 FEB AETNA FILES, SINCE THE DATE WAS 28TH, INSTEAD OF 14TH
# CHANGE i in the function into the number-1, THE IF STATEMENT WAS NO LONGER NEEDED SINCE 2020 FEB
for (i in 1:(length(NIH_quarters_start)-1)){
  
  # find/segment deliveries in month i 
  Current_P_Full_Final0[[i]]<-Current_P_Full[(Current_P_Full$Delivery>=NIH_quarters_start[i])&(Current_P_Full$Delivery<NIH_quarters_start[i+1]),]
  
  # load provider data in month i
  # if(i != 3){  # CHANGE HERE
  #   chip_prov <- paste("/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/CHIP_AIR_PROV_",year(NIH_quarters_start)[i+1],sprintf("%02d", month(NIH_quarters_start))[i+1],"14.txt",sep='')
  #   paid_prov <- paste("/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/PAID_AIR_PROV_",year(NIH_quarters_start)[i+1],sprintf("%02d", month(NIH_quarters_start))[i+1],"14.txt",sep='')
  # }else{
  #   chip_prov <- paste("/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/CHIP_AIR_PROV_",year(NIH_quarters_start)[i+1],sprintf("%02d", month(NIH_quarters_start))[i+1],"28.txt",sep='')
  #   paid_prov <- paste("/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/PAID_AIR_PROV_",year(NIH_quarters_start)[i+1],sprintf("%02d", month(NIH_quarters_start))[i+1],"28.txt",sep='')
  # }
  
  chip_prov <- paste("/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/CHIP_AIR_PROV_",year(NIH_quarters_start)[i+1],sprintf("%02d", month(NIH_quarters_start))[i+1],"14.txt",sep='')
  paid_prov <- paste("/home/michelle/T-Drive/PCHPAsthma/Data/ftp aetna/PAID_AIR_PROV_",year(NIH_quarters_start)[i+1],sprintf("%02d", month(NIH_quarters_start))[i+1],"14.txt",sep='')
  #chip_prov <- paste("T:/PCHPAsthma/Data/ftp aetna/CHIP_AIR_PROV_",year(NIH_quarters_start)[i+1],sprintf("%02d", month(NIH_quarters_start))[i+1],"14.txt",sep='')
  #paid_prov <- paste("T:/PCHPAsthma/Data/ftp aetna/PAID_AIR_PROV_",year(NIH_quarters_start)[i+1],sprintf("%02d", month(NIH_quarters_start))[i+1],"14.txt",sep='')
  
  pchp_prov_paid <- read.csv(paid_prov, header=T, quote="\"", sep='|', stringsAsFactors=FALSE)
  pchp_prov_chip <- read.csv(chip_prov, header=T, quote="\"", sep='|', stringsAsFactors=FALSE)
  #All the new chip ids are numeric.  Old alpha numerics fall out.
  pchp_prov_chip <- pchp_prov_chip[(!is.na(pchp_prov_chip$ProviderID))&!duplicated(pchp_prov_chip$ProviderID), ]
  names(pchp_prov_chip) <- gsub("\\.", '' , names(pchp_prov_chip))
  pchp_prov_paid <- pchp_prov_paid[(!is.na(pchp_prov_paid$ProviderI ))&!duplicated(pchp_prov_paid$ProviderID), ]
  names(pchp_prov_paid) <- gsub("\\.", '' , names(pchp_prov_paid))
  pchp_prov = rbind(pchp_prov_paid, pchp_prov_chip)
  rm(pchp_prov_paid, pchp_prov_chip)
  
  # process provider ID and name columns
  pchp_prov[,'ProviderID']<-str_trim(pchp_prov[, 'ProviderID'], side="both")
  pchp_prov<-pchp_prov[, c('ProviderID','FirstName','MI','LastName')]
  pchp_prov<-unique(pchp_prov)
  pchp_prov$FirstName<-gsub(" ","", pchp_prov$FirstName, fixed = TRUE)
  pchp_prov$LastName<-gsub(" ","", pchp_prov$LastName, fixed = TRUE)
  Name<-paste(pchp_prov$FirstName, pchp_prov$MI, pchp_prov$LastName, sep = ',')
  Name<-gsub(", ,", ",", Name, fixed = TRUE)
  Name<-gsub("^,", "", Name)
  pchp_prov$Attend_Name <- Name
  
  # most recent prenatal visit provider name
  pchp_prov$key_1<-pchp_prov$ProviderID
  Current_P_Full_Final0[[i]][,'Recent_pre_PROV_ID'] = str_trim(Current_P_Full_Final0[[i]][,'Recent_pre_PROV_ID'], side = "both")
  Current_P_Full_Final0[[i]]$key_1<-Current_P_Full_Final0[[i]]$Recent_pre_PROV_ID
  Current_P_Full_Final0[[i]]<- plyr::join(Current_P_Full_Final0[[i]], pchp_prov[ ,c('key_1', 'Attend_Name') ], type = 'left', by = 'key_1') 
  names(Current_P_Full_Final0[[i]])[dim(Current_P_Full_Final0[[i]])[2]]<-'Recent_pre_Attend_Name'
  
  # most recent outpatient visit provider name
  pchp_prov$key_1<-pchp_prov$ProviderID
  Current_P_Full_Final0[[i]][,'Recent_out_PROV_ID'] = str_trim(Current_P_Full_Final0[[i]][,'Recent_out_PROV_ID'], side = "both")
  Current_P_Full_Final0[[i]]$key_1<-Current_P_Full_Final0[[i]]$Recent_out_PROV_ID
  Current_P_Full_Final0[[i]]<- plyr::join(Current_P_Full_Final0[[i]], pchp_prov[ ,c('key_1', 'Attend_Name') ], type = 'left', by = 'key_1') 
  names(Current_P_Full_Final0[[i]])[dim(Current_P_Full_Final0[[i]])[2]]<-'Recent_out_Attend_Name'
  
  # generate final attending providers
  Current_P_Full_Final0[[i]]$Recent_AttendID<-Current_P_Full_Final0[[i]]$Recent_pre_PROV_ID
  Current_P_Full_Final0[[i]]$Recent_AttendName<-Current_P_Full_Final0[[i]]$Recent_pre_Attend_Name
  Current_P_Full_Final0[[i]]$Recent_AttendID[is.na(Current_P_Full_Final0[[i]]$Recent_AttendName)]<-Current_P_Full_Final0[[i]]$Recent_out_PROV_ID[is.na(Current_P_Full_Final0[[i]]$Recent_AttendName)]
  Current_P_Full_Final0[[i]]$Recent_AttendName[is.na(Current_P_Full_Final0[[i]]$Recent_AttendName)]<-Current_P_Full_Final0[[i]]$Recent_out_Attend_Name[is.na(Current_P_Full_Final0[[i]]$Recent_AttendName)]
  Current_P_Full_Final0[[i]]<-Current_P_Full_Final0[[i]][,!names(Current_P_Full_Final0[[i]])%in%c('key_1')]
  Current_P_Full_Final0[[i]]<-Current_P_Full_Final0[[i]][,!names(Current_P_Full_Final0[[i]])%in%c('MEMBER_ID.1')]
  
  print(paste("We just matched provider information for deliveries in the month starting from ", NIH_quarters_start[i], ".", sep = ""))
}

Current_P_Full_Final <- Current_P_Full_Final0[[1]]
for(i in 1:(length(NIH_quarters_start)-2)){  # equivalent to 1:11
  Current_P_Full_Final <- rbind(Current_P_Full_Final, Current_P_Full_Final0[[i+1]])
}
rm(i,chip_prov,paid_prov,Name,NIH_quarters_start,Current_P_Full,Current_P_Full_Final0,pchp_prov)


# ---------------------------------------------------------------------------------------------------- #
# OLD OBGYN ASSIGNMENT - EXCEPT PHHS COPC PROVIDER
#
# # thr
# thr_index <- grep("MEDICAL EDGE|TEXAS HEALTH (PRESBYTERIAN|HARRIS METHODIST|PRESBY|SURGERY CENTER DENTON|HUGULEY|WNJ|ARLINGTON|WOMENS HEALTH|PEDIATRIC|PHYSICIAN GROUP|CARE PLLC|HEART AND VASCULAR HOPS)", patient_l$OBGYN_Affil_Fullname)
# large_provider[thr_index] <- "THR"
# 
# # patient palce
# pat.place <- grep("PATIENT PLACE", patient_l$OBGYN_Affil_Fullname)
# large_provider[pat.place] <- "PATIENT PLACE"
# 
# # hoffman
# hoffman <- grep("HIGHWAY 30|PLEASANT GROVE| DENTON FAMILY HEALTH|FAMILY MEDICAL ASSOC OF NORTH DALLAS", patient_l$OBGYN_Affil_Fullname)
# large_provider[hoffman] <- "DR HOFFMAN"
# 
# # utsw
# utsw <- grep("UT (SOUTHWESTERN|SW)|UT SOUTHWESTERN MEDICAL CENTER", patient_l$OBGYN_Affil_Fullname)
# large_provider[utsw] <- "UTSW"
# added_UTSW<-paste(c('EVELINA,V,ALCALEN','FRANCIS,CUNNINGHAM','SHENA,J,DILLON','ELAINE,L,DURYEA','JOSIAH,Z,HAWKINS','BARBARA,L,HOFFMAN',
#                     'STEVEN,A,HOFFMAN','DEANA,HUSSAMY','ALVIN,T,HYSLOP','FARNAZ,JAHANGIRI','DEBRA,K,KNIPE','XERCERLA,A,LITTLES','JULIE,Y,LO',
#                     'ANN,LUTICH','JAMIE,L,MORGAN','ELYSIA,MOSCHOS','DAVID,B,NELSON','DAVIDMOWENSMD','SHIVANI,R,PATEL','SHELLEY,B,RAMOS',
#                     'SCOTT,W,ROBERTS','CONNIE,S,ROBERTS','VANESSA,ROGERS','DAVID,E,ROGERS','PATRICIA,SANTIAGO-MUNOZ','JENNIFER,L,STAUD','ROBERT,D,STEWART',
#                     'SUSAN,STORRY','TAM,C,TRUONG','PATRICK,M,WEIX','CHET,WELLS','CLAUDIA,L,WERNER','TIFFANY,N,WOODUS','SOUTHWESTERNWILLIAMPCLEMENTSJR'))
# added_UTSW_ind <- which(patient_l$OBGYN_Affil_Fullname%in%added_UTSW)
# #patient_l$OBGYN_Affil_Fullname[added_UTSW_ind]<-"UT Southwestern Medical Center"
# large_provider[added_UTSW_ind] <- "UTSW"
# 
# # BROCK,L,PIERCE
# PIERCE <- grep("BROCK,L,PIERCE|^LAWRENCESCOTTPIERCEMD|LAWRENCESCOTTPIERCEMD|BROCKLPIERCEMD|^BROCKLPIERCEMD", patient_l$OBGYN_Affil_Fullname)
# large_provider[PIERCE] <- "Pierce Group"
# 
# # mi doctor
# mi_doctor <- grep("MI DOCTOR|^PEDIATRICS OF DALLAS", patient_l$OBGYN_Affil_Fullname)
# large_provider[mi_doctor] <- "MI DOCTOR"
# 
# # pedcare
# ped_care <- grep("PEDCARE PA|PEDIATRIC CLINIC OF MESQUITE",patient_l$OBGYN_Affil_Fullname)
# large_provider[ped_care] <- "PEDCARE"
# 
# # PCA
# pca <- grep("PCA PRIMARY CARE ASSOCIATES|PCA GREENVILLE", patient_l$OBGYN_Affil_Fullname)
# large_provider[pca] <- "PCA"
# levels(factor(patient_l$OBGYN_Affil_Fullname[pca]))
# 
# # Childrens
# childrens <- grep("KIDS HEALTHCARE", patient_l$OBGYN_Affil_Fullname)
# large_provider[childrens] <- "KID'S HEALTHCARE (Charles Lewis)"
# 
# # My childrens
# mychild <- grep("PHYSICIANS FOR CHILDREN|CHILDRENS HEALTH PEDIATRIC GRP|CHILDRENS HEALTH GROUP|CHILDRENS HEALTH PEDIATRIC GROUP", patient_l$OBGYN_Affil_Fullname)
# large_provider[mychild] <- "CHILDRENS HEALTH PEDIATRIC GRP"
# 
# #Presbyterian Hospital#
# large_provider[which(large_provider=="TEXASHEALTHPRESBYTERIANHOSPITAL")]<-"PRESBYTERIAN HOSPITAL"
# large_provider[which(large_provider=="THE WOMENS HEALTH CENTER OF DALLAS PRESBYTERIAN")]<-"PRESBYTERIAN HOSPITAL"
# 
# #MacArthur Medical Cente
# MacArthur<-paste(c('ELIA,FANOUS','TERI,D,FORNEY','NEHAL,MASALAWALA','ADRIANA,Y,LOPEZ','ANDREA,ARGUELLO',
#                    'BRIAN,M,ENGGANO','JAMES,P,CARLEO','JEFF M LIVINGSTON MD','KEVIN,ONEIL',
#                    'KIMBERLY,SAKOVICH','MODENA,C,RUVALCABA','REBECCA,GRAY','RESHMA,N,PATEL','STEPHEN,P,SAKOVICH','WIYATTA,B,FREEMAN'))
# MacArthur_ind <- which(patient_l$OBGYN_Affil_Fullname%in% MacArthur)
# #patient_l$OBGYN_Affil_Fullname[MacArthur_ind]<-"MacArthur Medical Center"
# large_provider[MacArthur_ind] <- "MacArthur Medical Center"
# 
# #Advanced OB/Gyn Associates
# large_provider[which(large_provider=="DENISSE,HOLCOMB")]<-"Advanced OBGyn Associates"
# 
# #Advanced Women's Healthcare
# Adv<-paste(c('HEIDI,HARMS','MICHELLE,L,HEINTGES','AMIE,NAPIER','AMY,G,SIGMAN'))
# Adv_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Adv)
# #patient_l$OBGYN_Affil_Fullname[Adv_ind ]<-"Advanced Women's Healthcare"
# large_provider[Adv_ind] <- "Advanced Womens Healthcare"
# 
# #Carlos & Parnell, MD, PA
# CP<-paste(c('WINFRED,PARNELL','JOSEPH,J,CARLOS','MONICA,M,DIAZ','RACHEL,C,GUNDERSON','WENDY,C,PARNELL'))
# CP_ind <- which(patient_l$OBGYN_Affil_Fullname%in%CP)
# #patient_l$OBGYN_Affil_Fullname[CP_ind]<-"Carlos & Parnell, MD, PA"
# large_provider[CP_ind] <- "Carlos AND Parnell,MD,PA"
# 
# #Bernard Adami, MD P.A
# large_provider[which(large_provider=="BERNARDFADAMIMD")]<-"Bernard Adami,MD P.A."
# 
# #Comprehensive OB/GYN
# Com<-paste(c('RENEE,L,CHAN','GERALD,D,LUCIANI','CESAR,A,REYES','MARIA,T,REYES','SEAN,A,SADLER'))
# Com_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Com)
# #patient_l$OBGYN_Affil_Fullname[Com_ind]<-"Comprehensive OB/GYN"
# large_provider[Com_ind] <- "Comprehensive OBGYN"
# 
# #Contemporary Womens Care
# Con<-paste(c('SHAHEEN,F,JACOB','NEDRA,L,RICE'))
# Con_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Con)
# #patient_l$OBGYN_Affil_Fullname[Con_ind]<-"Contemporary Women??s Care"
# large_provider[Con_ind] <- "Contemporary Womens Care"
# 
# #Craig Ranch OB/Gyn
# Craig<-paste(c('HEATHER,S,DERRICK','ROBIN,EVENSON','SHEA,JOYNER','BARBARA,LARSEN','SARA,ROBERT','ANDREW,T,SHIMER'))
# Craig_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Craig)
# #patient_l$OBGYN_Affil_Fullname[Craig_ind]<-"Craig Ranch OB/Gyn"
# large_provider[Craig_ind] <- "Craig Ranch OBGyn"
# 
# #Dr. Deleon Women's Healthcare Clinic
# Deleon<-paste(c('ISAMARIE,L,ALCANTARA','GABRIELA,M,ZANDOMENI','JOSE F DE LEON MD PA'))
# Deleon_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Deleon)
# #patient_l$OBGYN_Affil_Fullname[Deleon_ind]<-"Dr. Deleon Women's Healthcare Clinic"
# large_provider[Deleon_ind] <- "Dr Deleon Womens Healthcare Clinic"
# large_provider[which(large_provider%in%c("DR DELEONS WOMANS HEALTH CARE"))]<-"Dr Deleon Womens Healthcare Clinic"
# 
# #F. Thomas Dean, MD
# large_provider[which(large_provider=="FTHOMASDEANMD")]<-"F.Thomas Dean,MD"
# 
# #Fetal Care Consultants
# large_provider[which(large_provider=="ZACHARY,S,RUBEO")]<-"Fetal Care Consultants"
# 
# #Flower Mound Women's Care
# Flo<-paste(c('ELIZABETH,L,DICKENS','SALY,THOMAS'))
# Flo_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Flo)
# #patient_l$OBGYN_Affil_Fullname[Flo_ind]<-"Flower Mound Women's Care"
# large_provider[Flo_ind] <- "Flower Mound Womens Care"
# 
# #Grace Women's Health Center
# large_provider[which(large_provider=="ABRAHAM, MINIJA A")]<-"Grace Womens Health Center"
# 
# #Grand Prairie Women's Health Center
# patient_l$OBGYN_Affil_Fullname[patient_l$OBGYN_Affil_Fullname=='CATHERINE,E,MCCAFFITY']<-"Grand Prairie Women's Health Center"
# large_provider[which(large_provider=="CATHERINE,E,MCCAFFITY")]<-"ParklandOB"
# 
# #Health Central, P.A.
# Hea<-paste(c('ARMINDA,CALLEJAS-WEINTZ','TRACY,H,ELLIOTT','ALAN,GREENBERG','CLARK,W,GRIFFITH','ELISE,HARPER','NATALIE,C,LIGHT','A,J,STAUB',
#              'FELICIA,M,TILLMAN','ANN,R,WOODBRIDGE'))
# Hea_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Hea)
# #patient_l$OBGYN_Affil_Fullname[Hea_ind]<-"Health Central, P.A."
# large_provider[Hea_ind] <- "Health Central, PA"
# 
# #Lake Pointe Medical Partners
# large_provider[which(large_provider=="MONIKA,HEARNE")]<-"Fetal Care Consultants"
# 
# #Lake Pointe Women's Centre
# Lake<-paste(c('JEFFERY,NELSON','THOMAS,S,SUDELA'))
# Lake_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Lake)
# #patient_l$OBGYN_Affil_Fullname[Lake_ind]<-"Lake Pointe Women's Centre"
# large_provider[Lake_ind] <- "Lake Pointe Womens Centre"
# 
# #Las Colinas OB/GYN
# large_provider[which(large_provider=="GONZALO,H,GARCIA")]<-"Las Colinas OBGYN"
# 
# #Matlock Obstetrics & Gynecology Associates, PA
# Matlock<-paste(c('JANIS,R,CORNWELL','TAMRA,FORTENBERRY','HARRY,P,FROESCHKE','ROBERT,A,GREVE'))
# Matlock_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Matlock)
# #patient_l$OBGYN_Affil_Fullname[Matlock_ind]<-"Matlock Obstetrics & Gynecology Associates, PA"
# large_provider[Matlock_ind] <- "Matlock Obstetrics and Gynecology Associates, PA"
# 
# #Methodist ObGyn
# Methodist<-paste(c('JORGE ARTURO ARZAC M.D','ROBERT,T,GUNBY','PATRICIA,N,HARRIS'))
# Methodist_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Methodist)
# #patient_l$OBGYN_Affil_Fullname[Methodist_ind]<-"Methodist ObGyn"
# large_provider[Methodist_ind] <- "Methodist ObGyn"
# 
# #OB Hospitalists of Texas
# OB<-paste(c('SHARON,BAKOS','ADRIANNE,C,BROWNING','PAUL,B,PAYNE','LESLIEHAVEMANNMD'))
# OB_ind <- which(patient_l$OBGYN_Affil_Fullname%in%OB)
# #patient_l$OBGYN_Affil_Fullname[OB_ind]<-"OB Hospitalists of Texas"
# large_provider[OB_ind] <- "OB Hospitalists of Texas"
# 
# #OBHG Texas Holdings, PC
# OBHG<-paste(c('THERESA,CONYAC','CHARLES,R,DOWNEY','MARIE,E,HOLLIS'))
# OBHG_ind <- which(patient_l$OBGYN_Affil_Fullname%in%OBHG)
# #patient_l$OBGYN_Affil_Fullname[OBHG_ind]<-"OBHG Texas Holdings, PC"
# large_provider[OBHG_ind] <- "OBHG Texas Holdings, PC"
# 
# #Obstetrics & Gynecology Associates of Dallas
# OBs<-paste(c('LINDEN,COLLINS','GLEN,W,HECKMAN'))
# OBs_ind <- which(patient_l$OBGYN_Affil_Fullname%in%OBs)
# #patient_l$OBGYN_Affil_Fullname[OBs_ind]<-"Obstetrics & Gynecology Associates of Dallas"
# large_provider[OBs_ind] <- "Obstetrics and Gynecology Associates of Dallas"
# 
# ##Omega OB/GYN Associates
# Omega<-paste(c('CHARMAINE,OLADELL','STEPHANIE,PICKEL','KEVIN,R,GORDON','MINDY,B,LUCK','JEFFREY,L,MORGAN','JOHN,A,PICKELJR','STEVEN,J,SELIGMAN','KEITH,R,STORTS'))
# Omega_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Omega)
# #patient_l$OBGYN_Affil_Fullname[Omega_ind]<-"Omega OB/GYN Associates"
# large_provider[Omega_ind] <- "Omega OBGYN Associates"
# 
# ##Parkland Health & Hospital System
# Park<-paste(c('SARAH,B,JENSEN','LINDSAY,D,MOUSER','SHAYLA,SIMPSON','KAREN,M,ZELMAN'))
# Park_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Park)
# patient_l$OBGYN_Affil_Fullname[Park_ind]<-"Parkland Health & Hospital System"
# large_provider[Park_ind] <- "ParklandOB"
# 
# ##Premier OB/GYN Center
# Premier<-paste(c('ANU,F,OGUNLARI','DR.MICHAELC.YANGM.D.'))
# Premier_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Premier)
# #patient_l$OBGYN_Affil_Fullname[Premier_ind]<-"Premier OB/GYN Center"
# large_provider[Premier_ind] <- "Premier OBGYN Center"
# 
# ##Questcare Obstetrics
# Questcare<-paste(c('REBECCA,L,CRAWFORD','CHARLES,E,WHITE'))
# Questcare_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Questcare)
# #patient_l$OBGYN_Affil_Fullname[Questcare_ind]<-"Questcare Obstetrics"
# large_provider[Questcare_ind] <- "Questcare Obstetrics"
# 
# ##Southeast Dallas Women's Health Center
# patient_l$OBGYN_Affil_Fullname[patient_l$OBGYN_Affil_Fullname%in%c('MELISSA,A,JONES')]<-"Southeast Dallas Women's Health Center"
# large_provider[which(large_provider%in%c("MELISSA,A,JONES"))]<-"ParklandOB"
# 
# #Texas Regional Womens Health Center
# large_provider[which(large_provider=="SAMI,E,CONSTANTINE")]<-"Texas Regional Womens Health Center"
# 
# ##Trinity Women's Center
# Trinity<-paste(c('DONALD,BLAIR','JOY,M,SMITH'))
# Trinity_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Trinity)
# #patient_l$OBGYN_Affil_Fullname[Trinity_ind]<-"Trinity Women's Center"
# large_provider[Trinity_ind] <- "Trinity Womens Center"
# 
# #Waxahachie Women's Health
# Wax<-paste(c('JASON,P,BROWN','ROBIN,P,KINDRED','DAVID,B,MOREHEAD','KRISTIN,N,WILLIAMS'))
# Wax_ind <- which(patient_l$OBGYN_Affil_Fullname%in%Wax)
# #patient_l$OBGYN_Affil_Fullname[Wax_ind]<-"Waxahachie Women's Health"
# large_provider[Wax_ind] <- "Waxahachie Womens Health"
# 
# ###White Rock OB/GYN
# White<-paste(c('JAVIER,GARCIARUIZDESOMOCURCIO','FRANCESCA,PERUGINI'))
# White_ind <- which(patient_l$OBGYN_Affil_Fullname%in%White)
# #patient_l$OBGYN_Affil_Fullname[White_ind]<-"White Rock OB/GYN"
# large_provider[White_ind] <- "White Rock OBGYN"
# 
# ##WOMENS SPECIALTY CENTER
# WOMENS<-paste(c('ELIZABETH,A,RAMIREZ','ZACHARY,RIPP'))
# WOMENS_ind <- which(patient_l$OBGYN_Affil_Fullname%in%WOMENS)
# #patient_l$OBGYN_Affil_Fullname[WOMENS_ind]<-"WOMENS SPECIALTY CENTER"
# large_provider[WOMENS_ind] <- "WOMENS SPECIALTY CENTER"
# 
# ##Women's Wellness Center
# large_provider[which(large_provider=="TERESA,D,KOWALCZYK")]<-"Womens Wellness Center"
# 
# #HEALTHTEXASPROVIDERNETWORK
# healthtexas <- grep("HEALTH TEXAS PROVIDER NETWORK  DBA PCA OF GUN BARREL CITY|^HEALTHTEXAS PROVIDER NETWORK|HEALTHTEXAS PROVIDER NETWORK|HEALTHTEXASPROVIDERNETWORK|HEALTHTEXASPROVIDERNETWORKDBA", patient_l$OBGYN_Affil_Fullname)
# large_provider[healthtexas] <- "HEALTH TEXAS PROVIDER NETWORK"
# 
# #METHODIST
# healthtexas <- grep("METHODISTCHARLTONMEDICALCTR|METHODISTHOSPITALSOFDALLASDBA|METHODISTRICHARDSONMEDICALCENTER", patient_l$OBGYN_Affil_Fullname)
# large_provider[healthtexas] <- "Methodist ObGyn"
# 
# #OB Hospitalists of Texas
# large_provider[which(large_provider=="OB HOSPITALISTS OF TEXAS PA")]<-"OB Hospitalists of Texas"
# large_provider[which(large_provider=="OBHG Texas Holdings, PC")]<-"OB Hospitalists of Texas"
# 
# # NA
# allmiss<-which(((large_provider=="NA")|(is.na(large_provider)))&(is.na(patient_l$Recent_PCPID)))
# large_provider[allmiss]<-"Missing PCP"
# 
# # NA OBGYN
# naobgyn<-which(((large_provider=="NA")|(is.na(large_provider)))&(!is.na(patient_l$Recent_PCPID)))
# large_provider[naobgyn]<-"Missing OBGYN And Have PCP"
# 
# patient_l <- cbind(patient_l, large_provider)   # append the large_provider information to each patient
# --------------------------------------------------------------------------------------------------------- #

# old long assignment process is the same as process in step 10
# Xiao combined as one function, May 26, 2020
# function is defined in step 10
patient_l <- assign_providers(Current_P_Full_Final, step = 10.5)

# Warning message:
# In stri_trim_both(string) : argument is not an atomic vector; coercing


## comparison result from function assign_providers() VS result step by step in May 26, 2020
## completely the same
#library(arsenal)
#comparedf(patient_l1, patient_l2)


patient_l$large_provider<-as.character(patient_l$large_provider)
patient_l$Y<-as.numeric(patient_l$ext_premature==1|patient_l$very_premature==1|patient_l$moder_premature==1)
patient_l$days<-as.numeric(patient_l$Delivery-patient_l$first_elg_start)
patient_l$days[is.na(patient_l$days)]<-0
patient_l$days_now<-as.numeric(patient_l$Delivery-patient_l$first_elg_start_now)
patient_l$days_now[is.na(patient_l$days_now)]<-0


# Xiao added in Dec 15, 2019
# Dr Rajala is missing from provider table
# so his preterm rate in the last 12 months is dropping eventually to 0, if no correction here
patient_l$OBGYN_Fullname <- ifelse(patient_l$OBGYN_ID == "PROV0000P03522" & is.na(patient_l$OBGYN_Fullname), "BRUCE,RAJALA", patient_l$OBGYN_Fullname)
patient_l$OBGYN_Affil_Fullname <- ifelse(patient_l$OBGYN_ID == "PROV0000P03522" & is.na(patient_l$OBGYN_Affil_Fullname), "BRUCE,RAJALA", patient_l$OBGYN_Affil_Fullname)
patient_l$large_provider <- ifelse(patient_l$OBGYN_ID == "PROV0000P03522" & patient_l$large_provider == "Missing OBGYN And Have PCP", "BRUCE,RAJALA", patient_l$large_provider)

# Xiao added in 3/28/2021, need to incoporated into assign_provider() function
# large provider == "WOMENS SPECIALTY CENTER" has no deliveries
# ALFRED,G,ANTONETTI
# VENEGAS, GONZALO
patient_l$large_provider <- ifelse(patient_l$OBGYN_ID %in% c("PROV0000P19823","PROV0000P03903"), "WOMENS SPECIALTY CENTER", patient_l$large_provider)  # from 0 to 83


Final_n <- patient_l %>% group_by(large_provider) %>% 
  dplyr::summarise(n = n(), n_preterm = sum(Y), Group_pretermrate = n_preterm/n, mean_day = mean(days, na.rm = T), mean_day_now = mean(days_now, na.rm = T)) %>% arrange(desc(n))
# the following 3 PCHP reference values are constant for different large providers
Final_n$PCHP_pretermrate<-sum(patient_l$Y)/dim(patient_l)[1]
Final_n$pchp_mean_day<-mean(patient_l$days)
Final_n$pchp_mean_day_now<-mean(patient_l$days_now)

# CHANGE month below
write.csv(Final_n, '/home/michelle/T-Drive/PCHP PreTerm/Data/PretermRate1years_March_2021.csv')
save(patient_l, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/March2021_1_year_Preterm.RData")
#write.csv(Final_n, 'T:/PCHP PreTerm/Data/PretermRate1years_February_2021.csv')
#save(patient_l, file = "T:/PCHP PreTerm/Data/February2021_1_year_Preterm.RData")

rm(Final_n,patient_l,Current_P_Full_Final,assign_providers)
# rm(Flo,Flo_ind,floater,garland,grand.prairie,green,Hea,Hea_ind,hoffman,homes,ID,index,int_med,irving,Lake,Lake_ind,MacArthur,MacArthur_ind,Matlock,Matlock_ind,
#    Methodist,Methodist_ind,mi_doctor,mychild,naobgyn,no_peds,oak.west,OB,OB_ind,OBHG,OBHG_ind,OBs,OBs_ind,Omega,Omega_ind,Park,Park_ind,pat.place,pca,ped_care,
#    ppcc,Premier,Premier_ind,Questcare,Questcare_ind,red,southeast.dallas,thr_index,Trinity,Trinity_ind,utsw,vickery,Wax,Wax_ind,White,White_ind,wom_hea,WOMENS,
#    WOMENS_ind,youth.fam.center,added_UTSW,added_UTSW_ind,Adv,Adv_ind,allmiss,childrens,Com,Com_ind,Con,Con_ind,copc,copc_ind,copc_list,copcs,copcs2,CP,CP_ind,
#    Craig,Craig_ind,deharo,Deleon,Deleon_ind,east_dallas,epo,fam_med,healthtexas,PIERCE)


#### PCHP Preterm Project Step 11 (final step) ####
# Apply model and assign risk scores to pregnant cohort

# PREPARATION - create an empty folder named MONTHYEAR in the Production folder
# copy and paste all the resulting files generated from step 1 to 10.5 into the empty folder 
# in total 22 files moved: 18 RData + 2 csv + 2 txt

# CHANGE month below
load("/home/michelle/T-Drive/PCHP PreTerm/Data/Production/March2021/PCHP_March_2021_Raw.RData")
#load("T:/PCHP PreTerm/Data/Production/March2021/PCHP_March_2021_Raw.RData")

# exclude threatened abortion
# CHENGE index
Final <- Final_42

Final$recent<-as.Date(0)
ID1<-(is.na(Final$recent_Previsit))&(!is.na(Final$recent_outpt))  # no recent prenatal, with recent outpt
Final$recent[ID1]<-Final$recent_outpt[ID1]
ID2<-(!is.na(Final$recent_Previsit))&(is.na(Final$recent_outpt))  # with recent prenatal, no recent outpt
Final$recent[ID2]<-Final$recent_Previsit[ID2]
ID3<-(!is.na(Final$recent_Previsit))&(!is.na(Final$recent_outpt))  # with recent prenatal, with recent outpt
for (i in 1:length(Final$recent[ID3])){
  Final$recent[ID3][i]<-max(Final$recent_outpt[ID3][i],Final$recent_Previsit[ID3][i])  # max means more recent date
}
ID4<-(is.na(Final$recent_Previsit))&(is.na(Final$recent_outpt))   # no recent prenatal, no recent outpt
Final$recent[ID4]<-NA
Final_1<-Final[(!is.na(Final$abortion_now))&(Final$abortion_now==1), ]  
Final_1<-Final_1[(as.numeric(Final_1$Anchor-Final_1$recent)<=91)|is.na(as.numeric(Final_1$Anchor-Final_1$recent)<=91), ]  
Final_2<-Final[is.na(Final$abortion_now)|(Final$abortion_now!=1), ]  
Final_2<-Final_2[(as.numeric(Final_2$Anchor-Final_2$recent)<=270)|is.na(as.numeric(Final_2$Anchor-Final_2$recent)<=270), ]  
Final<-rbind(Final_1,Final_2)

# CHENGE index
Final_42 <- Final  
nrow(Final_42)
# 5736,5801,5987,5947,6187,6315,6201,6113,6025,6111,5605,5352,6069,
rm(ID1,ID2,ID3,ID4,i,Final_1,Final_2,Final)


# function to update large providers and apply model
adjust_pchp <- function(pchp_lookup_final){
  
  pchp_lookup_final$large_provider <- as.character(pchp_lookup_final$large_provider)
  
  # add amerigroup stuff
  pchp_lookup_final$large_provider[grep('ESTRADA PEDIATRIC CENTER|CENTRO DE PEDIATRIC HISPANA|ESTRADA PEDIATRIC CLINIC LLC DBA VILLAGE PEDIATRIC CLINIC', pchp_lookup_final$large_provider)] <- 'ESTRADA, ROBERTO E'
  pchp_lookup_final$large_provider[grep('SOUTH POLK MEDICAL CLINIC', pchp_lookup_final$large_provider)] <- 'ROBERTS, GWENITA D'
  #pchp_lookup_final$large_provider[grep('WILLIS L STARNES MD', pchp_lookup_final$large_provider)] <- 'WILLIS L STARNES MD'
  pchp_lookup_final$large_provider[grep('RAINBOW CHILDREN', pchp_lookup_final$large_provider)] <- 'BERNARDEZ-TAN, RUTH A '
  # Previously Rainbow Childrens Clinic - issue is that childrens now has apostrophe children's
  pchp_lookup_final$large_provider[grep("HEENA N KANASE DO|OAK CLIFF CHILDRENS|DR. HEENA NITIN KANASE D.O.", pchp_lookup_final$large_provider)] <- 'HEENA N KANASE DO'
  
  pchp_lookup_final$OBGYN_Affil_Fullname[grep("SOUTHEAST DALLAS HEALTH|SOUTHEAST DALLAS HEALTH CENTER", pchp_lookup_final$OBGYN_Affil_Fullname)] <- 'SOUTHEAST DALLAS HEALTH CENTER'
  
  # additional COPC providers
  pchp_lookup_final$large_provider[grep("JOSE SALGUERO MD PA", pchp_lookup_final$OBGYN_Fullname)] <- 'JOSE SALGUERO'
  pchp_lookup_final$large_provider[grep("JOSE SALGUERO", pchp_lookup_final$large_provider)] <- 'JOSE SALGUERO'
  pchp_lookup_final$large_provider[grep('JOSELITA RODRIGUEZ|RODRIGUEZ, JOSELITA', pchp_lookup_final$OBGYN_Fullname)] <- 'JOSELITA RODRIGUEZ'
  pchp_lookup_final$large_provider[grep('REBECA PIANTINI|REBECA PIANTINI-ALVAREZ|REBECA ALVAREZ|PIANTINI-ALVAREZ',pchp_lookup_final$OBGYN_Fullname)] <- 'REBECA PIANTINI-ALVAREZ'
  pchp_lookup_final$large_provider[which(pchp_lookup_final$large_provider=='BROCK,L,PIERCE')] <- 'Pierce Group'
  pchp_lookup_final$large_provider[which(pchp_lookup_final$large_provider=='BLAKEEFRIEDENMD')] <- 'BLAKE,E,FRIEDEN'
  
  # combined preterm history
  pchp_lookup_final$Prehist1[is.na(pchp_lookup_final$Prehist1)]<-0
  pchp_lookup_final$Prehist11[is.na(pchp_lookup_final$Prehist11)]<-0
  pchp_lookup_final$Prehist2[is.na(pchp_lookup_final$Prehist2)]<-0
  
  pchp_lookup_final$Prehist_comb<-as.numeric(pchp_lookup_final$Prehist==1|pchp_lookup_final$Prehist1==1|pchp_lookup_final$Prehist11==1|pchp_lookup_final$Prehist2==1)
  pchp_lookup_final$Prehist_D<-as.numeric(pchp_lookup_final$Prehist==1|pchp_lookup_final$Prehist11==1)
  
  # combined mental and nutrition varibles
  pchp_lookup_final$mental<-as.numeric(pchp_lookup_final$mood==1|pchp_lookup_final$anxiety==1|pchp_lookup_final$depression==1|pchp_lookup_final$psy==1)
  pchp_lookup_final$nutrition<-as.numeric(pchp_lookup_final$obe_over==1|pchp_lookup_final$malnutrition==1)
  #pchp_lookup_final$gestation<-as.numeric(as.Date(pchp_lookup_final$Delivery)-pchp_lookup_final$Anchor)
  pchp_lookup_final$drug<-as.numeric(pchp_lookup_final$alcohol==1|pchp_lookup_final$substance==1)
  
  # hospitalization - num_inpt, num_ED, num_outpt, num_pre     
  # numbers of inpatient visit
  pchp_lookup_final$num_inpt_T<-pchp_lookup_final$num_inpt
  pchp_lookup_final$num_inpt_T[is.na(pchp_lookup_final$num_inpt)]<-0
  # numbers of outpatient visit
  pchp_lookup_final$num_outpt_T<-pchp_lookup_final$num_outpt
  pchp_lookup_final$num_outpt_T[is.na(pchp_lookup_final$num_outpt)]<-0
  # numbers of ED visit
  pchp_lookup_final$num_ED_T<-pchp_lookup_final$num_ED
  pchp_lookup_final$num_ED_T[is.na(pchp_lookup_final$num_ED)]<-0
  # numbers of prenatal visit
  pchp_lookup_final$num_pre_T<-pchp_lookup_final$num_pre
  pchp_lookup_final$num_pre_T[is.na(pchp_lookup_final$num_pre)]<-0
  # numbers of planned (outpt+pre) visit
  pchp_lookup_final$num_out_pre_T<-pchp_lookup_final$num_out_pre
  pchp_lookup_final$num_out_pre_T[is.na(pchp_lookup_final$num_out_pre)]<-0
  # numbers of unplanned (ED/inpt) visit
  pchp_lookup_final$num_ED_inpt_T<-pchp_lookup_final$num_ED_inpt
  pchp_lookup_final$num_ED_inpt_T[is.na(pchp_lookup_final$num_ED_inpt)]<-0
  
  #----------------------------------------------------------------------------------#
  # monthly mean outpatient/prenatal visit number
  # the end date is the prediction time (Anchor);
  # the start date is calculated by:
  # 1. for riskgroup ID = 5,20,309 and 310, use effective date as starting date;
  # 2. for riskgroup is NA, use first_HEDISDate as starting date;
  #----------------------------------------------------------------------------------#
  pchp_lookup_final$monthstart<-as.Date("2017-01-01")   # initialization
  pchp_lookup_final$monthstart[which(is.na(pchp_lookup_final$RiskGroupID))]<-pchp_lookup_final$first_HEDISDate[which(is.na(pchp_lookup_final$RiskGroupID))]
  pchp_lookup_final$monthstart[which(!is.na(pchp_lookup_final$RiskGroupID))]<-pchp_lookup_final$EffectiveDate[which(!is.na(pchp_lookup_final$RiskGroupID))]
  pchp_lookup_final$Month<-ceiling(as.numeric(pchp_lookup_final$Anchor-pchp_lookup_final$monthstart)/30.5)
  pchp_lookup_final$Month[pchp_lookup_final$Month==0]<-1      # 0 month (<30 days) counts as 1 month
  pchp_lookup_final$num_out_pre_month_T<-round((pchp_lookup_final$num_out_pre_T)/pchp_lookup_final$Month, digits = 2)
  pchp_lookup_final$num_ED_inpt_month_T<-round((pchp_lookup_final$num_ED_inpt_T)/pchp_lookup_final$Month, digits = 2)
  
  # 17OH use - num_OH_17, num_OH_17_now
  pchp_lookup_final$num_OH_17_now_T<-pchp_lookup_final$num_OH_17_now
  pchp_lookup_final$num_OH_17_now_T[is.na(pchp_lookup_final$num_OH_17_now)]<-0
  pchp_lookup_final$OH_17_now<-0
  pchp_lookup_final$OH_17_now[pchp_lookup_final$num_OH_17_now>0]<-1
  
  # insurance gap information                                     
  # last_gap_days, gap_days_correct, n_gaps_correct, time_first_elg 
  # because xiao changed codes in step 8, var meanings might be different! 
  # in the final model, the only feature selected from insurance is called gap_days_correct_T_s, computed from gap_days_correct, no need to change!
  pchp_lookup_final$gap_days_correct_T<-pchp_lookup_final$gap_days_correct
  pchp_lookup_final$gap_days_correct_T[is.na(pchp_lookup_final$gap_days_correct)]<-365
  # time length between anchor and starting date of most recent insurance
  pchp_lookup_final$last_gap_days_T<-pchp_lookup_final$last_gap_days
  pchp_lookup_final$last_gap_days_T[is.na(pchp_lookup_final$last_gap_days)]<-0
  # number of gaps, the missings are assigned the maximum? Q: assigned 0?
  pchp_lookup_final$n_gaps_correct_T<-pchp_lookup_final$n_gaps_correct
  pchp_lookup_final$n_gaps_correct_T[is.na(pchp_lookup_final$n_gaps_correct)]<-0
  pchp_lookup_final$last_gap_days_T_S<-pchp_lookup_final$last_gap_days_T/365
  pchp_lookup_final$gap_days_correct_T_S<-pchp_lookup_final$gap_days_correct_T/365
  
  # time-related variables
  # time_recentoutpt, time_recentprenatal, time_firstprenatal, time_firstoutpt, first_outpt_prenatal
  pchp_lookup_final$time_firstprenatal<-as.numeric(as.Date(pchp_lookup_final$Anchor)-as.Date(pchp_lookup_final$first_Previsit))
  pchp_lookup_final$time_firstoutpt<-as.numeric(as.Date(pchp_lookup_final$Anchor)-as.Date(pchp_lookup_final$first_outpt))
  pchp_lookup_final$time_recentprenatal<-as.numeric(as.Date(pchp_lookup_final$Anchor)-as.Date(pchp_lookup_final$recent_Previsit))
  pchp_lookup_final$time_recentoutpt<-as.numeric(as.Date(pchp_lookup_final$Anchor)-as.Date(pchp_lookup_final$recent_outpt))
  # time length between first prenatal/most recent visit and anchor date
  pchp_lookup_final$time_firstprenatal_T<-pchp_lookup_final$time_firstprenatal
  pchp_lookup_final$time_firstprenatal_T[is.na(pchp_lookup_final$time_firstprenatal)]<-0
  pchp_lookup_final$time_recentprenatal_T<-pchp_lookup_final$time_recentprenatal
  pchp_lookup_final$time_recentprenatal_T[is.na(pchp_lookup_final$time_recentprenatal)]<-0
  # time length between first outpatient visit and anchor date
  pchp_lookup_final$time_firstoutpt_T<-pchp_lookup_final$time_firstoutpt
  pchp_lookup_final$time_firstoutpt_T[is.na(pchp_lookup_final$time_firstoutpt)]<-0
  pchp_lookup_final$time_recentoutpt_T<-pchp_lookup_final$time_recentoutpt
  pchp_lookup_final$time_recentoutpt_T[is.na(pchp_lookup_final$time_recentoutpt)]<-0
  
  # social status - low_mean, low_median    
  pchp_lookup_final$Mean_Ratio_T<-pchp_lookup_final$Mean_Ratio
  pchp_lookup_final$Mean_Ratio_T[is.na(pchp_lookup_final$Mean_Ratio)]<-0
  
  # current variables: ges_diabetes_now, pi_hypertension_now,          
  # UTI_now, Cyst_now, Uret_now, asympt_now, Bact_now, abortion_now, cervix_now, Cerc_now      
  pchp_lookup_final$ges_diabetes_now_T<-0
  pchp_lookup_final$ges_diabetes_now_T[pchp_lookup_final$ges_diabetes_now==1]<-1
  pchp_lookup_final$pi_hypertension_now_T<-0
  pchp_lookup_final$pi_hypertension_now_T[pchp_lookup_final$pi_hypertension_now==1]<-1
  pchp_lookup_final$Cerc_now_T<-0
  pchp_lookup_final$Cerc_now_T[pchp_lookup_final$Cerc_now==1]<-1
  pchp_lookup_final$cervix_now_T<-0
  pchp_lookup_final$cervix_now_T[pchp_lookup_final$cervix_now==1]<-1
  pchp_lookup_final$abortion_now_T<-0
  pchp_lookup_final$abortion_now_T[pchp_lookup_final$abortion_now==1]<-1
  pchp_lookup_final$Bact_now_T<-0
  pchp_lookup_final$Bact_now_T[pchp_lookup_final$Bact_now==1]<-1
  pchp_lookup_final$asympt_now_T<-0
  pchp_lookup_final$asympt_now_T[pchp_lookup_final$asympt_now==1]<-1
  pchp_lookup_final$Uret_now_T<-0
  pchp_lookup_final$Uret_now_T[pchp_lookup_final$Uret_now==1]<-1
  pchp_lookup_final$Cyst_now_T<-0
  pchp_lookup_final$Cyst_now_T[pchp_lookup_final$Cyst_now==1]<-1
  pchp_lookup_final$UTI_now_T<-0
  pchp_lookup_final$UTI_now_T[pchp_lookup_final$UTI_now==1]<-1
  
  # obstetric history: ho_hypertension, ho_diabetes, ges_diabetes, pi_hypertension, cervix, Cerc, HIV, Abor_his, sex_trans
  pchp_lookup_final$ho_hypertension_T<-0
  pchp_lookup_final$ho_hypertension_T[pchp_lookup_final$ho_hypertension==1]<-1
  pchp_lookup_final$ho_diabetes_T<-0
  pchp_lookup_final$ho_diabetes_T[pchp_lookup_final$ho_diabetes==1]<-1
  pchp_lookup_final$pi_hypertension_T<-0
  pchp_lookup_final$pi_hypertension_T[pchp_lookup_final$pi_hypertension==1]<-1
  pchp_lookup_final$ges_diabetes_T<-0
  pchp_lookup_final$ges_diabetes_T[pchp_lookup_final$ges_diabetes==1]<-1
  pchp_lookup_final$HIV_T<-0
  pchp_lookup_final$HIV_T[pchp_lookup_final$HIV==1]<-1
  pchp_lookup_final$sex_trans_T<-1
  pchp_lookup_final$sex_trans_T[pchp_lookup_final$sex_trans==0]<-0
  pchp_lookup_final$Abor_his_T<-0
  pchp_lookup_final$Abor_his_T[pchp_lookup_final$Abor_his==1]<-1
  pchp_lookup_final$Cerc_T<-0
  pchp_lookup_final$Cerc_T[pchp_lookup_final$Cerc==1]<-1
  pchp_lookup_final$cervix_T<-0
  pchp_lookup_final$cervix_T[pchp_lookup_final$cervix==1]<-1
  
  # missing value indicators
  # 17OH medication usage, current pregnancy time window, missing indicator
  pchp_lookup_final$OH_17_now_NA<-0
  pchp_lookup_final$OH_17_now_NA[is.na(pchp_lookup_final$num_OH_17_now)]<-1
  # obstetric missing indicator
  pchp_lookup_final$Obstetric_NA<-0
  pchp_lookup_final$Obstetric_NA[is.na(pchp_lookup_final$cervix)]<-1
  # preterm history info, missing indicator
  pchp_lookup_final$prehistory_NA<-0
  pchp_lookup_final$prehistory_NA[is.na(pchp_lookup_final$uterine)]<-1
  # pregnancy missing indicator
  pchp_lookup_final$Preg_NA<-0
  pchp_lookup_final$Preg_NA[is.na(pchp_lookup_final$num_ED)]<-1
  # insurance gap info, missing indicator
  pchp_lookup_final$gap_NA<-0
  pchp_lookup_final$gap_NA[is.na(pchp_lookup_final$gap_days_correct)]<-1
  
  # others
  # whether already intervened by doctors
  pchp_lookup_final$intervention<-0
  pchp_lookup_final$intervention[(pchp_lookup_final$OH_17_now==1|pchp_lookup_final$Cerc_now_T==1)]<-1
  # whether belongs to totally unknown group
  pchp_lookup_final$unknown<-0
  pchp_lookup_final$unknown[(pchp_lookup_final$Prehist_comb==0)&!(pchp_lookup_final$OH_17_now==1|pchp_lookup_final$Cerc_now_T==1)]<-1
  
  
  new_pchp_lookup_final<-pchp_lookup_final[!((pchp_lookup_final$Preg_NA==0)&(pchp_lookup_final$gap_NA==1)), ]
  
  #### 8 model features ####
  model_features <- c('num_house','num_inpt_T','num_outpt_T','gap_days_correct_T_S','ho_diabetes_T','abortion_now_T','Mean_Ratio_T','Obstetric_NA')
  
  #### load predictive model ####
  # loaded model is called mod_fit
  load("/home/michelle/T-Drive/PCHP PreTerm/Data/Train_Test All Datasets/Fix3_15.RData")
  #load("T:/PCHP PreTerm/Data/Train_Test All Datasets/Fix3_15.RData")
  
  testing4 <- new_pchp_lookup_final[, model_features]
  temp <- predict(mod_fit, newdata = testing4)   
  scoremat <- exp(temp)/(1+exp(temp))
  score <- exp(temp)/(1+exp(temp))
  
  # extract model coefficients
  coef <- mod_fit$coefficients[2:9]
  
  riskfactor1 <- array()
  riskfactor11 <- array()
  riskfactor111 <- array()
  
  # calculate the top 3 risk factors for each patient in each category - clinical, hospital, social
  for (m in 1:dim(testing4)[1]){
    
    # for each patients, model coefficient * her own feature value
    t_rec_con1 <- coef*testing4[m, ]
    t_rec_con1 <- abs(t_rec_con1)
    
    # 3 clinical features
    t_rec_con1_clinical <- t_rec_con1[, names(t_rec_con1) %in% c('abortion_now_T','ho_diabetes_T','Obstetric_NA')]
    names(t_rec_con1_clinical) <- c('Threatened Abortion during current pregnancy','History of Diabetes','Obstetric history is missing')
    t_rec_con1_clinical <- t_rec_con1_clinical[order(t_rec_con1_clinical, decreasing = TRUE)]
    riskfactor1[m] <- names(t_rec_con1_clinical)[1]
    
    t_rec_con1_hospital <- t_rec_con1[, names(t_rec_con1) %in% c('num_inpt_T','num_outpt_T')]
    names(t_rec_con1_hospital) <- c('Numbers of inpatient Admissions to Date during current pregnancy','Number of Outpatient Visits to Date during current pregnancy')
    t_rec_con1_hospital <- t_rec_con1_hospital[order(t_rec_con1_hospital, decreasing = TRUE)]
    riskfactor11[m] <- names(t_rec_con1_hospital)[1]
    
    t_rec_con1_social <- t_rec_con1[, names(t_rec_con1) %in% c('num_house','gap_days_correct_T_S','Mean_Ratio_T')]
    names(t_rec_con1_social) <- c('Numbers of Address Changes in the past 12 months','Scaled Insurance gap days in the past 12 months','Annual Income Mean Ratio')
    t_rec_con1_social <- t_rec_con1_social[order(t_rec_con1_social, decreasing = TRUE)]
    riskfactor111[m] <- names(t_rec_con1_social)[1]
  }
  
  #### define SPECIAL groups and clusters ####
  
  # identify IDs in the special groups and clusters that we need to apply different cutoffs
  
  # if the patients used 17OH or had cervical cerclage, they are already intervened by doctors - G1
  ID_intervention <- which(new_pchp_lookup_final$OH_17_now == 1 | new_pchp_lookup_final$Cerc_now_T == 1)
  
  # by excluding intervened patients, find patients with preterm history, with and without PIH - G5 and G6
  ID_pretermHis_pi <- which( ( (new_pchp_lookup_final$Prehist_comb == 1) & (new_pchp_lookup_final$pi_hypertension_now_T == 1) ) & ! (new_pchp_lookup_final$OH_17_now == 1 | new_pchp_lookup_final$Cerc_now_T == 1) )
  ID_pretermHis_non_pi <- which( ( (new_pchp_lookup_final$Prehist_comb == 1) & (new_pchp_lookup_final$pi_hypertension_now_T == 0) ) & !(new_pchp_lookup_final$OH_17_now == 1 | new_pchp_lookup_final$Cerc_now_T == 1) )
  
  # by excluding intervened and with preterm history patients, the rest patients are in totally unknown group
  # we are mostly interested in hidden factors among patients in this group
  
  # patients not intervened, no or missing preterm history, with PIH - G7
  ID_unknown_pi <- which( (new_pchp_lookup_final$pi_hypertension_now_T == 1) & (new_pchp_lookup_final$Prehist_comb == 0) & !(new_pchp_lookup_final$OH_17_now == 1 | new_pchp_lookup_final$Cerc_now_T == 1) )
  
  # patients not intervened, no preterm history, without PIH, will be further divided
  # these patients are used in training
  ID_unknown_non_pi <- which( (new_pchp_lookup_final$pi_hypertension_now_T == 0) & (new_pchp_lookup_final$Prehist_comb == 0) & !(new_pchp_lookup_final$OH_17_now == 1 | new_pchp_lookup_final$Cerc_now_T == 1) )
  
  # above + insurance gap info is missing - C3
  ID_unknown_non_pi_C3 <- which( (new_pchp_lookup_final$gap_NA == 1) & (new_pchp_lookup_final$pi_hypertension_now_T == 0) & (new_pchp_lookup_final$Prehist_comb == 0) & !(new_pchp_lookup_final$OH_17_now == 1 | new_pchp_lookup_final$Cerc_now_T == 1) )
  # above + insurance gap info is not missing + preterm history info is not missing - C1
  ID_unknown_non_pi_C1 <- which( ( (new_pchp_lookup_final$gap_NA == 0) & (new_pchp_lookup_final$prehistory_NA == 0) ) & (new_pchp_lookup_final$pi_hypertension_now_T == 0) & (new_pchp_lookup_final$Prehist_comb == 0) & !(new_pchp_lookup_final$OH_17_now == 1 | new_pchp_lookup_final$Cerc_now_T == 1) )
  # above + insurance gap info is not missing + preterm history info is missing - C2
  ID_unknown_non_pi_C2 <- which( ( (new_pchp_lookup_final$gap_NA == 0) & (new_pchp_lookup_final$prehistory_NA == 1) ) & (new_pchp_lookup_final$pi_hypertension_now_T == 0) & (new_pchp_lookup_final$Prehist_comb == 0) & !(new_pchp_lookup_final$OH_17_now == 1 | new_pchp_lookup_final$Cerc_now_T == 1) )
  
  scoremat_G1<-as.numeric(scoremat[ID_intervention])
  scoremat_G5<-as.numeric(scoremat[ID_pretermHis_pi])
  scoremat_G6<-as.numeric(scoremat[ID_pretermHis_non_pi])
  scoremat_G7<-as.numeric(scoremat[ID_unknown_pi])
  scoremat_C1<-as.numeric(scoremat[ID_unknown_non_pi_C1])
  scoremat_C2<-as.numeric(scoremat[ID_unknown_non_pi_C2])
  scoremat_C3<-as.numeric(scoremat[ID_unknown_non_pi_C3])
  
  # risk stratification for Cluster 1
  risk_char = c('Very High', 'High', 'Medium', 'Low')
  aa = quantile(scoremat_C1,  c( 0, .8, .9,.975,1), type = 3)
  k=1
  for(jj in seq((length(aa)-1), 1)){
    scoremat_C1[(scoremat_C1>=aa[jj]) & (scoremat_C1<=aa[jj+1])] = risk_char[k]
    k = k+1
  }
  
  # risk stratification for Cluster 2
  risk_char = c('High', 'Low')
  aa = quantile(scoremat_C2,c(0, .5, 1), type = 1)
  k=1
  for(jj in seq((length(aa)-1), 1)){
    scoremat_C2[(scoremat_C2>=aa[jj]) & (scoremat_C2<=aa[jj+1])] = risk_char[k]
    k = k+1
  }
  
  # risk stratification for Cluster 3
  scoremat_C3<-rep('High',length(scoremat_C3))
  
  # risk stratification for Group 7
  risk_char = c('Very High', 'High', 'Medium')
  aa = quantile(scoremat_G7,c(0, 0.5, .75, 1), type = 1)
  k=1
  for(jj in seq((length(aa)-1), 1)){
    scoremat_G7[(scoremat_G7>=aa[jj]) & (scoremat_G7<=aa[jj+1])] = risk_char[k]
    k = k+1
  }
  
  # risk stratification for Group 5
  scoremat_G5<-rep('Very High',length(scoremat_G5))
  
  # risk stratification for Group 6
  risk_char = c('Very High', 'High', 'Medium')
  aa = quantile(scoremat_G6,c(0, .8, 0.95, 1), type = 2)
  k=1
  for(jj in seq((length(aa)-1), 1)){
    scoremat_G6[(scoremat_G6>=aa[jj]) & (scoremat_G6<=aa[jj+1])] = risk_char[k]
    k = k+1
  }
  
  # risk stratification for Group 1
  risk_char = c('Very High', 'High')
  aa = quantile(scoremat_G1,c(0, .75, 1), type = 1)
  k=1
  for(jj in seq((length(aa)-1), 1)){
    scoremat_G1[(scoremat_G1>=aa[jj]) & (scoremat_G1<=aa[jj+1])] = risk_char[k]
    k = k+1
  }
  
  # add exact/absolute risk scores
  new_pchp_lookup_final$score=score
  new_pchp_lookup_final$level=scoremat
  new_pchp_lookup_final$level[ID_intervention]=scoremat_G1
  new_pchp_lookup_final$level[ID_pretermHis_pi]=scoremat_G5
  new_pchp_lookup_final$level[ID_pretermHis_non_pi]=scoremat_G6
  new_pchp_lookup_final$level[ID_unknown_pi]=scoremat_G7
  new_pchp_lookup_final$level[ID_unknown_non_pi_C1]=scoremat_C1
  new_pchp_lookup_final$level[ID_unknown_non_pi_C2]=scoremat_C2
  new_pchp_lookup_final$level[ID_unknown_non_pi_C3]=scoremat_C3
  new_pchp_lookup_final$riskfactor1<-riskfactor1
  new_pchp_lookup_final$riskfactor11<-riskfactor11
  new_pchp_lookup_final$riskfactor111<-riskfactor111
  
  return(new_pchp_lookup_final)
}

# CHANGE index
pchp_final <- adjust_pchp(Final_42)   
dim(pchp_final)  # 130 var to 192 var


# 55 features for complete monthly report
report_features <- c(
  'MEMBER_ID',
  'LastName',
  'FirstName',
  'Age',
  'DOB',
  'Phone',
  'ZipCode',
  'Address1',
  'Address2',
  'Address3',
  'Ethnicity',
  'RiskGroupID',
  'OBGYN_ID',
  'OBGYN_Fullname',
  'OBGYN_Affil_ID',
  'OBGYN_Affil_Fullname',
  'OBGYN_ID_Missing',
  'Recent_PCPID',
  'Pcp_Fullname_1',
  'Pcp_Affil_Fullname_1',
  'large_provider',
  'score',
  'level',
  'riskfactor1',
  'riskfactor11',
  'riskfactor111',
  'Prehist_D',
  'Prehist_comb',
  'ho_hypertension_T',
  'ho_diabetes_T',
  'abortion_now_T',
  'pi_hypertension_now_T',
  'ges_diabetes_now_T',
  'OH_17_now',
  'num_OH_17_now',
  'Cerc_now_T',
  'num_pre_T',
  'num_outpt_T',
  'num_out_pre_T',
  'num_out_pre_month_T',
  'num_ED_T',
  'num_inpt_T',
  'num_ED_inpt_T',
  'num_ED_inpt_month_T',
  'recent_Previsit',
  'recent_outpt',
  'recent_ED',
  'recent_inpt',
  'first_elg_start_now',
  'first_elg_start',
  'first_HEDISDate',
  'EffectiveDate',
  'intervention',
  'unknown',
  'recent'    # new feature added by Xiao in the beginning of step 11
)

# 55 features for complete monthly report, with detailed and explained feature names
report_features_ds <- c(
  'Member ID',
  'Last Name',
  'First Name',
  'Age',
  'Date of Birth',
  'Phone Number',
  'Zip Code',
  'Address 1',
  'Address 2',
  'Address 3',
  'Ethnicity',
  'Risk Group ID',
  'OB-GYN Provider ID',
  'OB-GYN Provider',
  'OB-GYN Provider Affiliation ID',
  'OB-GYN Provider Affiliation',
  'OBGYN Provider ID Missing',
  'PCP ID',
  'PCP Provider',
  'PCP Provider Affiliation',
  'Large Provider Group',
  'Risk Score',
  'Risk Level',
  'Clinical Risk Factor #1',
  'Hospitalization Risk Factor #1',
  'SDH Risk Factor #1',
  'History of Preterm Delivery',
  'History of Preterm Labor/Delivery',
  'History of Hypertension',
  'History of Diabetes',
  'Current Threatened Abortion',
  'Current PIH',
  'Current Gestational Diabetes',
  '17-OH-Progesterone Current',
  '17-OH-Progesterone Current (# of doses)',
  'Current Cerclage',
  'Number of Prenatal Visits to Date',
  'Number of Outpatient Visits to Date',
  'Number of Prenatal/Outpatient Visit to Date',
  'Average Number of Prenatal/Outpatient Visit per month to Date',
  'Number of ED Visits to Date',
  'Number of Inpatient Admissions to Date',
  'Number of ED/Inpatient Visit to Date',
  'Average Number of ED/Inpatient Visit per month to Date',
  'Date of Most Recent Prenatal Visit', 
  'Date of Most Recent Outpatient Visit', 
  'Date of Most Recent ED Visit',
  'Date of Most Recent Inpatient Admission',
  'First Enrollment Date of insurance during index pregnancy',
  'First Enrollment Date of insurance in past 12 months',
  'First Identification Date of Pregnancy based on HEDIS',
  'Start date of insurance for Pregnancy',
  'Intervention',
  'Unknown',
  'Date of Most Recent Outpatient/Prenatal Visit'   # added by Xiao in the beginning of step 11
)

# 19 features for provider-level reports - create a shorter version of report for doctors
report_features_Doc <- c(
  'MEMBER_ID',
  'LastName',
  'FirstName',
  'Age',
  'Phone',
  'ZipCode',
  'OBGYN_Fullname',
  'OBGYN_Affil_Fullname',
  'large_provider',
  'level',
  'Prehist_comb',
  'ho_hypertension_T',
  'ho_diabetes_T',
  'abortion_now_T',
  'num_out_pre_month_T',
  'num_ED_inpt_T',
  'num_ED_inpt_month_T',
  'intervention',
  'unknown'
)


pchp_short <- pchp_final[, report_features]   
dim(pchp_short)  # 55 var
names(pchp_short) <- report_features_ds

# CHANGE month below
setwd("/home/michelle/T-Drive/PCHP PreTerm/Data/Production/March2021")
#setwd("T:/PCHP PreTerm/Data/Production/March2021")

# CHANGE jj to make report name into the current month
jj <- 0  # 0, 1
temp <- paste('PCHP_Preterm_Report_Update_0520_', months(Sys.Date()-months(jj)), '.csv', sep = '')
temp
write.csv(pchp_final, temp, row.names = FALSE)
temp <- paste('PCHP_Preterm_Report_Short_Update_0520_', months(Sys.Date()-months(jj)), '.csv', sep = '')
write.csv(pchp_short, temp, row.names = FALSE)

names(pchp_short) <- report_features
pchp_lookup_short <- pchp_short[, report_features_Doc]  # 19 var
names(pchp_short) <- report_features_ds


# create 27 provider-level reports + figures
# if a large provider changes name, final report name changes and need to make corresponding changes in the following places -
# 1) pchp_provider below
# 2) make change if large provider name changes in for-loop below

# [Important] since XLConnect library is not available in Jupyter hub, we need to save&load these two intermeidate df and run final for loop in local machine
save(pchp_final, file = "pchp_final.rdata")
save(pchp_lookup_short, file = "pchp_lookup_short.rdata")
# CHANGE month and year below
setwd("T:/PCHP PreTerm/Data/Production/March2021")
load("pchp_final.rdata")
load("pchp_lookup_short.rdata")


pchp_providers <- c("Missing OBGYN And Have PCP",
                    "ParklandOB",
                    "MacArthur Medical Center",
                    "Dr Deleon Womens Healthcare Clinic",
                    "UTSW",
                    "PRESBYTERIAN HOSPITAL",
                    "OB Hospitalists of Texas",
                    "Bernard Adami,MD P.A.",    # CHANGED into "BERNARD F ADAMI MD" in Feb 2020, see next line, made changes in large provider assignment process in step 10, so changed back
                    #"BERNARD F ADAMI MD",
                    "Carlos AND Parnell,MD,PA", # NO PATIENT in Jan/Feb 2020 reports
                    "WOMENS SPECIALTY CENTER",
                    "Health Central, PA",
                    "BRUCE,RAJALA",             # MANUALLY MATCHED PCPID TO PCPNAME IN STEP 10/10.5, MISSING INFO SINCE DEC2018 FROM AETNA PROV FILES
                    "Comprehensive OBGYN",
                    "Premier OBGYN Center",
                    "F.Thomas Dean,MD",         # CHANGED into "FRANKTHOMASDEANMD" in Jan 2020, see next line, made changes in large provider assignment process in step 10, so changed back
                    #"FRANKTHOMASDEANMD",
                    "JOSEPH,P,BEHAN",
                    "LOS BARRIOS UNIDOS COMMUNITY CLINIC",
                    "Fetal Care Consultants",     # NO PATIENT in Dec 2019 and Jan/Feb/May 2020 reports
                    "LEGACY WOMENS HEALTH PLLC",  # NO PATIENT in Sep/Oct/Nov 2019 reports
                    "White Rock OBGYN",
                    "Advanced Womens Healthcare",
                    "Methodist ObGyn",
                    #"SUMMER,N,ABUBAKER",         # CHANGED into "SUMMER,N,MIGONI" in Nov 2018, see next line
                    "SUMMER,N,MIGONI",
                    "Texas Regional Womens Health Center",
                    "BLAKE,E,FRIEDEN",
                    "Missing PCP",
                    "Pierce Group"                # NO PATIENT in May 2020 reports
)

providers <- pchp_providers
empty.provs <- character(0)

## copy and paste RData into VM
#load("/home/michelle/T-Drive/PCHP PreTerm/Data/Production/patient_num_by_large_provider.RData")
load("T:/PCHP PreTerm/Data/Production/patient_num_by_large_provider.RData")
patient_num %>% head(5)
current_pat_number <- list()

## CHANGE month below
#load("/home/michelle/T-Drive/PCHP PreTerm/Data/Production/March2021/March2021_1_year_Preterm.RData")
load("March2021_1_year_Preterm.RData")


for (prov in providers){
  
  #prov <- providers[1]  # for debugging
  
  mat <- pchp_final[pchp_final$large_provider %in% prov, ]  # filter all patients from complete report for this large provider
  mat2 <- pchp_lookup_short[pchp_lookup_short$large_provider %in% prov, ]  # filter all patients from short report for this large provider
  
  # Xiao added in Dec 2019, to append number of patients for each large provider into a list
  current_pat_number <- c(current_pat_number, nrow(mat2))
  
  mat2 <- mat2[apply(mat2, 1, function(y) !all(is.na(y))),]
  members <- length(mat2$MEMBER_ID[mat2$large_provider == prov])    # how many patients for this large provider
  
  # current patient number of visit below or above pchp average
  pchp <- mean(pchp_final$num_out_pre_month_T[!is.na(pchp_final$num_out_pre_month_T)])  # mean num_out_pre_month_T for all pchp patients, used to compare
  
  # Xiao changed from 1 to current version in Dec 17, 2019
  if(as.numeric(members) == 0){mat2$over <- character()}
  if(as.numeric(members) > 0){mat2$over <- 1}
  
  mat2$over[which(mat2$num_out_pre_month_T > pchp)] <- 'Yes'
  mat2$over[which(mat2$num_out_pre_month_T <= pchp)] <- 'No'
  mat2 <- mat2[!names(mat2) %in% c("PCPID", "large_provider", "Pcp_Affil_Fullname")]
  mat2 <- mat2[, c(names(mat2)[1:14], 'over', 'num_ED_inpt_T', 'intervention', 'unknown')]
  
  # convert columns into categorical variable
  mat2$Prehist_comb <- ifelse(mat2$Prehist_comb == 0, 'No', 'Yes')
  mat2$ho_hypertension_T <- ifelse(mat2$ho_hypertension_T == 0, 'No', 'Yes')
  mat2$ho_diabetes_T <- ifelse(mat2$ho_diabetes_T == 0, 'No', 'Yes')
  mat2$abortion_now_T <- ifelse(mat2$abortion_now_T == 0, 'No', 'Yes')
  mat2$intervention <- ifelse(mat2$intervention == 0, 'No', 'Yes')
  mat2$unknown <- ifelse(mat2$unknown == 0, 'No', 'Yes')
  
  # 18 var names in final report
  report_features_ds_Doc <- c(
    'Member ID',
    'Last Name',
    'First Name',
    'Age',
    'Phone Number',
    'Zip Code',
    'OB-GYN Provider',
    'OB-GYN Provider Affiliation',
    'History of Preterm Labor/Delivery',
    'History of Hypertension',
    'History of Diabetes',
    'Current Threatened Abortion',
    'Average Number of Prenatal/Outpatient Visit per month to Date',
    'Exceed Average PCHP Number of Prenatal/Outpatient Visit per month to Date',
    'Number of ED/Inpatient Visit to Date',
    'Intervention Group(17OH or cerclage)',
    'Unknown Group',
    'Risk Score')
  
  if(members == 0){ # if no patient is associated with current large provider
    
    empty.provs <- append(empty.provs, prov, after = length(empty.provs))
    
    print(paste("No patient is found for large provider ", prov, ".", sep = ""))
    
    # BUT we still need to generate an empty excel with figure for preterm rate in past 12 months for this provider
    # don't need prenatal visit rate and 17OH figures, Xiao added in Dec 17 2019
    mat2$RiskScores <- character()
    names(mat2) <- report_features_ds_Doc
    
    #detach("package:xlsx", unload = TRUE)
    wb <- XLConnect::loadWorkbook(paste(prov, '.xlsx', sep = ''), create = TRUE)
    XLConnect::createSheet(wb, 'Provider')
    XLConnect::writeWorksheet(wb, mat2, sheet = 1)
    
    # sheet 2: preterm delivery rate in the past 12 months
    
    png(filename = 'pretermrate.png', width = 800, height = 500)
    devAskNewPage(ask = FALSE)
    
    XLConnect::createSheet(wb, name = 'pretermrate')
    XLConnect::createName(wb, name = "pretermrate", formula = "pretermrate!$B$2")
    
    pchp = (sum(patient_l$Y)/dim(patient_l)[1])*100
    provider = prov
    mat_preterm <- patient_l[patient_l$large_provider%in%prov,]
    if(nrow(mat_preterm) == 0){
      amr = 0
    }else{
      amr = (sum(mat_preterm$Y)/dim(mat_preterm)[1])*100 
    }
    a = c(amr, pchp)
    names = c(provider, "PCHP")
    z <- barplot(a, names = names, ylim = c(0, max(a)+1),  col = c("#3DBBCC", "#CC3A4C"))
    title(paste(provider, " (N = ", dim(mat_preterm)[1], "):","\nPreterm Rate(%) of all deliveries in the past 12 months", sep = ""))
    text( x = c(z[1,], z[2,]), 
          y = c(a + par("cxy")[2], xpd = FALSE), round(a, 2), cex = 1)
    grid()
    dev.off()
    XLConnect::addImage(wb, filename = "pretermrate.png", name = "pretermrate", originalSize = TRUE)
    
    temp = paste(prov, ' report.xlsx')
    saveWorkbook(wb, file = temp)
  } 
  else{
    
    risk <- c("Very High", "High", "Medium", "Low")
    mat2$RiskScores <- factor(mat2$level, levels = risk, ordered = TRUE)
    
    # final reports ordered by intervention first then risk scores, added by xiao in Feb 2019
    mat2 <- mat2[order(mat2$intervention, mat2$RiskScores), ]
    
    mat2 <- mat2[, !names(mat2) %in% c('level')]
    names(mat2) <- report_features_ds_Doc
    
    wb <- XLConnect::loadWorkbook(paste(prov, '.xlsx', sep = ''), create = TRUE)
    XLConnect::createSheet(wb, 'Provider')
    XLConnect::writeWorksheet(wb, mat2, sheet = 1)
    
    # sheet2: average outpatient/prenatal visits per month per patient
    
    png(filename = 'outprepermonth.png', width = 800, height = 500)
    devAskNewPage(ask = FALSE)
    
    XLConnect::createSheet(wb, name = 'PrenatalOutpatientpermonth')
    XLConnect::createName(wb, name = "PrenatalOutpatientpermonth", formula = "PrenatalOutpatientpermonth!$B$2")
    
    N = length(mat2[,1])
    sd = sd(pchp_final$num_out_pre_month_T[!is.na(pchp_final$num_out_pre_month_T)])/sqrt(N)
    pchp = mean(pchp_final$num_out_pre_month_T[!is.na(pchp_final$num_out_pre_month_T)])
    provider = prov
    amr = mean(mat$num_out_pre_month_T[!is.na(mat$num_out_pre_month_T)])
    a = c(amr, pchp)
    names = c(provider, "PCHP")
    z <- barplot(a, names = names, ylim = c(0, max(a)+.5), col = c("#3DBBCC", "#CC3A4C"))
    abline(h = pchp + 2*sd, col = "#6BAF44", lwd = 2)
    abline(h = pchp - 2*sd, col = "#6BAF44", lwd = 2)
    title(paste(provider, " (N = ", N, "):","\nAverage numbers of prenatal/outpatient visits per member per month", sep = ""))
    text( x = c(z[1,], z[2,]), 
          y = c(a + par("cxy")[2], xpd=FALSE), round(a, 2), cex = 1)
    legend('topright', c('PCHP ?? 2 SD'), col = "#6BAF44", lty = 1)
    grid()
    dev.off()
    XLConnect::addImage(wb, filename = "outprepermonth.png", name = "PrenatalOutpatientpermonth", originalSize = TRUE)
    
    # sheet 3: eligible pregnancies on 17_OH-P currently among pregnant women with preterm delievery/history
    
    png(filename = 'OH17_now_prehist.png', width = 800, height = 500)
    devAskNewPage(ask = FALSE)
    
    XLConnect::createSheet(wb, name= 'OH17_now_prehist')
    XLConnect::createName(wb, name = "OH17_now_prehist", formula = "OH17_now_prehist!$B$2")
    
    N = length(mat2[,1])
    
    # Xiao changed code in Dec 15, 2019
    #pchp = (sum(pchp_final[pchp_final$Prehist_D==1, ]$OH_17_now)/dim(pchp_final[pchp_final$Prehist_D==1, ])[1])*100
    pchp = (sum(pchp_final[pchp_final$Prehist_D==1, ]$OH_17_now, na.rm = TRUE)/dim(pchp_final[pchp_final$Prehist_D==1, ])[1])*100
    
    provider = prov
    if (length(mat[mat$Prehist_D==1,][,1])==0){amr=0} else {amr=(sum(mat[mat$Prehist_D==1,]$OH_17_now)/length(mat[mat$Prehist_D==1,][,1]))*100}
    a = c(amr, pchp)
    names = c(provider, "PCHP")
    z <- barplot(a, names = names, ylim = c(0, max(a)+10), col = c("#3DBBCC", "#CC3A4C"))
    title(paste(provider, " (N = ", N, "):","\n%17-OH AND VAGINAL PROGESTERONE USAGE among Pregnant Women with Preterm Delivery History", sep = ""))
    text( x = c(z[1,], z[2,]), 
          y = c(a + par("cxy")[2], xpd = FALSE), round(a, 2), cex = 1)
    grid()
    dev.off()
    XLConnect::addImage(wb, filename = "OH17_now_prehist.png", name = "OH17_now_prehist", originalSize = TRUE)
    
    # sheet 4: preterm delivery rate in the past 12 months
    
    png(filename = 'pretermrate.png', width = 800, height = 500)
    devAskNewPage(ask = FALSE)
    
    XLConnect::createSheet(wb, name = 'pretermrate')
    XLConnect::createName(wb, name = "pretermrate", formula = "pretermrate!$B$2")
    
    
    #### make change if large provider name changes ####
    patient_l$large_provider[which(patient_l$large_provider=='BROCK,L,PIERCE')]<-'Pierce Group'
    patient_l$large_provider[which(patient_l$large_provider=='BLAKEEFRIEDENMD')]<-'BLAKE,E,FRIEDEN'  # weiwei kept old name, xiao kept new name
    # ADDED by xiao in Nov2018
    patient_l$large_provider[which(patient_l$large_provider=='SUMMER,N,ABUBAKER')]<-'SUMMER,N,MIGONI'
    ## ADDED by xiao in Jan2020
    #patient_l$large_provider[which(patient_l$large_provider=='F.Thomas Dean,MD')]<-'FRANKTHOMASDEANMD'
    ## ADDED by xiao in Feb2020
    #patient_l$large_provider[which(patient_l$large_provider=='Bernard Adami,MD P.A.')]<-'BERNARD F ADAMI MD'
    
    
    pchp = (sum(patient_l$Y)/dim(patient_l)[1])*100
    provider = prov
    mat_preterm <- patient_l[patient_l$large_provider%in%prov,]
    amr = (sum(mat_preterm$Y)/dim(mat_preterm)[1])*100
    a = c(amr, pchp)
    names = c(provider, "PCHP")
    z <- barplot(a, names = names, ylim = c(0, max(a)+1),  col = c("#3DBBCC", "#CC3A4C"))
    title(paste(provider, " (N = ", dim(mat_preterm)[1], "):","\nPreterm Rate(%) of all deliveries in the past 12 months", sep = ""))
    text( x = c(z[1,], z[2,]), 
          y = c(a + par("cxy")[2], xpd = FALSE), round(a, 2), cex = 1)
    grid()
    dev.off()
    XLConnect::addImage(wb, filename = "pretermrate.png", name = "pretermrate", originalSize = TRUE)
    
    
    temp = paste(prov, ' report.xlsx')
    saveWorkbook(wb, file = temp)
  }
}

#setwd("/home/michelle/T-Drive/PCHP PreTerm/Data/Production/MONTHYEAR/")
write.csv(empty.provs, file = "no_patient_provider_summary.csv")
# append current month patient numbers for all large provider 
patient_num$patient_number <- current_pat_number
# CHANGE the month below
names(patient_num)[names(patient_num) == "patient_number"] <- "Mar2021"
# add patient_number column again for next month
patient_num$patient_number <- 0
#save(patient_num, file = "/home/michelle/T-Drive/PCHP PreTerm/Data/Production/patient_num_by_large_provider.RData")
save(patient_num, file = "T:/PCHP PreTerm/Data/Production/patient_num_by_large_provider.RData")

# all resulting reports are in the Production folder of current month 
# in total 31 files = 3 png (keep updating for different large providers' reports) + 27 xlsx reports + 1 csv summary
rm(list = ls())

