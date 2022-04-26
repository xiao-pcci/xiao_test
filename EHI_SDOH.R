# EHI SDOH data pull
# 11/22/2021


# load EHI cohort data
library(readr)
EHI <- read_delim("/home/michelle/EHI/rev_geocoded_EHI_population.csv", delim = ",", col_names = T)  # 2396
EHI %>% filter(blockgroup == "Not Found") %>% tally()  # 150
table(EHI$County)
EHI %>% filter(County != "DALLAS") %>% tally()  # 517
EHI$zip_m <- substr(EHI$PostalCode,1,5)


CDI_path <- "/home/michelle/T-Drive/PCHP PreTerm/Data/CDI_Longitudinal_Data/"
# CDI_path <- "\\sc-acs228-1.parknet-ad.pmh.org\users$\p89514\Documents\XiaoWang\Xiao_PCCI\05 CDI\CDI_Longitudinal_Data"


# food insecurity
food <- read.csv(paste(CDI_path, "indicator_food_insecurity.csv", sep = ""))  # 2546
food <- food %>% filter(indicator_value != "NULL") %>% distinct()  # 2539
table(food$year)  # all 2016
food$food <- as.numeric(as.character(food$indicator_value))
food %>% group_by(blockgroup) %>% tally() %>% filter(n > 1) %>% arrange(desc(n))
# zip-level aggregation for heatmap
food_zip <- food %>% group_by(zipcode) %>% dplyr::summarise(n = n(), food = round(mean(food),2))  # 94 unique zips
# block group level aggregation
food <- food %>% group_by(blockgroup) %>% dplyr::summarise(n = n(), food = round(mean(food),2))  # 1665
EHI <- merge(EHI, food[,c("blockgroup","food")], by = "blockgroup", all.x = T)
EHI %>% filter(is.na(food)) %>% tally()  # 587
write.csv(food_zip, file = "/home/michelle/EHI/indicator_food_insecurity_zip_mean.csv")


# [TODO] take only 2017 indicator? or average over years (current maps)?
# poverty level
# document said up is good - but checked that highland park has lower numbers than CHNA zips?
poverty <- read.csv(paste(CDI_path, "indicator_poverty.csv", sep = ""))  # 20296
table(poverty$indicator_value == "NULL")  # all F
table(poverty$year)  # 2010 to 2017
poverty$poverty <- as.numeric(as.character(poverty$indicator_value))
poverty_zip <- poverty %>% group_by(year, zipcode) %>% dplyr::summarise(n = n(), poverty = round(mean(poverty),2))  # 94 -> 752, by adding year in group_by
poverty <- poverty %>% group_by(blockgroup) %>% dplyr::summarise(n = n(), poverty = round(mean(poverty),2))  # 1666
EHI <- merge(EHI, poverty[,c("blockgroup","poverty")], by = "blockgroup", all.x = T)
EHI %>% filter(is.na(poverty)) %>% tally()  # 587
write.csv(poverty_zip, file = "/home/michelle/EHI/indicator_poverty_zip_mean1.csv")  # average by all years
write.csv(poverty_zip, file = paste(CDI_path, "indicator_poverty_zip_mean.csv", sep = ""))


# ADI
ADI <- read.csv(paste(CDI_path, "indicator_adi.csv", sep = ""))  # 2538
ADI$ADI <- as.numeric(as.character(ADI$indicator_value))
table(ADI$ADI)  # Q: why only 10, 20 etc?
ADI_zip <- ADI %>% group_by(zipcode) %>% dplyr::summarise(n = n(), ADI = round(mean(ADI),2))  # 94
ADI <- ADI %>% group_by(blockgroup) %>% dplyr::summarise(n = n(), ADI = round(mean(ADI),2))  # 1664
EHI <- merge(EHI, ADI[,c("blockgroup","ADI")], by = "blockgroup", all.x = T)
EHI %>% filter(is.na(ADI)) %>% tally()  # 587
write.csv(ADI_zip, file = "/home/michelle/EHI/indicator_ADI_zip_mean.csv")


# income level
# there are two source data for household income
# source 1: used in preterm model
income <- read.table("/home/michelle/T-Drive/PCHP PreTerm/Data/Household Income and Population2010.txt", header = TRUE)  # 32634
names(income) <- c("ZipCode","median_income","mean_income","population")
income$mean_income <- as.numeric(as.character(income$mean_income))
income$median_income <- as.numeric(as.character(income$median_income))
income$mean_ratio <- income$mean_income/38000
income$median_ratio <- income$median_income/38000
range(income$median_income)  # 33  223k
EHI <- merge(EHI, income, by.x = "zip_m", by.y = "ZipCode", all.x = T)
EHI %>% filter(is.na(mean_income)) %>% tally()  # 21
EHI %>% filter(is.na(mean_income))

# source 2: from CDI data
income <- read.csv(paste(CDI_path, "indicator_medianincome.csv", sep = ""))  # 20136
names(income)
table(income$year)  # 2010 to 2017
income <- income %>% filter(indicator_value != "NULL") %>% distinct()
income$income <- as.numeric(as.character(income$indicator_value))
# zip-level aggregation
income_zip <- income %>% group_by(zipcode, year) %>% dplyr::summarise(n = n(), income = round(mean(income),2))  # 752
write.csv(poverty_zip, file = paste(CDI_path, "indicator_medianincome_zip_mean.csv", sep = ""))


write.csv(EHI, file = "/home/michelle/EHI/rev_geocoded_EHI_population_SDOH1.csv")
write.csv(EHI, file = "/home/michelle/EHI/rev_geocoded_EHI_population_SDOH2.csv")
  
  