# Created by Vy Mai on 10/20/21 (maivl@beloit.edu)
# Last modified on 12/12/2021 by Vy Mai
# Purpose: Create and analyze dataset from Internal Displacement Monitoring Center (IDMC), 
# World Risk Index (WRI) Report, Climate Risk Index (CRI) Report, and other World Bank dataset
# on countries' characteristics. 

# The dataset merge from many different datasets, including Internal displacement per 
# disaster events (2008-2020), WRI score (specifically the "Exposure" component), 
# (2011-2021), CRI Score (2009-2019), World Bank datasets, including:
#   1. Land Area (), 
#   2. Precipitation (2017): Average precipitation in depth (mm per year)
#   3. Employment in Agriculture
#   4. GDP per Capita: GDP per capita (current US$)
#   5. Elevation: Land area where elevation is below 5 meters (% of total land area)
#   6. Percentage of Forest
#   7. Data from World Bank include country-level 

######*********SET UP WORK SPACE*********######
#set up working  space
setwd("/Users/maingoclanvy/Documents/Thesis Code")
getwd()

######*********DOWNLOAD DATA*********######
#import, load, backup and save data
library(readxl)
#download Migration Excel file from the directory
MigData <- read_excel("IDMC_Internal_Displacement_Disasters_Events_2008_2020.xlsx")
CRIData09 <- read_excel("CRI_2009.xlsx")
CRIData10 <- read_excel("CRI_2010.xlsx")
CRIData11 <- read_excel("CRI_2011.xlsx")
CRIData12 <- read_excel("CRI_2012.xlsx")
CRIData13 <- read_excel("CRI_2013.xlsx")
CRIData14 <- read_excel("CRI_2014.xlsx")
CRIData15 <- read_excel("CRI_2015.xlsx")
CRIData16 <- read_excel("CRI_2016.xlsx")
CRIData17 <- read_excel("CRI_2017.xlsx")
CRIData18 <- read_excel("CRI_2018.xlsx")
CRIData19 <- read_excel("CRI_2019.xlsx")
WRIData <- read_excel("WorldRiskIndex2011-2021_time_series.xlsx")
WB_Land <- read_excel("WB_LandArea.xls")
WB_Precip <- read_excel("WB_AvgPrecipitation.xls")
WB_EmpAgri <- read_excel("WB_EmpInAgriculture.xls")
WB_GDPCapita <- read_excel("WB_GDPperCaptia.xls")
WB_Elevation <- read_excel("WB_LandElevationBelow5.xls")
WB_PercForest <- read_excel("WB_PercForestArea.xls")
WB_PopDensity <- read_excel("WB_PopDensity.xlsx")
WB_Poverty <- read_excel("WB_PovertyGap.xlsx")
WB_PercAgri <- read_excel("WB_PercAgriculture.xlsx")
WB_Income <- read_excel("WB_IncomeGroup.xlsx")
WB_Pop <- read_excel("WB_Population.xlsx")

#View(MigData)
#View(CRIData09)
View(WRIData)
save(MigData, file="Data.backup.RData")
save(WRIData, file="Data.backup.RData")
save(WB_Land, file="Data.backup.RData")
save(WB_Precip, file="Data.backup.RData")
save(WB_EmpAgri, file="Data.backup.RData")
save(WB_GDPCapita, file="Data.backup.RData")
save(WB_Elevation, file="Data.backup.RData")
save(WB_PercForest, file="Data.backup.RData")
save(WB_PopDensity, file="Data.backup.RData")
save(WB_Poverty, file="Data.backup.RData")
save(WB_PercAgri, file="Data.backup.RData")
save(WB_Income, file="Data.backup.RData")
save(WB_Pop, file="Data.backup.RData")

load("Data.backup.RData")

######*********Clean up WB_Pop*********######
WB_PopWide <- WB_Pop[-c(1:3),] #delete the first 3 rows

#rename variables
colnames(WB_PopWide) <- c('ISO','Country', '2008','2009','2010','2011', '2012',
                           '2013', '2014', '2015','2016', '2017', '2018', '2019',
                           '2020')

sapply(WB_PopWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016',
              '2017','2018','2019','2020')
WB_PopWide[cols.num] <- sapply(WB_PopWide[cols.num],as.numeric)
#View(WB_PopWide)

######*********Clean up MigData*********######
MigData = MigData[-1,] #delete the first row
View(MigData)
#rename variable
library(dplyr)
colnames(MigData) <- c('ISO','Country', 'Year', 'Start', 'Event', 'Category',
                       'Type', 'Migration')

###VALIDATE:Country Code match up
countrycode <- unique(MigData$ISO, incomparables = FALSE, fromLast = FALSE, nmax = 205)
#View(countrycode)
countryname <- unique(MigData$Country, incomparables = FALSE, fromLast = FALSE, nmax = 205)
#View(countryname)

#data only when Category is Weather related
weatherrelated<-subset(MigData,Category=="Weather related")

## Check for NA values
variables <- c(MigData[,1:8])
sapply(MigData, function(variables) sum(is.na(variables)))
summary(MigData)

# Number of NA values: Event:1202, Category:3, Type:3, Migration: 590
# Will need to 1) Drop NA 2) Replace NA with 0 3) Replace NA with 1
View(MigData)

## Replace NA value with 0
weatherrelated$Migration[is.na(weatherrelated$Migration)] = "0"

#convert Migration to numeric
weatherrelated$mig<-as.numeric(weatherrelated$Migration)
View(weatherrelated)
#aggregate sum by each Year and Country Code (Long panel data)
MigDataLong<-aggregate(weatherrelated$mig,
                    by=list(weatherrelated$ISO,weatherrelated$Year),
                    FUN = "sum")

View(MigDataLong)
colnames(MigDataLong)[1]<-"ISO"
colnames(MigDataLong)[2]<-"Year"
colnames(MigDataLong)[3]<-"Migration"

#convert long panel data to wide panel data
#install.packages('tidyr')
library(tidyr)

MigDataWide<-reshape(MigDataLong, timevar = "Year",idvar = "ISO",
                     direction='wide', new.row.names = NULL,
                     sep = "_",
                     split = if (sep == "") {
                       list(regexp = "[A-Za-z][0-9]", include = TRUE)
                       } else {
                         list(regexp = sep, include = FALSE, fixed = TRUE)}
                     )
View(MigDataWide)
summary(MigDataWide)
#Calculate internal migration rate
MigRateWide <- merge(MigDataWide, WB_PopWide, by="ISO") #merge MigDataWide & WB_PopWide
View(MigRateWide)
#delete columns that has "Country"
MigRateWide <- MigRateWide %>% select(-contains("Country"))

x = c("Migration_2008","Migration_2009","Migration_2010","Migration_2011",
      "Migration_2012","Migration_2013","Migration_2014","Migration_2015",
      "Migration_2016","Migration_2017","Migration_2018","Migration_2019",
      "Migration_2020")

library(tibble)
MigRateWide <- MigRateWide %>% add_column(MigRate_2008 = 
                                            (MigRateWide$Migration_2008/MigRateWide$'2008') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2009 = 
                                            (MigRateWide$Migration_2009/MigRateWide$'2009') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2010 = 
                                            (MigRateWide$Migration_2010/MigRateWide$'2010') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2011 = 
                                            (MigRateWide$Migration_2011/MigRateWide$'2011') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2012 = 
                                            (MigRateWide$Migration_2012/MigRateWide$'2012') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2013 = 
                                            (MigRateWide$Migration_2013/MigRateWide$'2013') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2014 = 
                                            (MigRateWide$Migration_2014/MigRateWide$'2014') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2015 = 
                                            (MigRateWide$Migration_2015/MigRateWide$'2015') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2016 = 
                                            (MigRateWide$Migration_2016/MigRateWide$'2016') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2017 = 
                                            (MigRateWide$Migration_2017/MigRateWide$'2017') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2018 = 
                                            (MigRateWide$Migration_2018/MigRateWide$'2018') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2019 = 
                                            (MigRateWide$Migration_2019/MigRateWide$'2019') * 100, .before = "Migration_2008")
MigRateWide <- MigRateWide %>% add_column(MigRate_2020 = 
                                            (MigRateWide$Migration_2020/MigRateWide$'2020') * 100, .before = "Migration_2008")
View(MigRateWide)
MigRateWide <- MigRateWide[,-c(15:40)]

colnames(MigRateWide) <- c('ISO', '2008','2009','2010','2011', '2012', '2013', 
                           '2014', '2015','2016', '2017', '2018', '2019', '2020')


#convert wide panel data to long panel data
library(tidyr)
variables <- c('2008', '2009', '2010', '2011', '2012', '2013',
               '2014', '2015', '2016', '2017', '2018', '2019', '2020')
Year <- "ISO"
MigRateLong <- gather(MigRateWide, Year, MigRate, variables)
View(MigRateLong)

library(psych)
describe(MigRateLong)
variables <- c(MigRateLong[,1:2])
sapply(MigRateLong, function(variables) sum(is.na(variables)))
View(MigRateLong)

######*********Clean up CRIData*********######
CRIData09 <- CRIData09[,-c(1,4:11)] #drop column 1 and columns 4-7
CRIData10 <- CRIData10[,-c(1,4:11)] #drop column 1 and columns 4-7
CRIData11 <- CRIData11[,-c(1,4:11)] #drop column 1 and columns 4-7
CRIData12 <- CRIData12[,-c(1,4:11)] #drop column 1 and columns 4-7
CRIData13 <- CRIData13[,-c(1,4:11)] #drop column 1 and columns 4-7
CRIData14 <- CRIData14[,-c(1,4:11)] #drop column 1 and columns 4-7
CRIData15<- CRIData15[,-c(1,4:11)] #drop column 1 and columns 4-7
CRIData16 <- CRIData16[,-c(1,4:12)] #drop column 1 and columns 4-7
CRIData17 <- CRIData17[,-c(1,4:11)] #drop column 1 and columns 4-7
CRIData18 <- CRIData18[,-c(1,4:7)] #drop column 1 and columns 4-7

View(CRIData09)
#View(CRIData10)
#View(CRIData11)
#View(CRIData12)
#View(CRIData13)
#View(CRIData14)
#View(CRIData15)
#View(CRIData16)
#View(CRIData17)
#View(CRIData18)
#View(CRIData19)

#Get country codes & country names
#install.packages("purrr")
library(purrr)
CountryMerge<-aggregate(weatherrelated$mig,
                       by=list(weatherrelated$ISO,weatherrelated$Country),
                       FUN = "sum")
colnames(CountryMerge)[1]<-"ISO"
colnames(CountryMerge)[2]<-"Country"
CountryMerge <- CountryMerge[,-c(3)] #drop column 3
View(CountryMerge)

CRIDataList = list(CountryMerge,CRIData09,CRIData10,CRIData11,CRIData12,
                   CRIData13,CRIData14,CRIData15,CRIData16,CRIData17,CRIData18,
                   CRIData19)

CRIDataWide <- CRIDataList %>% reduce(full_join, by = c("Country"))
colnames(CRIDataWide) <- c('ISO', 'Country', '2009', '2010', '2011', '2012', '2013',
                           '2014', '2015', '2016', '2017', '2018', '2019')
View(CRIDataWide)
?reduce()

#drop observation that has NA value in ISO column
CRIDataWide <- CRIDataWide[complete.cases(CRIDataWide[ ,1]),]
View(CRIDataWide)

#convert wide panel data to long panel data
library(tidyr)
variables <- c('2009', '2010', '2011', '2012', '2013',
               '2014', '2015', '2016', '2017', '2018', '2019')
Year <- "ISO"
CRIDataLong <- gather(CRIDataWide, key=Year, value=CRI, na.rm=FALSE, variables)
View(CRIDataLong)

library(psych)
describe(CRIDataLong)
variables <- c(CRIDataLong[,])
sapply(CRIDataLong, function(variables) sum(is.na(variables)))

######*********Clean up WRIData*********######
#Exposure
ExposureWide <- WRIData[,-c(3:25,36:139)] #drop column 3-25, 37-139
ExposureWide <- ExposureWide[-c(1,2),] #delete the first 2 rows
colnames(ExposureWide) <- c('Country','ISO', '2011', '2012', '2013', '2014', '2015',
                       '2016', '2017', '2018', '2019', '2020')

sapply(ExposureWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2011','2012','2013','2014','2015','2016','2017','2018','2019',
              '2020')
ExposureWide[cols.num] <- sapply(ExposureWide[cols.num],as.numeric)

#convert wide panel data to long panel data
library(tidyr)
variables <- c('2011','2012','2013','2014','2015','2016','2017','2018','2019',
               '2020')
Year <- "ISO"
ExposureLong <- gather(ExposureWide, Year, Exposure, variables)
View(WRIDataLong)

#Vulnerability
View(WRIData)
View(VulnerabilityWide)
VulnerabilityWide <- WRIData[,-c(3:48,59:139)] #drop column 3-25, 37-139
VulnerabilityWide <- VulnerabilityWide[-c(1,2),] #delete the first 2 rows
colnames(VulnerabilityWide) <- c('Country','ISO', '2011', '2012', '2013', '2014', '2015',
                            '2016', '2017', '2018', '2019', '2020')

sapply(VulnerabilityWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2011','2012','2013','2014','2015','2016','2017','2018','2019',
              '2020')
VulnerabilityWide[cols.num] <- sapply(VulnerabilityWide[cols.num],as.numeric)

#convert wide panel data to long panel data
library(tidyr)
variables <- c('2011','2012','2013','2014','2015','2016','2017','2018','2019',
               '2020')
Year <- "ISO"
VulnerabilityLong <- gather(VulnerabilityWide, Year, Vulnerability, variables)

#Susceptibility
SusceptibilityWide <- WRIData[,-c(3:71,82:139)] #drop column 3-25, 37-139
SusceptibilityWide <- SusceptibilityWide[-c(1,2),] #delete the first 2 rows
colnames(SusceptibilityWide) <- c('Country','ISO', '2011', '2012', '2013', '2014', '2015',
                                 '2016', '2017', '2018', '2019', '2020')

sapply(SusceptibilityWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2011','2012','2013','2014','2015','2016','2017','2018','2019',
              '2020')
SusceptibilityWide[cols.num] <- sapply(SusceptibilityWide[cols.num],as.numeric)

#convert wide panel data to long panel data
library(tidyr)
variables <- c('2011','2012','2013','2014','2015','2016','2017','2018','2019',
               '2020')
Year <- "ISO"
SusceptibilityLong <- gather(SusceptibilityWide, Year, Susceptibility, variables)

#Lack of adaptive capacities
AdaptWide <- WRIData[,-c(3:94,105:139)] #drop column 3-25, 37-139
AdaptWide <- AdaptWide[-c(1,2),] #delete the first 2 rows
colnames(AdaptWide) <- c('Country','ISO', '2011', '2012', '2013', '2014', '2015',
                                  '2016', '2017', '2018', '2019', '2020')

sapply(AdaptWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2011','2012','2013','2014','2015','2016',
              '2017','2018','2019','2020')
AdaptWide[cols.num] <- sapply(AdaptWide[cols.num],as.numeric)

#convert wide panel data to long panel data
library(tidyr)
variables <- c('2011','2012','2013','2014','2015','2016','2017','2018','2019',
               '2020')
Year <- "ISO"
AdaptLong <- gather(AdaptWide, Year, Adapt, variables)

#Lack of coping capacities
CopeWide <- WRIData[,-c(3:117,128:139)] #drop column 3-25, 37-139
CopeWide <- CopeWide[-c(1,2),] #delete the first 2 rows
colnames(CopeWide) <- c('Country','ISO', '2011', '2012', '2013', '2014', '2015',
                         '2016', '2017', '2018', '2019', '2020')

sapply(CopeWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2011','2012','2013','2014','2015','2016',
              '2017','2018','2019','2020')
CopeWide[cols.num] <- sapply(CopeWide[cols.num],as.numeric)

#convert wide panel data to long panel data
library(tidyr)
variables <- c('2011','2012','2013','2014','2015','2016','2017','2018','2019',
               '2020')
Year <- "ISO"
CopeLong <- gather(CopeWide, Year, Cope, variables)

# Check for NA at index variables
variables <- c(WRIDataLong[,1:2])
sapply(WRIDataLong, function(variables) sum(is.na(variables)))

######*********Clean up WB_Land*********######
WB_LandWide <- WB_Land[,-c(3:52,65)] #drop column 3-52 and 65
WB_LandWide <- WB_LandWide[-c(1:3),] #delete the first 3 rows

#rename variables
colnames(WB_LandWide) <- c('Country','ISO', '2008','2009','2010','2011', '2012',
                       '2013', '2014', '2015','2016', '2017', '2018', '2019')

sapply(WB_LandWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016',
              '2017','2018','2019')
WB_LandWide[cols.num] <- sapply(WB_LandWide[cols.num],as.numeric)
#View(WB_LandWide)

#convert wide panel data to long panel data
Year <- "ISO"
WB_LandLong <- gather(WB_LandWide, Year, Land, all_of(cols.num))
#View(WB_LandLong)

# Check for NA at index variables
variables <- c(WB_LandLong[,1:2])
sapply(WB_LandLong, function(variables) sum(is.na(variables)))

######*********Clean up WB_Precip*********######
WB_Precip <- WB_Precip[,-c(3:61,63:65)] #drop column 3-61 and 63-65
WB_Precip <- WB_Precip[-c(1:3),] #delete the first 3 rows

#rename variables
colnames(WB_Precip) <- c('Country','ISO', 'Precipitation2017')
View(WB_Precip)

sapply(WB_Precip, class) #check column class
#Transform 1 column into numeric
WB_Precip$`Precipitation2017`<-as.numeric(WB_Precip$`Precipitation2017`)

# Check for NA at index variables
variables <- c(WB_Precip[,1:2])
sapply(WB_Precip, function(variables) sum(is.na(variables)))

######*********Clean up WB_EmpAgri*********######
WB_EmpAgriWide <- WB_EmpAgri[,-c(3:52,65)] #drop column 3-52 and 65
WB_EmpAgriWide <- WB_EmpAgriWide[-c(1:3),] #delete the first 3 rows

#rename variables
colnames(WB_EmpAgriWide) <- c('Country','ISO', '2008','2009','2010','2011', '2012',
                          '2013', '2014', '2015','2016', '2017', '2018', '2019')

sapply(WB_EmpAgriWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016',
              '2017','2018','2019')
WB_EmpAgriWide[cols.num] <- sapply(WB_EmpAgriWide[cols.num],as.numeric)
#View(WB_EmpAgriWide)

#convert wide panel data to long panel data
Year <- "ISO"
WB_EmpAgriLong <- gather(WB_EmpAgriWide, Year, EmpAgri, all_of(cols.num))
#View(WB_EmpAgriLong)

# Check for NA at index variables
variables <- c(WB_EmpAgriLong[,1:2])
sapply(WB_EmpAgriLong, function(variables) sum(is.na(variables)))

######*********Clean up WB_GDPCapita*********######
WB_GDPCapitaWide <- WB_GDPCapita[,-c(3:52,65)] #drop column 3-52 and 65
WB_GDPCapitaWide <- WB_GDPCapitaWide[-c(1:3),] #delete the first 3 rows

#rename variables
colnames(WB_GDPCapitaWide) <- c('Country','ISO', '2008','2009','2010','2011', '2012',
                            '2013', '2014', '2015','2016', '2017', '2018', '2019')

sapply(WB_GDPCapitaWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016',
              '2017','2018','2019')
WB_GDPCapitaWide[cols.num] <- sapply(WB_GDPCapitaWide[cols.num],as.numeric)
#View(WB_GDPCapitaWide)

#convert wide panel data to long panel data
Year <- "ISO"
WB_GDPCapitaLong <- gather(WB_GDPCapitaWide, Year, GDPCapita, all_of(cols.num))
View(WB_GDPCapitaLong)

# Check for NA at index variables
variables <- c(WB_GDPCapitaLong[,1:2])
sapply(WB_GDPCapitaLong, function(variables) sum(is.na(variables)))

######*********Clean up WB_Elevation*********######
WB_Elevation <- WB_Elevation[,-c(3:54,56:65)] #drop column 3-52 and 65
WB_Elevation <- WB_Elevation[-c(1:3),] #delete the first 3 rows

#rename variables
colnames(WB_Elevation) <- c('Country','ISO', 'Elevation2010')

sapply(WB_Elevation, class) #check column class
#Transform 1 column into numeric
WB_Elevation$`Elevation2010`<-as.numeric(WB_Elevation$`Elevation2010`)
#View(WB_Elevation)

# Check for NA at index variables
variables <- c(WB_Elevation[,1:2])
sapply(WB_Elevation, function(variables) sum(is.na(variables)))

######*********Clean up WB_PercForest*********######
WB_PercForestWide <- WB_PercForest[,-c(3:52,65)] #drop column 3-52 and 65
WB_PercForestWide <- WB_PercForestWide[-c(1:3),] #delete the first 3 rows

#rename variables
colnames(WB_PercForestWide) <- c('Country','ISO', '2008','2009','2010','2011', '2012',
                             '2013', '2014', '2015','2016', '2017', '2018', '2019')

sapply(WB_PercForestWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016',
              '2017','2018','2019')
WB_PercForestWide[cols.num] <- sapply(WB_PercForestWide[cols.num],as.numeric)
#View(WB_PercForestWide)

#convert wide panel data to long panel data
Year <- "ISO"
WB_PercForestLong <- gather(WB_PercForestWide, Year, PercForest, all_of(cols.num))
View(WB_PercForestLong)

# Check for NA at index variables
variables <- c(WB_PercForestLong[,1:2])
sapply(WB_PercForestLong, function(variables) sum(is.na(variables)))

######*********Clean up WB_PopDensity*********######
WB_PopDensityWide <- WB_PopDensity[,-c(3:52,65)] #drop column 3-52 and 65
WB_PopDensityWide <- WB_PopDensityWide[-c(1:3),] #delete the first 3 rows

#rename variables
colnames(WB_PopDensityWide) <- c('Country','ISO', '2008','2009','2010','2011', '2012',
                             '2013', '2014', '2015','2016', '2017', '2018', '2019')

sapply(WB_PopDensityWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016',
              '2017','2018','2019')
WB_PopDensityWide[cols.num] <- sapply(WB_PopDensityWide[cols.num],as.numeric)
View(WB_PopDensityWide)

#convert wide panel data to long panel data
Year <- "ISO"
WB_PopDensityLong <- gather(WB_PopDensityWide, Year, PopDensity, all_of(cols.num))
#View(WB_PopDensityLong)

# Check for NA at index variables
variables <- c(WB_PopDensityLong[,1:2])
sapply(WB_PopDensityLong, function(variables) sum(is.na(variables)))

######*********Clean up WB_Poverty*********######
WB_PovertyWide <- WB_Poverty[,-c(3:52,65)] #drop column 3-52 and 65
WB_PovertyWide <- WB_PovertyWide[-c(1:3),] #delete the first 3 rows

#rename variables
colnames(WB_PovertyWide) <- c('Country','ISO', '2008','2009','2010','2011', '2012',
                          '2013', '2014', '2015','2016', '2017', '2018', '2019')

sapply(WB_PovertyWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016',
              '2017','2018','2019')
WB_PovertyWide[cols.num] <- sapply(WB_PovertyWide[cols.num],as.numeric)
#View(WB_PovertyWide)

#convert wide panel data to long panel data
Year <- "ISO"
WB_PovertyLong <- gather(WB_PovertyWide, Year, Poverty, all_of(cols.num))
#View(WB_PovertyLong)

# Check for NA at index variables
variables <- c(WB_PovertyLong[,1:2])
sapply(WB_PovertyLong, function(variables) sum(is.na(variables)))

######*********Clean up WB_PercAgri*********######
WB_PercAgriWide <- WB_PercAgri[,-c(3:52,64:65)] #drop column 3-52 and 65
WB_PercAgriWide <- WB_PercAgriWide[-c(1:3),] #delete the first 3 rows

#rename variables
colnames(WB_PercAgriWide) <- c('Country','ISO', '2008','2009','2010','2011', '2012',
                           '2013', '2014', '2015','2016', '2017', '2018')

sapply(WB_PercAgriWide, class) #check column class
#Transform columns to numeric
cols.num <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016',
              '2017','2018')
WB_PercAgriWide[cols.num] <- sapply(WB_PercAgriWide[cols.num],as.numeric)
#View(WB_PercAgriWide)

#convert wide panel data to long panel data
Year <- "ISO"
WB_PercAgriLong <- gather(WB_PercAgriWide, Year, PercAgri, all_of(cols.num))
#View(WB_PercAgriLong)

# Check for NA at index variables
variables <- c(WB_PercAgriLong[,1:2])
sapply(WB_PercAgriLong, function(variables) sum(is.na(variables)))

######*********Clean up WB_Income*********######
WB_Income <- WB_Income[,-c(4:5)] #drop colxumn 4 & 5
colnames(WB_Income)[1]<-"ISO"
View(WB_Income)

######*********MERGE DATAFRAMES*********######
#install.packages("tidyverse")
#install.packages("dplyr")
library("dplyr")
library("tidyverse")

DataList = list(MigRateLong, CRIDataLong, ExposureLong,VulnerabilityLong,
                SusceptibilityLong, AdaptLong, CopeLong, WB_LandLong, WB_EmpAgriLong,
                WB_GDPCapitaLong, WB_PercForestLong, WB_PopDensityLong,
                WB_PovertyLong, WB_PercAgriLong)
#omit Poverty & Precipitation data because it's not longitudinal

ThesisData <- DataList %>% reduce(full_join, by = c("ISO","Year"))
#View(ThesisData)
DataList1 = list(ThesisData,WB_Income)
ThesisData <- DataList1 %>% reduce(full_join, by = "ISO")

#delete columns that has "Country"
ThesisData <- ThesisData %>% select(-contains("Country"))

#filter out regional xcodes in World Bank Data
ThesisData <- ThesisData %>% 
   filter(!ISO %in% c("WLD","UMC","TSS","TSA","TMN","TLA","TEC","TEA",
                        "SST","SSF","SSA","PST","PSS","PRE","OSS","OED",
                        "NAC","MNA","MIC","MEA","LTE","LMY","LMC","LIC",
                        "LDC","LCN","LAC","INX","IDX","IDB","IDA","IBT",
                        "IBD","HPC","HIC","FCS","EUU","EMU","ECS","ECA",
                        "EAS","EAR","EAP","CSS","CHI","CEB","CAF","ARB",
                        "AFW","AFE"))
summary(ThesisData)

?filter()

summary(ThesisData)

View(ThesisData)
#View(ExposureLong)
#View(VulnerabilityLong)
#View(SusceptibilityLong)
#View(AdaptLong)
#View(CopeLong)

#clean NA values in columns that run regression (Migration,CRI,WRI)
#ThesisData_Reg <- ThesisData[complete.cases(ThesisData[ , 3:5]),]
#ThesisData_Reg <- ThesisData_Reg[,-c(6:16)]

#delete all NA values
ThesisData_Clean <- na.omit(ThesisData)
#View(ThesisData_Clean)
#View(ThesisData_Reg)

#Duplicate
#duplicated(ThesisData)
#sum(duplicated(ThesisData))
#ThesisData_Reg[!duplicated(ThesisData_Reg),]
#length(unique(ThesisData$ISO))
#length(unique(ThesisData$Year))

install.packages("plm")
library(tidyverse)
library(dplyr)
library(plm) # to remove note about dependencies
#is.pbalanced(ThesisData_Reg) # are the data balanced?
is.pbalanced(ThesisData)


ThesisData$unique_id <- paste(ThesisData$ISO,ThesisData$Year) # concatenate to make unique ID
ThesisData$duplicate = duplicated(ThesisData$unique_id) # generate the duplicate variable
subset(ThesisData, duplicate=="TRUE") # find the duplicate

#Duplicate identified:
#AFG 2011, AFG 2012, AFG 2013, AFG 2014, AFG 2015, AFG 2016, AFG 2017, AFG 2018,
#AFG 2019

#Delete duplicate
ThesisData <- ThesisData[!duplicated(ThesisData[c("ISO","Year")]),]
View(ThesisData)

######*********IMPUTING MISSING VALUES*********######
#DID NOT END UP USING THIS DATASET
variables <- c("MigRate","CRI","Exposure","Vulnerability","Susceptibility",
               "Adapt","Cope")

ThesisData[is.na(c(ThesisData$MigRate & ThesisData$Exposure), ]  

data_count_1 <- aggregate(data = ThesisData,                # Applying aggregate
                          x ~ ISO,
                          function(x) length(unique(x)))
data_count_1                                          # Print counts

library("dplyr")                                      # Load dplyr
data_count_2 <- ThesisData %>%                              # Applying group_by & summarise
   group_by(ISO) %>%
   summarise(count = n_distinct(x))
data_count_2                                          # Print counts


library("tidyverse")
NA_values <- ThesisData %>% 
   filter(is.na(MigRate)) %>% 
   filter(is.na(CRI)) %>% 
   filter(is.na(Exposure)) %>% 
   filter(grepl("RTB",unique_id))

# step 1: group by Country with NA
NAcountsByCountry <- summarise(NA_values_by_country, n_individuals = n())
View(NA_values_by_country)
# step 2: summarize the number of Country NA values of each using the new df
NAcountsByCountry <- summarise(NA_values_by_country, n_individuals = n())
View(NAcountsByCountry)
ThesisData_1 <- merge(ThesisData, NAcountsByCountry, by="ISO")
View(ThesisData_1)
summary(ThesisData_1)

# Countries that has all 12 years of missing values
CountriesToDelete <- subset(ThesisData_1, n_individuals>11)
View(CountriesToDelete)
NAcountsByCountry_ToDelete <- subset(NAcountsByCountry, n_individuals>11)
View(NAcountsByCountry_ToDelete)
library("writexl")
write_xlsx(NAcountsByCountry_ToDelete,"/Users/vymai/Documents/ECON Senior Sem R/Thesis Code/NAcountsByCountry_ToDelete.xlsx")

# Delete countries that has 12 years of missing values
ThesisData_2 <- subset(ThesisData_1, n_individuals<12)
View(ThesisData_2)

NAcountsByCountry_1 <- subset(NAcountsByCountry, n_individuals<12)
View(NAcountsByCountry_1)

## MI PACKAGES
#install.packages("mi")
library(mi)
summary(ThesisData_2)

MI_ThesisData <- mi(ThesisData_2)
?mi
summary(MI_ThesisData)
View(MI_ThesisData)
#DID NOT WORK

#Impute with mean by country
require(dplyr)
ThesisData <- ThesisData %>% group_by(ISO) %>%
   mutate(MigRate=ifelse(is.na(MigRate),mean(MigRate,na.rm=TRUE),MigRate)) %>%
   mutate(CRI=ifelse(is.na(CRI),mean(CRI,na.rm=TRUE),CRI)) %>%
   mutate(Exposure=ifelse(is.na(Exposure),mean(Exposure,na.rm=TRUE),Exposure)) %>%
   mutate(Vulnerability=ifelse(is.na(Vulnerability),mean(Vulnerability,na.rm=TRUE),Vulnerability)) %>%
   mutate(Susceptibility=ifelse(is.na(Susceptibility),mean(Susceptibility,na.rm=TRUE),Susceptibility)) %>%
   mutate(Adapt=ifelse(is.na(Adapt),mean(Adapt,na.rm=TRUE),Adapt)) %>%
   mutate(Cope=ifelse(is.na(Cope),mean(Cope,na.rm=TRUE),Cope)) %>%
   mutate(Land=ifelse(is.na(Land),mean(Land,na.rm=TRUE),Land)) %>%
   mutate(EmpAgri=ifelse(is.na(EmpAgri),mean(EmpAgri,na.rm=TRUE),EmpAgri)) %>%
   mutate(GDPCapita=ifelse(is.na(GDPCapita),mean(GDPCapita,na.rm=TRUE),GDPCapita)) %>%
   mutate(PercForest=ifelse(is.na(PercForest),mean(PercForest,na.rm=TRUE),PercForest)) %>%
   mutate(PopDensity=ifelse(is.na(PopDensity),mean(PopDensity,na.rm=TRUE),PopDensity)) %>%
   mutate(PercAgri=ifelse(is.na(PercAgri),mean(PercAgri,na.rm=TRUE),PercAgri))
View(ThesisData)

######*********SUMMARY STATS*********######

describe(ThesisData$MigRate)
describe(ThesisData$Exposure)
describe(ThesisData$MigRate)
hist(ThesisData$MigRate)
sd(ThesisData$MigRate, na.rm=TRUE)
sd(ThesisData$CRI, na.rm=TRUE)
sd(ThesisData$Exposure, na.rm=TRUE)

#Calculate modes of variables
Mode <- function(x, na.rm = TRUE) {
   if(na.rm){
      x = x[!is.na(x)]
   }
   
   ux <- unique(x)
   return(ux[which.max(tabulate(match(x, ux)))])
}

Mode(ThesisData$MigRate)
Mode(ThesisData$CRI)
Mode(ThesisData$Exposure)

install.packages("modeest")
library(modeest)
mlv(ThesisData$MigRate, method = "mfv", na.rm = )

##investigate the dataframe 
library(psych)
describe(ThesisData)
summary(ThesisData, na.rm=TRUE)

######*********DESCRIPTIVE ANALYSIS*********######
library(foreign)
install.packages("panelr")
install.packages("skimr")
library(panelr)
library(skimr)

sapply(ThesisData, class) #check column class
ThesisData$Year <- as.numeric(ThesisData$Year) #Transform columns Year numeric

thesisdata <- panel_data(ThesisData, id = ISO, wave = Year)

line_plot(Thesis_Data, Exposure)
line_plot(Thesis_Data, Exposure, overlay = TRUE,
          subset.ids = filter(thesisdata, IncomeGroup == "Low income")$ISO, add.mean = TRUE)

line_plot(thesisdata, Exposure, overlay = TRUE,
          subset.ids = filter(thesisdata, IncomeGroup == "High income")$ISO, add.mean = TRUE)

line_plot(thesisdata, Exposure, overlay = FALSE,
          subset.ids = filter(thesisdata, IncomeGroup == "Low income")$ISO,
          add.mean = TRUE, mean.function = "loess")

dev.off()

library(dplyr)

# Plot
variables %>%
   ggplot( aes(x=Year, y=MigRate, group=name, color=name)) +
   geom_line()

ggplot(ThesisData, mapping = aes(x = Year, y = MigRate)) +
   geom_line(aes(linetype = as.factor(IncomeGroup)))

ggplot(ThesisData, mapping = aes(x = Year, y = MigRate)) +
   geom_line(aes(linetype = as.factor(Region)))

ggplot(ThesisData, mapping = aes(x = Year, y = Exposure, group=IncomeGroup)) +
   geom_line()

don <- babynames %>% 
   filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
   filter(sex=="F")


#Mean Migration Rate to Exposure by Income Group and Region
id <- as_data_frame(ThesisData)
id1 <- as_tibble(ThesisData)
#Income Group
gd1 <- id1 %>% 
   group_by(IncomeGroup) %>% 
   summarise(MigRate = mean(MigRate),
             Exposure = mean(Exposure))

ggplot(id1, aes(x = MigRate, y = Exposure, color = IncomeGroup, shape = IncomeGroup)) +
   geom_point(alpha = .4) +
   geom_point(data = gd1, size = 4) +
   theme_bw() +
   guides(color = guide_legend("IncomeGroup"),  shape = guide_legend("IncomeGroup")) +
   labs(
      title = "Migration Rate to Exposure to Extreme weather events by Income Group",
      x = "Migration Rate",
      y = "Exposure to Extreme weather events"
   )

#Region
gd2 <- id %>% 
   group_by(Region) %>% 
   summarise(MigRate = mean(MigRate),
             Exposure = mean(Exposure))

ggplot(id, aes(x = MigRate, y = Exposure, color = Region, shape = Region)) +
   geom_point(alpha = .4) +
   geom_point(data = gd2, size = 4) +
   theme_bw() +
   guides(color = guide_legend("Region"),  shape = guide_legend("Region")) +
   labs(
      title = "Migration Rate to Exposure to Extreme weather events by Region",
      x = "Migration Rate",
      y = "Exposure to Extreme weather events"
   )

# Exposure by Region over the years
p1 <- ggplot(id, aes(x = Year, y = Exposure, group = ISO, color = Region)) +
   geom_line()
p1 + scale_y_continuous(name="Exposure", limits=c(0, 100)) +
    scale_x_continuous(name="Year", limits=c(2009, 2020))

# Exposure by Income Group over the years
p1 <- ggplot(id, aes(x = Year, y = Exposure, group = ISO, color = IncomeGroup)) +
   geom_line()
p1 
+ scale_y_continuous(name="Exposure", limits=c(0, 100)) +
   scale_x_continuous(name="Year", limits=c(2009, 2020))

# MigRate by Region over the years
p1 <- ggplot(id, aes(x = Year, y = MigRate, group = ISO, color = Region)) +
   geom_line()
p1 + scale_y_continuous(name="MigRate", limits=c(0, 50)) +
   scale_x_continuous(name="Year", limits=c(2009, 2020))

# MigRate by Income Group over the years
p1 <- ggplot(id, aes(x = Year, y = MigRate, group = ISO, color = IncomeGroup)) +
   geom_line()
p1 + scale_y_continuous(name="MigRate", limits=c(0, 50)) +
   scale_x_continuous(name="Year", limits=c(2009, 2020))

# Vulnerability by Income Group over the years
p1 <- ggplot(id, aes(y = Vulnerability, x = Year, group = ISO, color = IncomeGroup)) +
   geom_line()
p1 + scale_y_continuous(name="Vulnerability", limits=factor(0, 75)) +
   scale_x_discrete(name="Year", limits=factor(2009, 2020))

# Susceptibility by Income Group over the years
p1 <- ggplot(id, aes(x = Year, y = Susceptibility, group = ISO, color = IncomeGroup)) +
   geom_line()
p1 + scale_y_continuous(name="Susceptibility", limits=c(0, 150)) +
   scale_x_continuous(name="Year", limits=c(2009, 2020))

# Adapt by Income Group over the years
p1 <- ggplot(id, aes(x = Year, y = Adapt, group = ISO, color = IncomeGroup)) +
   geom_line()
p1 + scale_y_continuous(name="Adapt", limits=c(0, 75)) +
   scale_x_continuous(name="Year", limits=c(2009, 2020))

# Cope by Income Group over the years
p1 <- ggplot(id, aes(x = Year, y = Cope, group = ISO, color = IncomeGroup)) +
   geom_line()
p1 + scale_y_continuous(name="Cope", limits=c(0, 100)) +
   scale_x_continuous(name="Year", limits=c(2009, 2020))

# Average land area over the years
mean_land <- group_by(ThesisData, Land, Year) %>% 
   summarise(Land = mean(Land, na.rm = TRUE))

library(ggplot2) 
ggplot(na.omit(mean_land), aes(x = Year, y = Land)) + 
   geom_point() + 
   geom_line()

ggplot(data = ThesisData, aes(y = Land, x = Year, group = 1)) + 
   stat_summary(geom = "line", fun= mean)

ggplot(data = ThesisData, aes(y = PopDensity, x = Year, group = 1)) + 
   stat_summary(geom = "line", fun= mean)

ggplot(data = ThesisData, aes(y = MigData, x = Year, group = 1)) + 
   stat_summary(geom = "line", fun= mean)

ggplot(ThesisData, aes(Year, EmpAgri, group = 1)) +
   stat_summary(geom = "line", fun.y= mean) +
   labs(x = "Year", y = "Employment", 
        title = "Motor vehicle emissions in Baltimore")

ggplot(ThesisData, aes(x=IncomeGroup, y=MigRate, fill=IncomeGroup, na.rm = TRUE)) +
   geom_boxplot(varwidth=TRUE, alpha=0.3, na.rm = TRUE)+
   scale_fill_discrete(name = "Legend", labels = c("high income", "low income", "lower middle income", "upper middle income"))+
   stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")

ggplot(ThesisData, aes(x=IncomeGroup, y=CRI, fill=IncomeGroup, na.rm = TRUE)) +
   geom_boxplot(varwidth=TRUE, alpha=0.3, na.rm = TRUE)+
   scale_fill_discrete(name = "Legend", labels = c("high income", "low income", "lower middle income", "upper middle income"))+
   stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")

ggplot(ThesisData, aes(x=IncomeGroup, y=Exposure, fill=IncomeGroup, na.rm = TRUE)) +
   geom_boxplot(varwidth=TRUE, alpha=0.3, na.rm = TRUE)+
   scale_fill_discrete(name = "Legend", labels = c("high income", "low income", "lower middle income", "upper middle income"))+
   stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")

ggplot(ThesisData, aes(x=Region, y=MigRate, fill=Region, na.rm = TRUE)) +
   geom_boxplot(varwidth=TRUE, alpha=0.3, na.rm = TRUE)+
   scale_fill_discrete(name = "Legend", labels = c("high income", "low income", "lower middle income", "upper middle income"))+
   stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")

ggplot(ThesisData, aes(x=Region, y=CRI, fill=Region, na.rm = TRUE)) +
   geom_boxplot(varwidth=TRUE, alpha=0.3, na.rm = TRUE)+
   scale_fill_discrete(name = "Legend", labels = c("high income", "low income", "lower middle income", "upper middle income"))+
   stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")

ggplot(ThesisData, aes(x=Region, y=Exposure, fill=Region, na.rm = TRUE)) +
   geom_boxplot(varwidth=TRUE, alpha=0.3, na.rm = TRUE)+
   ggtitle("Boxplots of Exposure Rate grouped by Country Regions")+
   scale_fill_discrete(name = "Legend", labels = c("East Asia & Pacific", "Europe & Central Asia",
                                                   "Latin America & Caribbean", "Middle East & North Africa",
                                                   "South Asia", "Sub-Saharan Africa"))+
   stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")

coplot(CRI ~ Year|ISO, type="l", data=ThesisData) # Lines
coplot(MigRate ~ Year|ISO, type="b", data=ThesisData) # Points and lines

describe(ThesisData$Exposure)
hist(ThesisData$EmpAgri,
     xlab="Number of people exposed to extreme weather events",
     main="Histogram of ")
hist(ThesisData$PercAgri,
     xlab="Number of people exposed to extreme weather events",
     main="Histogram of ")
hist(ThesisData$PercForest,
     xlab="Number of people exposed to extreme weather events",
     main="Histogram of ")
hist(ThesisData$Susceptibility,
     xlab="Number of people exposed to extreme weather events",
     main="Histogram of ")
hist(ThesisData$Vulnerability,
     xlab="Number of people exposed to extreme weather events",
     main="Histogram of ")
hist(ThesisData$Adapt,
     xlab="Number of people exposed to extreme weather events",
     main="Histogram of ")
hist(ThesisData$Cope,
     xlab="Number of people exposed to extreme weather events",
     main="Histogram of ")

hist(ThesisData$Land,
     xlab="Number of people exposed to extreme weather events",
     main="Histogram of ")

which.min(ThesisData$Land)

## Distribution of MigRate
ggplot(ThesisData, aes(x=MigRate)) + 
   geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                  binwidth=.5,
                  colour="black", fill="white") +
   geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

## Distribution of CRI
ggplot(ThesisData_3, aes(x=CRI)) + 
   geom_histogram(aes(y=..density..),
                  binwidth=.5,
                  colour="black", fill="white") +
   geom_density(alpha=.2, fill="#FF6666")

## Distribution of Exposure
ggplot(ThesisData_3, aes(x=Exposure)) + 
   geom_histogram(aes(y=..density..),
                  binwidth=.5,
                  colour="black", fill="white") +
   geom_density(alpha=.2, fill="#FF6666")

# qplot(dat$rating, binwidth=.5)

# Draw with black outline, white fill
ggplot(ThesisData_3, aes(x=Exposure, fill=cond)) +
   geom_histogram(binwidth=.5, alpha=.5, position="identity", colour="black", fill="white")

# Density curve
ggplot(dat, aes(x=rating)) + geom_density()

# Histogram overlaid with kernel density curve
ggplot(dat, aes(x=rating)) + 
   geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                  binwidth=.5,
                  colour="black", fill="white") +
   geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

install.packages("car")
library(car)
scatterplot(MigRate ~ Year|ISO, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=ThesisData)

#install.packages("gplots")
library(gplots)
library(ggplot2)
plotmeans(MigRate ~ ISO, main="Heterogeineity across countries", data=ThesisData)
plotmeans(MigRate ~ Year, main="Heterogeineity across years", data=ThesisData)

install.packages("finalfit")
library(finalfit)
library(dplyr)

#mean of data with specific criteria
mean(ThesisData$MigRate[ThesisData$Year=="2008"],na.rm=TRUE)
mean(ThesisData$MigRate[ThesisData_2$IncomeGroup=="Upper middle income"], na.rm=FALSE)
mean(ThesisData$MigRate[ThesisData_2$IncomeGroup=="Lower middle income"])
mean(ThesisData$MigRate[ThesisData_2$IncomeGroup=="Low income"])

mean(ThesisData$MigRate[ThesisData_2$Region=="Sub-Saharan Africa"])
mean(ThesisData$MigRate[ThesisData_2$Region=="Middle East & North Africa"])
mean(ThesisData$MigRate[ThesisData_2$Region=="East Asia & Pacific"])
mean(ThesisData$MigRate[ThesisData_2$Region=="Europe & Central Asia"])
mean(ThesisData$MigRate[ThesisData_2$Region=="South Asia"])
mean(ThesisData$MigRate[ThesisData_2$Region=="Latin America & Caribbean"])
mean(ThesisData$MigRate[ThesisData_2$Region=="North America"])

##VIEW DATA AND PARTIAL SUMMARY
#make a vector and pass it into a square bracket so R knows you want 4 columns
View(ThesisData_2[c("ISO","Migration","AvgCRI","AvgWRI","Precipitation2017")])
#make a new variable then descrie it too works for summarizing it as cant do mean
variables <- c("ISO","MigRate","CRI","Exposure")
describe(ThesisData_3[variables])

######*********DATA VISUALIZATION*********######

#HISTOGRAM: xlab for x axis, ylab for y axis, main for title 
hist(ThesisData$MigRate, xlab="Number of Displaced people",
     ylab="Frequency",
     main="Histogram of Internally Displaced People")

hist(ThesisData$CRI, xlab="Climate Risk Index Score",
     ylab="Frequency",
     main="Histogram of Climate Risk Index Score")

hist(ThesisData$Exposure, xlab="Average World Risk Index Score",
     ylab="Frequency",
     main="Histogram of Average World Risk Index Score")

hist(ThesisData$Precipitation2017, xlab="Preciptitation in 2017",
     ylab="Frequency",
     main="Histogram of Preciptitation in 2017")

hist(ThesisData$Avg_Land, xlab="Average percentage of land area 2008-2019",
     ylab="Frequency",
     main="Histogram of Average percentage of land area 2008-2019")

hist(ThesisData$Avg_EmpAgri,
     xlab="Average percentage of Employment in Agriculture 2008-2019",
     ylab="Frequency",
     main="Histogram of Average Percentage of Employment in Agriculture 2008-2019")

hist(ThesisData$Avg_GDPCapita, xlab="Average GDP per Capita 2008-2019",
     ylab="Frequency",
     main="Histogram of Average GDP per Capita 2008-2019")

hist(ThesisData$Avg_PercForest,
     xlab="Average percentage of Forest Land 2008-2019",
     ylab="Frequency",
     main="Histogram of Average percentage of Forest Land 2008-2019")

hist(ThesisData$Avg_PopDensity, xlab="Average Population Density 2008-2019",
     ylab="Frequency",
     main="Histogram of Average Population Density 2008-2019")

hist(ThesisData$Avg_PercAgri,
     xlab="Average percentage of Agriculture Land 2008-2018",
     ylab="Frequency",
     main="Histogram of Average percentage of Agriculture Land 2008-2018")

#BOXPLOT
##ggplot2
ggplot(ThesisData, aes(x=IncomeGroup, y=MigRate, na.rm = TRUE)) + 
   geom_boxplot()

ggplot(ThesisData_2, aes(x=IncomeGroup, y=Exposure)) + 
   geom_boxplot()
ggplot(ThesisData_2, aes(x=IncomeGroup, y=CRI)) + 
   geom_boxplot()
ggplot(ThesisData_2, aes(x=Region, y=MigRate)) + 
   geom_boxplot()
ggplot(ThesisData_2, aes(x=Region, y=Exposure)) + 
   geom_boxplot()
ggplot(ThesisData_2, aes(x=Region, y=CRI)) + 
   geom_boxplot()

boxplot(ThesisData$Land, 
        main="Land")

boxplot(ThesisData$Land)$out 

boxplot(ThesisData$EmpAgri,
        main="EmpAgri")

boxplot(ThesisData$PercAgri,
        main="PercAgri")

boxplot(ThesisData$PopDensity,
        main="PopDensity")

boxplot(ThesisData$PopDensity)$out 

boxplot(ThesisData$PercForest,
        main="PercForest")

boxplot(ThesisData$GDPCapita,
        main="GDPCapita")

boxplot(ThesisData$GDPCapita)$out 

boxplot(ThesisData$Susceptibility,
        main="Susceptibility")

boxplot(ThesisData$Adapt,
        main="Adapt")

boxplot(ThesisData$Vulnerability,
        main="Vulnerability")

boxplot(ThesisData$Cope,
        main="Cope")

boxplot(ThesisData$Cope)$out 

#Examine distribution of countries by regions
tb<-table(ThesisData$Region, ThesisData$IncomeGroup) #frequency table
tb 
prop.table(tb) #frequency in %
install.packages("epiDisplay")
install.packages("magrittr")
library(epiDisplay)
library(magrittr)
tb %$% cc(ThesisData$Region, ThesisData$IncomeGroup, graph = F)
prop.table(table(ThesisData$Region)) #frequency in %
prop.table(table(ThesisData$IncomeGroup)) #frequency in %


#Examine distribution of countries by income group
table(ThesisData$IncomeGroup) #frequency table
prop.table(table(ThesisData$IncomeGroup)) #frequency in %

##SCATTERPLOT
# scatterplot
install.packages("ggplot2")
library(ggplot2)
ggplot(ThesisData) + aes(x = Exposure, y = MigRate) + 
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = CRI, y = MigRate) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Precipitation2017, y = Migration) +
   geom_point(colour = "#0c4c8a") + theme_minimal()

ggplot(ThesisData) + aes(x = Avg_Land, y = AvgCRI) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Avg_EmpAgri, y = AvgCRI) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Avg_GDPCapita, y = AvgCRI) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Avg_PercForest, y = AvgCRI) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Avg_PopDensity, y = AvgCRI) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Avg_PercAgri, y = AvgCRI) +
   geom_point(colour = "#0c4c8a") + theme_minimal()

ggplot(ThesisData) + aes(x = Vulnerability, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Susceptibility, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Adapt, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Cope, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Land, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = EmpAgri, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = GDPCapita, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = PercForest, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = PopDensity, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = Poverty, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()
ggplot(ThesisData) + aes(x = PercAgri, y = Exposure) +
   geom_point(colour = "#0c4c8a") + theme_minimal()

##CORRELATION
#Correlation for all variables
round(cor(ThesisData_2[,variables]),
      digits = 5)

#Visualize correlation
install.packages('corrplot')
library(corrplot)
variables <- c("MigRate","CRI","Exposure","Vulnerability","Susceptibility",
               "Adapt","Cope","Land","EmpAgri","GDPCapita","PercForest",
               "PopDensity","Poverty","PercAgri")
corrplot(cor(ThesisData_2[,variables], use="complete.obs"),
         method="color", order ="AOE", addCoef.col="grey",
         number.cex=0.5, cl.cex=0.5, tl.cex=0.75)

#Correlation tests for whole dataset
install.packages("Hmisc")
library(Hmisc)
res <- rcorr(as.matrix(ThesisData_3[,variables])) # rcorr() accepts matrices only
#Display p-values (rounded to 3 decimals)
round(res$P, 3)

install.packages("GGally")
library(GGally)
ggpairs(ThesisData[,c("MigRate","CRI","Exposure")])
?ggpairs

######*********OLS REGRESSION*********######
ols1_dummy = lm(MigRate ~ Exposure + factor(Year) + factor(ISO), data=ThesisData)
summary(ols1_dummy)

ols1 <-lm(MigRate ~ CRI + Exposure + Susceptibility + Adapt
          + Cope + Land + EmpAgri + GDPCapita + PercForest + PopDensity 
          + PercAgri, data=ThesisData)
summary(ols1,type="hc0")

ols2 <-lm(MigRate ~ Exposure, data=ThesisData_2)
summary(ols2)

ols3 <-lm(MigRate ~ Vulnerability, data=ThesisData)
summary(ols3)

ols4 <-lm(MigRate ~ Susceptibility, data=ThesisData)
summary(ols4)

ols5 <-lm(MigRate ~ Adapt, data=ThesisData)
summary(ols5)

ols6 <-lm(MigRate ~ Cope, data=ThesisData)
summary(ols6)

yhat1 <- ols1$fitted
plot(ThesisData_2$CRI, ThesisData_2$MigRate, pch=19, xlab="Climate Risk Index", 
     ylab="Migration Rate")
abline(lm(ThesisData_2$MigRate~ThesisData_2$CRI),lwd=3, col="red")

yhat2 <- ols2$fitted
plot(ThesisData_2$Exposure, ThesisData_2$MigRate, pch=19, xlab="Exposure", 
     ylab="Migration Rate")
abline(lm(ThesisData_2$MigRate~ThesisData_2$Exposure),lwd=3, col="red")

stargazer(ols1, title="OLS Regression Results", align=TRUE, type="text", out="OLS_Reg_Results.html")

stargazer(ols1, title="OLS Regression Results", out="OLS_Reg_Results.html",
          dep.var.labels="Migration Rate")
#Regular OLS regression does not consider heterogeneity across groups or time

######*********LEAST SQUARE DUMMY VARIABLE REGRESSION*********#####
fixed.dum <- lm(MigRate ~ Exposure + factor(ISO) - 1, data=ThesisData)
summary(fixed.dum)
yhat <- fixed.dum$fitted
scatterplot(yhat ~ ThesisData_2$Exposure | ThesisData_2$ISO, boxplots=FALSE,
            xlab="Exposure to Weather events", ylab=yhat, smooth=FALSE)
abline(lm(ThesisData$Exposure),lwd=3,col="red")
######*********FIXED EFFECTS REGRESSION*********######
#install.packages("plm")
library(plm)
library(stats)

#country fixed effects
country_fixed <- plm(MigRate ~ Exposure + factor(Year), data=ThesisData, index="ISO", 
                     model="within",
                     na.action=na.exclude)
summary(country_fixed)

fixed1 <- plm(MigRate ~ CRI + Exposure + Susceptibility + Adapt
              + Cope + Land + EmpAgri + GDPCapita + PercForest + PopDensity 
              + PercAgri + factor(ISO), data=ThesisData, index=c("ISO", "Year"), model="within")
summary(fixed1)
coeftest(fixed1, vcovHC(fixed1, method = "arellano")) 

fixed12 <- plm(MigRate ~ CRI + Susceptibility + Adapt
               + Cope + Land + EmpAgri + GDPCapita + PercForest + PopDensity
               + PercAgri, data=ThesisData, index=c("ISO", "Year"), model="within",
              useNA = "ifany")
summary(fixed12)

fixed13 <- plm(MigRate ~ Exposure + Susceptibility + Adapt
              + Cope + Land + EmpAgri + GDPCapita + PercForest + PopDensity
              + PercAgri, data=ThesisData_2, index=c("ISO", "Year"), model="within",
              useNA = "ifany")
summary(fixed3)

fixed4 <- plm(MigRate ~ CRI*IncomeGroup + Exposure*IncomeGroup + 
                 Susceptibility + Adapt + Cope + EmpAgri + GDPCapita + PercForest + PopDensity 
              + PercAgri, data=ThesisData_2, 
              index=c("ISO", "Year"), model="random", useNA = "ifany")
summary(fixed4)

fixed5 <- plm(MigRate ~ CRI*Region + Exposure*Region + 
                 Susceptibility + Adapt + Cope + EmpAgri + GDPCapita + PercForest + PopDensity 
              + PercAgri, data=ThesisData_2, index=c("ISO", "Year"), model="random",
              useNA = "ifany")
summary(fixed5)

# create a nice table to present regression results and compare across models
install.packages("stargazer")
library(stargazer)
stargazer(fixed1, fixed2, title="Fixed Effects Regression Results", align=TRUE, 
          type="text", out="Reg_Results.txt")

stargazer(fixed1, fixed2, title="Fixed Effects Regression Results", out="FE_Reg_Results.txt",
          dep.var.labels="Migration Rate")

######*********RANDOM EFFECTS REGRESSION*********######
library(lmtest)
random1 <- plm(MigRate ~ CRI + Exposure + Susceptibility + Adapt
               + Cope + Land + EmpAgri + GDPCapita + PercForest + PopDensity
               + PercAgri , data=ThesisData, index=c("ISO", "Year"), model="random",
               useNA = "ifany")
summary(random1)

coeftest(random1, vcovHC(random1, type = "HC3"))


random12 <- plm(MigRate ~ CRI + Susceptibility + Adapt
               + Cope + Land + EmpAgri + GDPCapita + PercForest + PopDensity
               + PercAgri , data=ThesisData_2, index=c("ISO", "Year"), model="random",
               useNA = "ifany")
summary(random12, type="hc0"))


random13 <- plm(MigRate ~ Exposure + Susceptibility + Adapt
                + Cope + Land + EmpAgri + GDPCapita + PercForest + PopDensity
                + PercAgri , data=ThesisData_2, index=c("ISO", "Year"), model="random",
                useNA = "ifany")
summary(random13, type="hc0")

random2 <- plm(MigRate ~ CRI + Exposure + Susceptibility + Adapt
               + Cope, data=ThesisData_2, index=c("ISO", "Year"), model="random",
               useNA = "ifany")
summary(random2, type="hc0"))

random22 <- plm(MigRate ~ CRI + Susceptibility + Adapt
               + Cope, data=ThesisData_2, index=c("ISO", "Year"), model="random",
               useNA = "ifany")
summary(random22)

random23 <- plm(MigRate ~ Exposure + Susceptibility + Adapt
                + Cope, data=ThesisData_2, index=c("ISO", "Year"), model="random",
                useNA = "ifany")
summary(random23)

random3 <- plm(MigRate ~ CRI + Exposure + Land + EmpAgri + GDPCapita + PercForest + PopDensity 
               + PercAgri, data=ThesisData_2, index=c("ISO", "Year"), model="random",
               useNA = "ifany")
summary(random3)

random32 <- plm(MigRate ~ Exposure + Land + EmpAgri + GDPCapita + PercForest + PopDensity 
               + PercAgri, data=ThesisData_2, index=c("ISO", "Year"), model="random",
               useNA = "ifany")
summary(random32)

random33 <- plm(MigRate ~ CRI + Land + EmpAgri + GDPCapita + PercForest + PopDensity 
                + PercAgri, data=ThesisData_2, index=c("ISO", "Year"), model="random",
                useNA = "ifany")
summary(random33)

random4 <- plm(MigRate ~ CRI*IncomeGroup + Exposure*IncomeGroup + 
               Susceptibility + Adapt + Cope + EmpAgri + GDPCapita + PercForest + PopDensity 
               + PercAgri, data=ThesisData, 
               index=c("ISO", "Year"), model="random", useNA = "ifany")
summary(random4)
coeftest(random4, vcovHC(random4, type = "HC3"))

random5 <- plm(MigRate ~ CRI*Region + Exposure*Region + 
                   Susceptibility + Adapt + Cope + EmpAgri + GDPCapita + PercForest + PopDensity 
                + PercAgri, data=ThesisData, index=c("ISO", "Year"), model="random")
summary(random5)
coeftest(random5, vcovHC(random5, type = "HC3"))


random51 <- plm(MigRate ~ CRI*Region + Exposure + 
               Susceptibility + Adapt + Cope + EmpAgri + GDPCapita + PercForest + PopDensity 
               + PercAgri, data=ThesisData, index=c("ISO", "Year"), model="random")
summary(random51)
coeftest(random51, vcovHC(random51, type = "HC3"))

random52 <- plm(MigRate ~ CRI + Exposure*Region + 
               Susceptibility + Adapt + Cope + EmpAgri + GDPCapita + PercForest + PopDensity 
                + PercAgri, data=ThesisData, index=c("ISO", "Year"), model="random")
summary(random52)

coeftest(random52, vcovHC(random52, type = "HC3"))


stargazer(fixed1, random1, random4, random51, random52, title=" Regression Results", 
          align=TRUE, type="text", out="Reg_Results_RE2.html")
?stargazer

stargazer(fixed1, random1, random4, random51, random52, title="Regression Results", 
          out="Reg_Results_RE2.html", dep.var.labels="Migration Rate")
install.packages("textreg")
library(textreg)
?textreg


######*********DIAGNOSTIC TESTS*********######
## Hausman test
# tests whether the unique errors are correlated with the regressors, the null hypothesis is they are not.
# If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects.
phtest(fixed1,random1)
phtest(fixed2,random12)
phtest(fixed3,random13)
phtest(fixed4,random4)
phtest(fixed5,random5)
# for all five tests, we use Random Effects

library("lmtest")
library("sandwich")

coeftest(random1, vcov = vcovHC(random1, type = "HC0"))


## Testing for time-fixed effects
# F test for individual effects
fixed1.time <- plm(MigRate ~ CRI + factor(Year), data=ThesisData_2,
                   index=c("ISO","Year"), model="within")
summary(fixed1.time)
pFtest(fixed1.time, fixed1) # The null is that no time-fixed effects needed

fixed2.time <- plm(MigRate ~ Exposure + factor(Year), data=ThesisData_2,
                   index=c("ISO","Year"), model="within")
summary(fixed2.time)
pFtest(fixed2.time, fixed2) # The null is that no time-fixed effects needed

# Lagrange Multiplier Test - time effects (Breusch-Pagan) for unbalanced panels
plmtest(fixed1, c("time"), type=("bp"))
plmtest(fixed2, c("time"), type=("bp"))

# Testing for serial correlation
pbgtest(fixed1)
pbgtest(fixed2)

# Testing for heteroskedasticity 
library(lmtest)
bptest(MigRate ~ CRI + Exposure + factor(ISO), data = ThesisData, studentize=F)

bptest(MigRate ~ CRI + Exposure + Susceptibility + Adapt
+ Cope + Land + EmpAgri + GDPCapita + PercForest + PopDensity
+ PercAgri, data = ThesisData, studentize=F)

# Testing for fixed effects, null: OLS better than fixed
pFtest(fixed1, ols1)
pFtest(fixed2, ols2)
