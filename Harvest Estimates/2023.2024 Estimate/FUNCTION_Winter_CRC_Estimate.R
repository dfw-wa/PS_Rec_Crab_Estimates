
#####Load Packages
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(eeptools)
library(ggplot2)
library(readxl)
library(patchwork)
library(lubridate)
library(plyr)
library(writexl)

###Run function to summarize catch from winter CRCs

Winter.CRC.Estimate.function <- function(Year=Year, data.name=data.name){

############Bring in data#########
  #Assign directory to global inputs
  global.directory<-paste0(getwd(),"//global_inputs")
  
  #Set the current directory of interest
  current.directory<-paste0(getwd(),"/CRC Data/")
  
  #Bring in data files
  catch.crc=read_excel(paste0(current.directory,data.name))
  
  
############Clean data############
  
  #Optional step: Create new column to define successful trips (at least 1 crab caught) and remove unsuccessful trips (crab=0)
  #This step isn't necessary as crab are being totaled but it cleans the data
  catch=catch.crc
  catch$success<-ifelse(catch$num_crab>0,1,0)
  catch=catch %>% filter(success==1)
  
  ##Set marine area, month, and day as numeric
  catch$area=as.numeric(catch$area)
  catch$month=as.numeric(catch$month)
  catch$day=as.numeric(catch$day)
  
  ####Remove catch from coastal Marine Areas/Columbia River
  unique(catch$area) #Identify Marine Areas in data frame
  #Some Marine Areas are entered as NA or a number that doesn't make sense....set these to 192 (code for an unknown Marine Area)
  catch$area[is.na(catch$area)]=192
  #Select only Puget Sound MA's (including unknown's). If Code 519 is present: Salmon catch code for Columbia R. Buoy 10 to Rocky Pt.-Tongue Pt. line
  catch.crc2=catch %>% filter(area==4|area==5|area==6|area==7|area==8|area==81|area==82|area==9|
                                area==10|area==11|area==12|area==13|area==192)
  
  
  #Some CRC data sets will not have any catch for a specific MA. This will cause errors in the script/functions below. 
  #We need to ensure all MA's are accounted for by applying zero catch to one trip for each MA.
  define.area=c(4,5,6,7,8,81,82,9,10,11,12,13,192)
  
  for (i in 1:13)
  {
    catch.crc2[nrow(catch.crc2) + 1,1] = c(define.area[i])
    catch.crc2[nrow(catch.crc2),5] = c(0)
  }
  
  
  #Some months and days are entered into the data frame as NA instead of 99 (code for an unknown day or month). Set each NA to 99.
  #Also, set entered values for day and month that don't make sense on a calendar to 99.
  unique(catch.crc2$month)
  unique(catch.crc2$day)
  catch.crc2$day[is.na(catch.crc2$day)]=99
  catch.crc2$month[is.na(catch.crc2$month)]=99
  catch.crc2$year[is.na(catch.crc2$year)]=Year
  catch.crc2$day[catch.crc2$day > 31 & catch.crc2$day < 99]=99
  catch.crc2$month[catch.crc2$month > 12 & catch.crc2$month < 99]=99
  catch.crc2$day[catch.crc2$day > 99]=99
  catch.crc2$month[catch.crc2$month > 99]=99
  
  #Create date column for filtering later
  catch.crc2$date=as.Date(with(catch.crc2, paste(year,month,day,sep="-")), "%Y-%m-%d")
  #Create Julian day (day of year) column
  #catch.crc2$doy=yday(catch.crc2$date)
  
 
########Define Labor Day based on Year###############
  #Pull in data frame with Labor Day dates and Year
  LD.dates="LaborDay_Dates.xlsx"
  current.directory2<-paste0(getwd(),"/CRC Data/Labor Day Dates Dataframe/")
  yr.data=read_excel(paste0(current.directory2,LD.dates))
  
  #Define Labor Day date based on year
  LD=yr.data[yr.data$yr.vec==Year,2]
  LD=paste0(Year,"-",LD)
  
  
###########Run loop to summarize catch by date and Marine Area#############
  
  #Pre-define lists and vectors for looping
  area.list=list()
  Pre.LD1=list()
  Pre.LD1.sum=c()
  Pre.LD2=list()
  Pre.LD2.sum=c()
  September=list()
  September.sum=c()
  September.unk=list()
  September.unk.sum=c()
  October=list()
  October.sum=c()
  November=list()
  November.sum=c()
  December=list()
  December.sum=c()
  Unk.date=list()
  Unk.date.sum=c()
  areas=unique(catch.crc2$area)
  
  #Run for loop to summarize catch for Pre-Labor Day, September (Post-Labor Day), October, November, December, and unknown month.
  #Data is filtered for MA and the date. Any crab reported in Sept but unknown day is included in the September Post-Labor Day estimate.
  for (i in 1:length(unique(catch.crc2$area)))
  {
    
    #Filter by area
    area.list[[i]]=catch.crc2 %>% filter(area==areas[i])
    
    Pre.LD1[[i]]=area.list[[i]] %>% filter(date <= LD & date >= paste0(Year,"-09-01"))
    Pre.LD1.sum[i]=sum(Pre.LD1[[i]]$num_crab)
    
    Pre.LD2[[i]]=area.list[[i]] %>% filter(month==8 | month==7 | month==6 | month==5 | month==4 | month==3 | month==2 | month==1)
    Pre.LD2.sum[i]=sum(Pre.LD2[[i]]$num_crab)
    
    September[[i]]=area.list[[i]] %>% filter(date > LD & date <= paste0(Year,"-09-30"))
    September.sum[i]=sum(September[[i]]$num_crab)
    
    September.unk[[i]]=area.list[[i]] %>% filter(month==9 & day==99)
    September.unk.sum[i]=sum(September.unk[[i]]$num_crab) 
    
    October[[i]]=area.list[[i]] %>% filter(month==10)
    October.sum[i]=sum(October[[i]]$num_crab) 
    
    November[[i]]=area.list[[i]] %>% filter(month==11)
    November.sum[i]=sum(November[[i]]$num_crab)
    
    December[[i]]=area.list[[i]] %>% filter(month==12)
    December.sum[i]=sum(December[[i]]$num_crab) 
    
    Unk.date[[i]]=area.list[[i]] %>% filter(month==99)
    Unk.date.sum[i]=sum(Unk.date[[i]]$num_crab) 
    
  }
  
  #Add Summer harvest on winter card (pre-season)
  Summer.catch=c(Pre.LD1.sum+Pre.LD2.sum)
  
  #Add together 'September Post-Labor Day' and 'September w/unknown day'
  September.postLD=c(September.sum+September.unk.sum)
  
#############Create CRC Table 1: Sum catch by each date category and Marine Area##########
  
  Marine.Area=unique(catch.crc2$area) #define marine area column
  Table1=as.data.frame(cbind(Marine.Area,Summer.catch,September.postLD,October.sum,November.sum,December.sum,Unk.date.sum))
  Table1
  
  ###Create a column for total crab and pounds
  Table1$Total.Crabs=rowSums(Table1[,2:7])
  Table1$Lbs.Crab=Table1$Total.Crabs*1.8
  Total=c(colSums(Table1[,2:9]))
  Total=c("Total",Total)
  Table1=as.data.frame(rbind(Table1,Total))
  
  #Ensure all counts of crab are numeric not a character
  library(dplyr)
  Table1[,2:9]=Table1[,2:9] %>%
    mutate_all(as.numeric)
  
  #Order the data by MA: 4-13
  Table1=Table1 %>% arrange(factor(Marine.Area, levels = c('4', '5', '6', '7', '8', '81', '82','9','10','11','12','13','192','Total')))
  
  Table1[,9]=round_any(Table1[,9], 1)
  
  Table1=Table1
  colnames(Table1) <- c("Marine Area", "Summer Catch", "Sept (post Labor Day)", "October", "November", "December", "Unknown Date", "Total Crab", "Pounds (lbs.)")
  
#########Create CRC Table2: Distribute out of season catch into unknown date category############
  
  #Out of season catch = Pre-June, June, Post-labor Day and unknown date into the Unknown column
  Table2=Table1
  Table2$Unknown=Table2$`Summer Catch`+Table2$`Unknown Date`
  Table2=Table2[,-c(2,7)]
  Table2=Table2 %>% relocate(Unknown, .after = December)
  
  
###########Create CRC Table 3: Apportioning unknown date/Marine and MA8 into 8-1 and 8-2###########
  
  ###Apportion:
  #1) Unknown Marine Area/month into known Marine Area/unknown Month
  #2) All catch in unknown month column to known month for each Marine Area
  #3) Catch from Marine Area 8 into 8-1 and 8-2
  
  Table3=Table2
  
  #1) Join catch from unknown MA (192)/known month cells into unknown MA/unknown month cell
  Table3[13,6]=Table3[13,6]+sum(Table3[13,2:5])
  Table3[13,2:5]=0
  
  #1) Find catch proportions for each MA in the Unknown month column
  #Apportion unknown MA/month cell to unknown month for each MA
  unk.prop=Table3$Unknown[1:12]/sum(Table3$Unknown[1:12])
  Table3[1:12,6]=(unk.prop*Table3$Unknown[13])+(Table3$Unknown[1:12])
  #Now remove unknown MA row
  Table3=Table3[-c(13),]
  
  #2) Find catch proportions for open season columns (July, Aug, Sept 1-5) for each marine area
  month.prop=Table3[1:12,2:5]/rowSums(Table3[1:12,2:5])
  #Convert all NAs to zero
  month.prop[is.na(month.prop)]=0
  ##Special case when total catch during known months for a marine area=0 but some unknown catch is present: 
  ##When calculating catch proportions, each month will calculate to 0 (or NA) and cant apportion unknown catch.
  ##If total catch is 0 for known months for a marine area, assign 0.25 as the catch proportion for each month.
  for(i in 1:12){
  if(sum(month.prop[i,1:4])==0){
    month.prop[i,1:4]=0.25}}
  Table3[1:12,2:5]=(month.prop*Table3[1:12,6])+(Table3[1:12,2:5])
  #Now remove unknown date column
  Table3=Table3[,-c(6)]
  
  #Convert all NAs to zero
  Table3[is.na(Table3)]=0
  
  #3) Find catch proportions for 81 and 82
  eight.prop2=Table3$`Sept (post Labor Day)`[6:7]/sum(Table3$`Sept (post Labor Day)`[6:7])
  eight.prop3=Table3$October[6:7]/sum(Table3$October[6:7])
  eight.prop4=Table3$November[6:7]/sum(Table3$November[6:7])
  eight.prop1=Table3$December[6:7]/sum(Table3$December[6:7])
  eight.prop5=as.data.frame(cbind(eight.prop2,eight.prop3, eight.prop4, eight.prop1))
  Table3[6:7,2]=(eight.prop5[,1]*Table3$`Sept (post Labor Day)`[5])+(Table3[6:7,2])
  Table3[6:7,3]=(eight.prop5[,2]*Table3$October[5])+(Table3[6:7,3])
  Table3[6:7,4]=(eight.prop5[,3]*Table3$November[5])+(Table3[6:7,4])
  Table3[6:7,5]=(eight.prop5[,4]*Table3$December[5])+(Table3[6:7,5])
  #Now remove MA8 row
  Table3=Table3[-c(5),]
  
  #Convert all NAs to zero
  Table3[is.na(Table3)]=0
  
  #Update total crabs and pounds
  #Total crab for each MA and calculate pounds
  Table3$`Total Crab`=rowSums(Table3[,2:5])
  Table3$`Pounds (lbs.)`=Table3$`Total Crab`*1.8
  Total=c(colSums(Table3[1:11,2:7]))
  Total=c("Total",Total)
  Table3=as.data.frame(rbind(Table3,Total))
  Table3=Table3[-c(12),]
  
  #Ensure all counts of crab are numeric not a character
  library(dplyr)
  Table3[,2:7]=Table3[,2:7] %>%
    mutate_all(as.numeric)
  
  #Round numbers
  Table3[1:12,2]=round_any(Table3[1:12,2],1)
  Table3[1:12,3]=round_any(Table3[1:12,3],1)
  Table3[1:12,4]=round_any(Table3[1:12,4],1)
  Table3[1:12,5]=round_any(Table3[1:12,5],1)
  Table3[1:12,6]=round_any(Table3[1:12,6],1)
  Table3[1:12,7]=round_any(Table3[1:12,7],1)
  
  
###########Create Table 4: Split Marine Area 9 into 25C and non-25C############
  
  Area25c=Table3
  
  #Define MA9 total crab
  nine.catch=Area25c[7,2:5]
  
  #Adjust MA9 crab to non 25c portion
  Area25c[7,2:5]=Area25c[7,2:5]*(1-0.28)
  
  #Create MA9 25C portion
  Area25c[nrow(Area25c) + 1,1:5] = c("9 (25C)",nine.catch*0.28)
  
  #Re-order the data by MA: 4-13
  Area25c=Area25c %>% arrange(factor(`Marine Area`, levels = c('4', '5', '6', '7', '81', '82','9','9 (25C)','10','11','12','13','Total')))
  #Rename MA9 to MA 9 (non-25C)
  Area25c[7,1]="9 (non-25C)"
  
  #Update total crabs and pounds
  #Total crab for each MA and calculate pounds
  Area25c$`Total Crab`=rowSums(Area25c[,2:5])
  Area25c$`Pounds (lbs.)`=Area25c$`Total Crab`*1.8
  Total=c(colSums(Area25c[1:12,2:7]))
  Total=c("Total",Total)
  Area25c=as.data.frame(rbind(Area25c,Total))
  Area25c=Area25c[-c(13),]
  
  #Round numbers
  #Ensure all counts of crab are numeric not a character
  library(dplyr)
  Area25c[,2:7]=Area25c[,2:7] %>%
    mutate_all(as.numeric)
  Area25c[1:13,2]=round_any(Area25c[1:13,2],1)
  Area25c[1:13,3]=round_any(Area25c[1:13,3],1)
  Area25c[1:13,4]=round_any(Area25c[1:13,4],1)
  Area25c[1:13,5]=round_any(Area25c[1:13,5],1)
  Area25c[1:13,6]=round_any(Area25c[1:13,6],1)
  Area25c[1:13,7]=round_any(Area25c[1:13,7],1)
  
  Table4=Area25c
  
  
#########Create Table 5: Estimate non response catch (Apply bias correction)###############
  
  #Define total cards and reported cards based on the Year
  #Pull in data frame with CRC Reporting Rates by Year
  CRCs="CRC_Reporting_Rates.xlsx"
  current.directory3<-paste0(getwd(),"/CRC Data/CRC Reporting Rates/")
  yr.data=read_excel(paste0(current.directory3,CRCs))
  
  yr.data$yr.vec=as.numeric(yr.data$yr.vec)
  total=yr.data[yr.data$yr.vec==Year,4]
  reported=yr.data[yr.data$yr.vec==Year,5]
  total=as.numeric(total)
  reported=as.numeric(reported)
  
  Total.crc=total
  Reported.crc=reported

  #Calculate response rate
  Resp.rate=Reported.crc/Total.crc
  
  #Using bias correction model, calculate the bias.correction based on the response rate
  Bias.corr=(-0.5*((Resp.rate)^2))+(1.39*(Resp.rate))+0.097
  
  #Calculate the expansion factor based on reporting rates
  Expansion.fact=Total.crc/Reported.crc
  
  #Rename data and convert all to numeric
  expanded=Table4
  
  #Ensure catch is numeric
  expanded[,2:7]=expanded[,2:7] %>%
    mutate_all(as.numeric)
  
  #Calculate harvest with bias correction and expansion factor
  expanded[1:12,2:5]=(expanded[1:12,2:5])*(Expansion.fact)*(Bias.corr)
  
  #Round to nearest crab
  expanded[1:12,2]=round_any(expanded[1:12,2],1)
  expanded[1:12,3]=round_any(expanded[1:12,3],1)
  expanded[1:12,4]=round_any(expanded[1:12,4],1)
  expanded[1:12,5]=round_any(expanded[1:12,5],1)
  
  #Update total crabs and pounds
  #Total crab for each MA and calculate pounds
  expanded$`Total Crab`=rowSums(expanded[,2:5])
  expanded$`Pounds (lbs.)`=expanded$`Total Crab`*1.8
  expanded[1:12,7]=round_any(expanded[1:12,7],1)
  Total=c(colSums(expanded[1:12,2:7]))
  Total=c("Total",Total)
  expanded=as.data.frame(rbind(expanded,Total))
  expanded=expanded[-c(13),]
  
  expanded[,2:7]=expanded[,2:7] %>%
    mutate_all(as.numeric)
  
  Table5=expanded
  

#######Create Table 6: Add catch from 25C to Marine Area 12

  Table6=Table5
  Table6[11,2:7]=Table6[11,2:7]+Table6[8,2:7]
  Table6=Table6[-c(8),]
  
  
#########Export as table############
  
  current.directory4<-paste0(getwd(),"/Estimate Output/All Year Summary")
  
  dataset_names <- list('Sheet1' = Table1, 'Sheet2' = Table2, 'Sheet3' = Table3, 'Sheet4' = Table4 , 'Sheet5' = Table5,
                        'Sheet6' = Table6)
  
  Output.directory=paste0(current.directory4,paste0("/",Year,"_Winter_CRC_Tables",".xlsx"))
  
  write_xlsx(dataset_names, Output.directory)
  
  
}




#!!!!!!!!!!!!!!#

#This script is for winter CRC data
#Manually adjust CRC year of interest and CRC input data file name

# 1) Ensure spreadsheet is in correct format: Area, Month, Day, Year, NumCrab
# 2) Ensure data file name is: "Catch Data Winter YEAR.xlsx"
# 3) Input year of interest below
# 4) Input data file name below

#!!!!!!!!!!!!!!#

##Define the CRC year of interest
Year=2023

#Define the winter CRC file name (Example: Catch Data Winter 2022.xlsx)
data.name=paste0("Catch Data Winter ",Year,".xlsx")

######RUN FUNCTION
Winter.CRC.Estimate.function(Year, data.name)




