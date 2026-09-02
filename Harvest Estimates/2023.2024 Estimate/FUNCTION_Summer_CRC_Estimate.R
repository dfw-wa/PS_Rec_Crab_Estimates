
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

###Run function to summarize catch from summer CRCs

Summer.CRC.Estimate.function <- function(Year=Year, data.name=data.name){

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
  catch=catch %>% filter(success==1) #Remove unsuccessful trips if you want (doesn't change anything)
  
  ##Set marine area, month, and day as numeric
  catch$area=as.numeric(catch$area)
  catch$month=as.numeric(catch$month)
  catch$day=as.numeric(catch$day)
  
  ####Remove catch from coastal Marine Areas
  unique(catch$area) #Identify Marine Areas in data frame
  #Some Marine Areas are entered as NA or a number that doesn't make sense (e.g., '519')....set these to 192 (code for an unknown Marine Area)
  catch$area[catch$area==519]=192
  catch$area[is.na(catch$area)]=192
  #Select only Puget Sound MA's (including unknown's)
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
  Pre.June=list()
  Pre.June.sum=c()
  June=list()
  June.sum=c()
  July=list()
  July.sum=c()
  August=list()
  August.sum=c()
  September=list()
  September.sum=c()
  September.unk=list()
  September.unk.sum=c()
  post.labor=list()
  post.labor.sum=c()
  Unk.date=list()
  Unk.date.sum=c()
  areas=unique(catch.crc2$area)
  
  ##Run for loop to summarize catch:
  #1) Pre-June (Jan-May)
  #2) June
  #3) All open season catch- July, August, and September (1-Labor Day) 
  #4) Post season catch (winter catch reported on a summer card)
  #5) Catch with an unknown date
  
  #Any crab reported in September with an unknown day is included in the Sept 1-Labor Day estimate.
  
  #Run for loop based on Marine Area while filtering for the date
  for (i in 1:length(unique(catch.crc2$area)))
  {
    
    #Filter by area
    area.list[[i]]=catch.crc2 %>% filter(area==areas[i])
    
    Pre.June[[i]]=area.list[[i]] %>% filter(month==5 | month==4 | month==3 | month==2 | month==1)
    Pre.June.sum[i]=sum(Pre.June[[i]]$num_crab)
    
    June[[i]]=area.list[[i]] %>% filter(month==6)
    June.sum[i]=sum(June[[i]]$num_crab) 
    
    July[[i]]=area.list[[i]] %>% filter(month==7)
    July.sum[i]=sum(July[[i]]$num_crab) 
    
    August[[i]]=area.list[[i]] %>% filter(month==8)
    August.sum[i]=sum(August[[i]]$num_crab) 
    
    September[[i]]=area.list[[i]] %>% filter(date >= paste0(Year,"-09-01") & date <= LD)
    September.sum[i]=sum(September[[i]]$num_crab)
    
    September.unk[[i]]=area.list[[i]] %>% filter(month==9 & day==99)
    September.unk.sum[i]=sum(September.unk[[i]]$num_crab) 
    
    post.labor[[i]]=area.list[[i]] %>% filter(date > LD)
    post.labor.sum[i]=sum(post.labor[[i]]$num_crab)
    
    Unk.date[[i]]=area.list[[i]] %>% filter(month==99)
    Unk.date.sum[i]=sum(Unk.date[[i]]$num_crab) 
    
  }
  
  #Add 'September 1-Labor Day' and 'September with an unknown day' together
  #Assuming anything reported in September with an unknown day occurred during the open season.
  September=c(September.sum+September.unk.sum)
  
#############Create CRC Table 1: Sum catch by each date category and Marine Area##########
  
  Marine.Area=unique(catch.crc2$area) #define marine area column
  Table1=as.data.frame(cbind(Marine.Area,Pre.June.sum,June.sum,July.sum,August.sum,September,post.labor.sum,Unk.date.sum))
  Table1
  
  ###Create a new row and column for total crab and pounds
  Table1$Total.Crabs=rowSums(Table1[,2:8])
  Table1$Lbs.Crab=Table1$Total.Crabs*1.8
  Total=c(colSums(Table1[,2:10]))
  Total=c("Total",Total)
  Table1=as.data.frame(rbind(Table1,Total))
  
  #Ensure all counts of crab are numeric not a character
  library(dplyr)
  Table1[,2:10]=Table1[,2:10] %>%
    mutate_all(as.numeric)
  
  #Order the data by Marine Area: 4-13
  Table1=Table1 %>% arrange(factor(Marine.Area, levels = c('4', '5', '6', '7', '8', '81', '82','9','10','11','12','13','192','Total')))
  
  #Round to nearest crab
  Table1[,10]=round_any(Table1[,10], 1)
  
  #Rename columns
  colnames(Table1) <- c("Marine Area", "Pre June", "June", "July", "August", "September", "Post Labor Day", "Unknown Date", "Total Crab", "Pounds (lbs.)")
  
  
#########Create CRC Table2: Distribute out of season catch into unknown date category############
  
  #Out of season catch = Pre-June, June, Post-labor Day and unknown date into the Unknown column
  Table2=Table1
  Table2$Unknown=Table2$`Pre June`+Table2$June+Table2$`Post Labor Day`+Table2$`Unknown Date`
  Table2=Table2[,-c(2,3,7,8)]
  Table2=Table2 %>% relocate(Unknown, .after = September)
  
  
###########Create CRC Table 3: Apportioning unknown date/Marine and MA8 into 8-1 and 8-2###########
  
  ###Apportion:
  #1) Unknown Marine Area/month into known Marine Area/unknown Month
  #2) All catch in unknown month column to known month for each Marine Area
  #3) Catch from Marine Area 8 into 8-1 and 8-2
  
  Table3=Table2
  
  #1) Join catch from unknown MA (192)/known month cells into unknown MA/unknown month cell
  Table3[13,5]=Table3[13,5]+sum(Table3[13,2:4])
  Table3[13,2:4]=0
  
  #1) Find catch proportions for each MA in the Unknown month column
  #Apportion unknown MA/month cell to unknown month for each MA
  unk.prop=Table3$Unknown[1:12]/sum(Table3$Unknown[1:12])
  Table3[1:12,5]=(unk.prop*Table3$Unknown[13])+(Table3$Unknown[1:12])
  #Now remove unknown MA row
  Table3=Table3[-c(13),]
  
  #2) Find catch proportions for open season columns (July, Aug, Sept 1-5) for each marine area
  month.prop=Table3[1:12,2:4]/rowSums(Table3[1:12,2:4])
  #Convert all NAs to zero
  month.prop[is.na(month.prop)]=0
  ##Special case when total catch during known months for a marine area=0 but some unknown catch is present: 
  ##When calculating catch proportions, each month will calculate to 0 (or NA) and cant apportion unknown catch.
  ##If total catch is 0 for known months for a marine area, assign 0.3333 as the catch proportion for each month.
  for(i in 1:12){
    if(sum(month.prop[i,1:3])==0){
      month.prop[i,1:3]=1/3}}
  Table3[1:12,2:4]=(month.prop*Table3[1:12,5])+(Table3[1:12,2:4])
  #Now remove unknown date column
  Table3=Table3[,-c(5)]
  
  #Convert all NAs to zero
  Table3[is.na(Table3)]=0
  
  #3) Find catch proportions for 81 and 82
  eight.prop2=Table3$July[6:7]/sum(Table3$July[6:7])
  eight.prop3=Table3$August[6:7]/sum(Table3$August[6:7])
  eight.prop4=Table3$September[6:7]/sum(Table3$September[6:7])
  eight.prop5=as.data.frame(cbind(eight.prop2,eight.prop3, eight.prop4))
  Table3[6:7,2]=(eight.prop5[,1]*Table3$July[5])+(Table3[6:7,2])
  Table3[6:7,3]=(eight.prop5[,2]*Table3$August[5])+(Table3[6:7,3])
  Table3[6:7,4]=(eight.prop5[,3]*Table3$September[5])+(Table3[6:7,4])
  #Now remove MA8 row
  Table3=Table3[-c(5),]
  
  #Update total crabs and pounds
  #Total crab for each MA and calculate pounds
  Table3$`Total Crab`=rowSums(Table3[,2:4])
  Table3$`Pounds (lbs.)`=Table3$`Total Crab`*1.8
  Total=c(colSums(Table3[1:11,2:6]))
  Total=c("Total",Total)
  Table3=as.data.frame(rbind(Table3,Total))
  Table3=Table3[-c(12),]
  
  #Round numbers
  #Ensure all counts of crab are numeric not a character
  library(dplyr)
  Table3[,2:6]=Table3[,2:6] %>%
    mutate_all(as.numeric)
  
  Table3[1:12,2]=round_any(Table3[1:12,2],1)
  Table3[1:12,3]=round_any(Table3[1:12,3],1)
  Table3[1:12,4]=round_any(Table3[1:12,4],1)
  Table3[1:12,5]=round_any(Table3[1:12,5],1)
  Table3[1:12,6]=round_any(Table3[1:12,6],1)
  
  
###########Create Table 4: Split Marine Area 9 into 25C and non-25C############
  
  Area25c=Table3
  
  #Define MA9 total crab
  nine.catch=Area25c[7,2:4]
  
  #Adjust MA9 crab to non 25c portion
  #Catch Area 25C represents 28% of catch from Marine Area 9
  Area25c[7,2:4]=Area25c[7,2:4]*(1-0.28)
  
  #Create MA9 25C portion
  Area25c[nrow(Area25c) + 1,1:4] = c("9 (25C)",nine.catch*0.28)
  
  #Re-order the data by Marine Area
  Area25c=Area25c %>% arrange(factor(`Marine Area`, levels = c('4', '5', '6', '7', '81', '82','9','9 (25C)','10','11','12','13','Total')))
  
  #Rename MA9 to MA 9 (non-25C)
  Area25c[7,1]="9 (non-25C)"
  
  #Update total crabs and pounds
  Area25c$`Total Crab`=rowSums(Area25c[,2:4])
  Area25c$`Pounds (lbs.)`=Area25c$`Total Crab`*1.8
  Total=c(colSums(Area25c[1:12,2:6]))
  Total=c("Total",Total)
  Area25c=as.data.frame(rbind(Area25c,Total))
  Area25c=Area25c[-c(13),]
  
  #Round numbers
  #Ensure all counts of crab are numeric not a character
  library(dplyr)
  Area25c[,2:6]=Area25c[,2:6] %>%
    mutate_all(as.numeric)
  
  Area25c[1:13,2]=round_any(Area25c[1:13,2],1)
  Area25c[1:13,3]=round_any(Area25c[1:13,3],1)
  Area25c[1:13,4]=round_any(Area25c[1:13,4],1)
  Area25c[1:13,5]=round_any(Area25c[1:13,5],1)
  Area25c[1:13,6]=round_any(Area25c[1:13,6],1)
  
  #Rename table
  Table4=Area25c
  
  
#########Create Table 5: Estimate non response catch (Apply bias correction)###############
  
  #Define total cards and reported cards based on the Year
  #Pull in data frame with CRC Reporting Rates by Year
  CRCs="CRC_Reporting_Rates.xlsx"
  current.directory3<-paste0(getwd(),"/CRC Data/CRC Reporting Rates/")
  yr.data=read_excel(paste0(current.directory3,CRCs))
  
  yr.data$yr.vec=as.numeric(yr.data$yr.vec)
  total=yr.data[yr.data$yr.vec==Year,2]
  reported=yr.data[yr.data$yr.vec==Year,3]
  total=as.numeric(total)
  reported=as.numeric(reported)
  
  Total.crc=total
  Reported.crc=reported

  #Calculate response rate
  Resp.rate=Reported.crc/Total.crc
  
  #Using bias correction model, calculate the bias.correction based on the response rate
  Bias.corr=(-0.5*((Resp.rate)^2))+(1.39*(Resp.rate))+0.097
  
  #Updated bias correction model:
  #Bias.corr=(-0.36678*((Resp.rate)^2))+(1.24398*(Resp.rate))+0.11722
  
  #Calculate the expansion factor based on reporting rates
  Expansion.fact=Total.crc/Reported.crc
  
  #Rename data and convert all to numeric
  expanded=Table4
  
  expanded[,2:6]=expanded[,2:6] %>%
    mutate_all(as.numeric)
  
  #Calculate harvest with bias correction and expansion factor
  expanded[1:12,2:4]=(expanded[1:12,2:4])*(Expansion.fact)*(Bias.corr)
  
  #Round to nearest crab
  expanded[1:12,2]=round_any(expanded[1:12,2],1)
  expanded[1:12,3]=round_any(expanded[1:12,3],1)
  expanded[1:12,4]=round_any(expanded[1:12,4],1)
  
  #Update total crabs and pounds
  expanded$`Total Crab`=rowSums(expanded[,2:4])
  expanded$`Pounds (lbs.)`=expanded$`Total Crab`*1.8
  expanded[1:12,6]=round_any(expanded[1:12,6],1)
  Total=c(colSums(expanded[1:12,2:6]))
  Total=c("Total",Total)
  expanded=as.data.frame(rbind(expanded,Total))
  expanded=expanded[-c(13),]
  
  #Make all numeric again
  expanded[,2:6]=expanded[,2:6] %>%
    mutate_all(as.numeric)
  
  #Final Harvest for summer
  Table5=expanded
  

#######Create Table 6: Add catch from 25C to Marine Area 12

  Table6=Table5
  Table6[11,2:6]=Table6[11,2:6]+Table6[8,2:6]
  Table6=Table6[-c(8),]
  
  
#########Export as table############
  
  current.directory4<-paste0(getwd(),"/Estimate Output/All Year Summary")
  
  dataset_names <- list('Sheet1' = Table1, 'Sheet2' = Table2, 'Sheet3' = Table3, 'Sheet4' = Table4 , 'Sheet5' = Table5,
                        'Sheet6' = Table6)
  
  Output.directory=paste0(current.directory4,paste0("/",Year,"_Summer_CRC_Tables_Final",".xlsx"))
  
  write_xlsx(dataset_names, Output.directory)
  
  
}




#!!!!!!!!!!!!!!#

#This script is for summer CRC data
#Manually adjust CRC year of interest and CRC input data file name

# 1) Ensure spreadsheet is in correct format: Area, Month, Day, Year, NumCrab
# 2) Ensure data file name is: "Catch Data Summer YEAR.xlsx"
# 3) Input year of interest below
# 4) Input data file name below

#!!!!!!!!!!!!!!#

##Define the CRC year of interest
Year=2023

#Define the summer CRC file name (Example: Catch Data Summer 2022.xlsx)
data.name=paste0("Catch Data Summer ",Year,".xlsx")


######RUN FUNCTION
Summer.CRC.Estimate.function(Year, data.name)




