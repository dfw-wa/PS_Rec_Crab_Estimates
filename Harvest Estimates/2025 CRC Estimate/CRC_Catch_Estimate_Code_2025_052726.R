#Version 2 2025 order of operations:
#*First define season, year, and file name at bottom of script
# 1) Add respondent catch
# 2) Add late catch
# 3) Add Non-respondent catch
# 4) Add Charter catch
# 5) Convert to sub-areas for management regions
# 6) Add EUC  but dont include EUC on charter catch** (use regional euc xlsx file).
# 7) Convert to CMR

#####Load Packages
library(tidyr)
library(plyr)
library(dplyr)
library(magrittr)
library(eeptools)
library(readxl)
library(lubridate)
library(writexl)

###Run function to summarize catch from CRCs


CRC.Estimate.function <- function(Year=Year, Season=Season, data.name=data.name){
  
############Bring in data#########
  #Set the current directory of interest
  current.directory<-paste0(getwd(),"/CRC Estimate Function Inputs/")
  
  #Bring in data files
  catch=read_excel(paste0(current.directory,data.name))
  
############Prep catch file############
  catch$num_crab=as.numeric(catch$num_crab) #Ensure crabs reported is numeric
  
  ##Handling NA crab (this is not supposed to happen but might show up). Likely due to deactivated holders account. 
  catch %>% filter(is.na(num_crab)) #rows that report NA crab harvested
  catch$num_crab[is.na(catch$num_crab)]=0 #Treat as an unsuccessful trip/0 catch card
  
  ##########Total Catch Reported from all CRCs (including coastal catch)
  sum(catch$num_crab)
  
  #Resolve area formatting issues
  #Convert any "8-1" and "8-2" area format to 81 and 82 for processing
  catch$area[catch$area=="8-1"] <- "81"  
  catch$area[catch$area=="8-2"] <- "82"  
  #Resolve other area formatting issues that may appear. This is rare.
  catch$area[catch$area=="01" | catch$area=="001"] <- "1"  
  catch$area[catch$area=="02" | catch$area=="002"] <- "2"  
  catch$area[catch$area=="03" | catch$area=="003"] <- "3"  
  catch$area[catch$area=="04" | catch$area=="004"] <- "4"  
  catch$area[catch$area=="05" | catch$area=="005"] <- "5"  
  catch$area[catch$area=="06" | catch$area=="006"] <- "6"  
  catch$area[catch$area=="07" | catch$area=="007"] <- "7"  
  catch$area[catch$area=="08" | catch$area=="008"] <- "8"  
  catch$area[catch$area=="09" | catch$area=="009"] <- "9" 
  
  #Some Marine Areas (with reported catch) are entered as NA, set these to 192 (code for an unknown Marine Area). This is rare.
  catch$area[is.na(catch$area) & catch$num_crab > 0]=192
  
  ####Handling catch from coastal Marine Areas (don't include coastal catch in Puget Sound estimate)
  unique(catch$area) #Identify Marine Areas in data frame
  
  catch.coast=catch %>% filter(area=="1"|area=="2"|area=="2-1"|area=="2-2"|area=="21" |area=="22" |area=="23" |area=="3"|area=="519")
  unique(catch.coast$area) #coastal MA's present in data
  crabs.reported.coastal=sum(catch.coast$num_crab) 
  crabs.reported.coastal #Total reported crabs from the coast
 
  ##Puget Sound catch
  
  #Convert areas present in the data frame that are NOT Coastal/PS to unknown (192).
   #Any non-sensible areas present are converted to unknown. This is rare but might be present on a mailed in card.
  #Vector listing all coastal and PS areas. Note: A '0' catch card (did not attempt to crab or catch crab) will have NA for marine area...leave as NA.
  known.areas=c("1","2","21","22","23","2-1","2-2","3","519","4","5","6","7","8","81","82","9","10","11","12","13","192",NA)
  catch$area[!catch$area %in% known.areas] <- "192"  
  
  ##Select only Puget Sound MA's (including unknown's)
  catch.crc2=catch %>% filter(area=="4"|area=="5"|area=="6"|area=="7"|area=="8"|area=="81"|area=="82"|area=="9"|
                                area=="10"|area=="11"|area=="12"|area=="13"|area=="192")
  catch.crc2$area=as.numeric(catch.crc2$area) # convert area to numeric
  crabs.reported.PS=sum(catch.crc2$num_crab)
  crabs.reported.PS #Total reported crabs from Puget Sound
  
  #Some CRC data sets will not have any catch reported for a specific MA. Because catch is reported/summarized for all areas (even if the catch is 0),
   #we need each area to be present in the catch file.
   #Apply zero catch to one trip for each MA.
  define.area=c(4,5,6,7,8,81,82,9,10,11,12,13,192)
  
  for (i in 1:13)
  {
    catch.crc2[nrow(catch.crc2) + 1,1] = c(define.area[i])
    catch.crc2[nrow(catch.crc2),5] = c(0)
  }
  
 
  ##Cleaning day and month data entry errors. These are very rare.
  #For successful trips, some months and days are entered into the data frame as NA instead of 99 (code for an unknown day or month). Set each NA to 99.
  #Also, set values for day and month that don't make sense on a calendar to 99.
  
  unique(catch.crc2$month) #Identify months in data frame
  catch.crc2$month=as.numeric(catch.crc2$month)
  
  unique(catch.crc2$day) #Identify days in data frame
  catch.crc2$day=as.numeric(catch.crc2$day)
  
  catch.crc2$day[is.na(catch.crc2$day) & catch.crc2$num_crab > 0]=99 #For successful trips, if day=NA set day to 99.
  catch.crc2$month[is.na(catch.crc2$month) & catch.crc2$num_crab > 0]=99 #For successful trips, if month=NA set month to 99.
  
  #Convert any non-sensible months in the data frame to unknown month (99)
  known.months=c(1,2,3,4,5,6,7,8,9,10,11,12,99,NA)
  catch.crc2$month[!catch.crc2$month %in% known.months] <- 99  
  
  #Convert any non-sensible days in the data frame to unknown day (99)
  known.days=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,99,NA)
  catch.crc2$day[!catch.crc2$day %in% known.days] <- 99  
  
  #Create a DATE column for filtering in-season and out-season catch: Convert year, month, and day columns into a date column for filtering
  catch.crc2$date=as.Date(with(catch.crc2, paste(year,month,day,sep="-")), "%Y-%m-%d")
  
  #Identify failed dates (this identifies data entry errors related to the day).
  ##Example: June 31st, September 31st, etc.
  date.error=catch.crc2 %>% filter(is.na(date) & day < 32 & month < 13) #Identify where the date failed when the day/month is known
  #Change the day to 99 for dates identified above
  catch.crc2 <- catch.crc2 %>%
    mutate(day = case_when(
      is.na(date) & day < 32 & month < 13 ~ 99,
      TRUE ~ day))
  
#############    
  ##Read in file that contains which areas were fully closed (did not have a season)
  
  closed.data.name=paste0("Closed_Areas_Inputs_",Season,".xlsx")
  current.directory2<-paste0(getwd(),"/CRC Estimate Function Inputs/",Season,"/")
  closed.areas=read_excel(paste0(current.directory2,closed.data.name),sheet = paste0(Year))
  
  ##Create new column to identify an area that was fully closed (never had a season). Used later.
  catch.crc2 = catch.crc2 %>%
    mutate(area.closed = ifelse( (area %in% unique(closed.areas$closed.areas)), "yes", "no"))
  
  
#############   
  ##Read in file that contains the season dates by area
  
  dates=paste0("Seasons_Areas_Inputs_",Season,".xlsx")
  current.directory2<-paste0(getwd(),"/CRC Estimate Function Inputs/",Season,"/")
  yr.data=read_excel(paste0(current.directory2,dates),sheet = paste0(Year))
  
  yr.data$season.start=as.Date(yr.data$season.start) #Convert season start to a date
  yr.data$season.end=as.Date(yr.data$season.end) #Convert season end to a date
  
  
###########Run loop to summarize catch by date categories and Marine Area#############
  
  #Pre-define lists and vectors for looping
  area.list=list()
  date.list=list()
  areas=unique(catch.crc2$area)
  in.season=list()
  in.season.sum=c()
  in.season.no.day=list()
  in.season.no.day.sum=c()
  out.season=list()
  out.season.sum=c()
  out.season.no.day=list()
  out.season.no.day.sum=c()
  Unk.date=list()
  Unk.date.sum=c()
  closed.area=list()
  closed.area.sum=c()
  unknown.date.no.day=list()
  unknown.date.no.day.sum=c()
  
  in.season.full=list()
  in.season.partial=list()
  out.season.full=list()
  
  ##Run for loop to summarize catch:
  #1) Open Season
  #2) Closed Season
  #3) Catch with an unknown date
  
  areas=c(4,5,6,7,8,81,82,9,10,11,12,13,192)
  
  #Run for loop based on Marine Area while filtering for the date
  for (i in 1:length(unique(catch.crc2$area)))
  {
    
    ##Filter by area
    area.list[[i]]=catch.crc2 %>% filter(area==areas[i])
    date.list[[i]]=yr.data %>% filter(area==areas[i])
    
    ##Identify and put into a list the months for each area that are open for the full duration of the month, partially open, and closed for the full duration of the month
    ##Catch reported during a partially open month (and no day reported) will be categorized as 'Unknown date'
    date.list[[i]]$full.in.season=as.character(date.list[[i]]$full.in.season) #Ensure character
    date.list[[i]]$partial.in.season=as.character(date.list[[i]]$partial.in.season) #Ensure character
    date.list[[i]]$full.out.season=as.character(date.list[[i]]$full.out.season) #Ensure character
    
    in.season.full[[i]]=unlist(strsplit(date.list[[i]]$full.in.season, ","))
    in.season.partial[[i]]=unlist(strsplit(date.list[[i]]$partial.in.season, ","))
    out.season.full[[i]]=unlist(strsplit(date.list[[i]]$full.out.season, ","))
    
    ####In-season
    
    ##1a) In-season catch
    #Filter and sum all in-season catch with a known date
    #Will only filter for dates defined in the seasons by area file.
    
    in.season[[i]]=area.list[[i]] %>% filter(date >= date.list[[i]]$season.start & date <= date.list[[i]]$season.end)
    in.season.sum[i]=sum(in.season[[i]]$num_crab)
    
    ###1b) Known month (area open for the full duration of that month) and unknown day --- in-season catch
    #Will only filter for months defined in the seasons by area file.
    #Assume anything reported during a fully open month without a day is in-season.
    
    in.season.no.day[[i]]=area.list[[i]] %>% filter(month==in.season.full[[i]][1] & day==99 | month==in.season.full[[i]][2] & day==99
                                                    | month==in.season.full[[i]][3] & day==99 | month==in.season.full[[i]][4] & day==99 
                                                    | month==in.season.full[[i]][5] & day==99 | month==in.season.full[[i]][6] & day==99
                                                    | month==in.season.full[[i]][7] & day==99 | month==in.season.full[[i]][8] & day==99
                                                    | month==in.season.full[[i]][9] & day==99 | month==in.season.full[[i]][10] & day==99
                                                    | month==in.season.full[[i]][11] & day==99 | month==in.season.full[[i]][12] & day==99)
    in.season.no.day.sum[i]=sum(in.season.no.day[[i]]$num_crab)
    
    
    ####Out of Season
    
    ##2a) Out of season catch
    #Filter and sum all out-season catch with a known date
    #Will only filter for dates defined in the seasons by area file.
    out.season[[i]]=area.list[[i]] %>% filter(date < date.list[[i]]$season.start | date > date.list[[i]]$season.end)
    out.season.sum[i]=sum(out.season[[i]]$num_crab)
    
    #2b)  Known month (area closed for the full duration of that month) and unknown day ---- Out of season catch
    ##Will only filter for months defined in the seasons by area file.
    ##Assume anything reported during a fully closed month without a day is out of season
    
    out.season.no.day[[i]]=area.list[[i]] %>% filter(month==out.season.full[[i]][1] & day==99 | month==out.season.full[[i]][2] & day==99
                                                     | month==out.season.full[[i]][3] & day==99 | month==out.season.full[[i]][4] & day==99 
                                                     | month==out.season.full[[i]][5] & day==99 | month==out.season.full[[i]][6] & day==99
                                                     | month==out.season.full[[i]][7] & day==99 | month==out.season.full[[i]][8] & day==99
                                                     | month==out.season.full[[i]][9] & day==99 | month==out.season.full[[i]][10] & day==99
                                                     | month==out.season.full[[i]][11] & day==99 | month==out.season.full[[i]][12] & day==99)
    out.season.no.day.sum[i]=sum(out.season.no.day[[i]]$num_crab)
    
    #2c) Closed area catch (area without a season) is treated as out-season catch
    #Filter for closed areas and sum all catch and treat as out-season
    closed.area[[i]]=area.list[[i]] %>% filter(area.closed=="yes")
    closed.area.sum[i]=sum(closed.area[[i]]$num_crab)
    
    
    ####Unknown Date 
    
    ###3a) Catch with unknown month
    #Filter and sum all catch without a month
    Unk.date[[i]]=area.list[[i]] %>% filter(month==99)
    Unk.date.sum[i]=sum(Unk.date[[i]]$num_crab) 
    
    ###3b) Known month (area is only partially open for that month) with an unknown day ---- Unknown date catch
    ##Will only filter for months defined in the seasons by area file.
    #Assume anything reported during a partially open month without a day is unknown
    
    unknown.date.no.day[[i]]=area.list[[i]] %>% filter(month==in.season.partial[[i]][1] & day==99 | month==in.season.partial[[i]][2] & day==99
                                                       | month==in.season.partial[[i]][3] & day==99 | month==in.season.partial[[i]][4] & day==99 
                                                       | month==in.season.partial[[i]][5] & day==99 | month==in.season.partial[[i]][6] & day==99
                                                       | month==in.season.partial[[i]][7] & day==99 | month==in.season.partial[[i]][8] & day==99
                                                       | month==in.season.partial[[i]][9] & day==99 | month==in.season.partial[[i]][10] & day==99
                                                       | month==in.season.partial[[i]][11] & day==99 | month==in.season.partial[[i]][12] & day==99)
    unknown.date.no.day.sum[i]=sum(unknown.date.no.day[[i]]$num_crab)
    
  }
  
  #Add all in-season catch categories together 
  ##1) 'in season with known date' 
  ##2) 'in season with known month but unknown day'
  in.season.sum2=c(in.season.sum+in.season.no.day.sum)
  
  #Add all out-season catch categories together 
  ##1) 'out season with known date' 
  ##2) 'out season with known month but unknown day'
  ##3) 'closed are catch'
  out.season.sum2=c(out.season.sum+out.season.no.day.sum+closed.area.sum)
  
  #Add all unknown date catch categories together 
  ##1) 'Catch with unknown month' 
  ##2) 'Catch with known month (in and out of season month) with an unknown day'
  Unk.date.sum2=c(Unk.date.sum+unknown.date.no.day.sum)
  
  #Check that all catch has been accounted for (not including catch from coastal Marine Areas)
  total.crab.check=sum(catch.crc2$num_crab)==sum(c(in.season.sum2+out.season.sum2+Unk.date.sum2))
  #STOP the function if this condition is not met (If FALSE, the for loop did not account for every category of catch). Should always be TRUE.
  if(total.crab.check==FALSE)
  {
    stop()
  }
  
  #############Create CRC Table 1: Sum catch by each date category and Marine Area##########
  
  #Order the data by Marine Area: 4-13
  catch.crc2=catch.crc2 %>% arrange(factor(area, levels = c('4', '5', '6', '7', '8', '81', '82','9','10','11','12','13','192','Total')))
  Marine.Area=unique(catch.crc2$area) #define marine area column
  #Put everything together in a table
  Table1=as.data.frame(cbind(Marine.Area,in.season.sum2,out.season.sum2,Unk.date.sum2))
  Table1
  
  colnames(Table1) <- c("Marine Area", "In-Season", "Out of Season", "Unknown Date")
  
  ##Create an output table (Table1 Output: Total crabs, total pounds, w/ decimals rounded)
  Table1.output=Table1
  
  ###Create a new row and column for total crab and pounds
  Table1.output$`Total Crab`=rowSums(Table1.output[,2:4])
  Table1.output$`Pounds (lbs.)`=Table1.output$`Total Crab`*1.8
  Total=c(colSums(Table1.output[,2:6]))
  Total=c("Total",Total)
  Table1.output=as.data.frame(rbind(Table1.output,Total))
  
  #Ensure all counts of crab are numeric not a character (for rounding)
  Table1.output[,2:ncol(Table1.output)]=Table1.output[,2:ncol(Table1.output)] %>%
    mutate_all(as.numeric)
  
  ##Round to nearest crab
  Table1.output[1:13,ncol(Table1.output)]=round_any(Table1.output[1:13,ncol(Table1.output)], 1)
  #Update Total Pounds after rounding
  Table1.output[14,6]=sum(Table1.output$`Pounds (lbs.)`[1:13])
  
  #########Create CRC Table 2: Distribute MA 8 catch to 8-1 and 8-2############
  Table2=Table1
  
  #Find catch proportions for 81 and 82
  eight.prop.in=Table2$`In-Season`[6:7]/sum(Table2$`In-Season`[6:7])
  eight.prop.out=Table2$`Out of Season`[6:7]/sum(Table2$`Out of Season`[6:7])
  eight.prop.unk=Table2$`Unknown Date`[6:7]/sum(Table2$`Unknown Date`[6:7])
  eight.prop.combined=as.data.frame(cbind(eight.prop.in,eight.prop.out, eight.prop.unk))
  
  #If the sum of the catch proportions for 8-1/8-2 is 0 (or NaN; cant divide by zero), assign in-season catch proportions of 8-1/8-2.
  for(i in 1:3){
    if(sum(eight.prop.combined[1:2,i])==0 | sum(is.nan(eight.prop.combined[1:2,i]))){
      eight.prop.combined[1:2,i]=eight.prop.combined[1:2,1]}} #Assign in-season catch proportions
  
  #Apportion MA8 catch to 81 and 82 based on proportions
  Table2[6:7,2]=(eight.prop.combined[,1]*Table2$`In-Season`[5])+(Table2[6:7,2])
  Table2[6:7,3]=(eight.prop.combined[,2]*Table2$`Out of Season`[5])+(Table2[6:7,3])
  Table2[6:7,4]=(eight.prop.combined[,3]*Table2$`Unknown Date`[5])+(Table2[6:7,4])
  
  #Now remove MA 8 row
  Table2=Table2[-c(5),]
  
  #########Distribute unknown area catch (192) to known areas############
  
  #Find catch proportions for each area
  unk.prop.in=Table2$`In-Season`[1:11]/sum(Table2$`In-Season`[1:11])
  unk.prop.out=Table2$`Out of Season`[1:11]/sum(Table2$`Out of Season`[1:11])
  unk.prop.unk=Table2$`Unknown Date`[1:11]/sum(Table2$`Unknown Date`[1:11])
  unk.prop.combined=as.data.frame(cbind(unk.prop.in,unk.prop.out, unk.prop.unk))
  
  ###############
  #If the sum of the unknown month catch proportions for MAs 4-13 is 0 (or NaN; cant divide by zero), assign in-season catch proportions for all areas.
  #Highly unlikely for in-season and out-season catch but may happen for unknown month catch.
  for(i in 1:3){
    if(sum(unk.prop.combined[1:11,i])==0 | sum(is.nan(unk.prop.combined[1:11,i]))){
      unk.prop.combined[1:11,i]=unk.prop.combined[1:11,1]}} #Assign in-season catch proportions
  ###############
  
  #Apportion area 192 (unknown) catch to all areas based on proportions
  Table2[1:11,2]=(unk.prop.combined[,1]*Table2$`In-Season`[12])+(Table2[1:11,2])
  Table2[1:11,3]=(unk.prop.combined[,2]*Table2$`Out of Season`[12])+(Table2[1:11,3])
  Table2[1:11,4]=(unk.prop.combined[,3]*Table2$`Unknown Date`[12])+(Table2[1:11,4])
  
  #Now remove MA 192 row
  Table2=Table2[-c(12),]
  
  ##Table2 Output (Total crabs and pounds and round decimals)
  Table2.output=Table2
  
  #Ensure all counts of crab are numeric not a character
  Table2.output[,2:4]=Table2.output[,2:4] %>%
    mutate_all(as.numeric)
  
  #Round numbers
  Table2.output[1:11,2]=round_any(Table2.output[1:11,2],1)
  Table2.output[1:11,3]=round_any(Table2.output[1:11,3],1)
  Table2.output[1:11,4]=round_any(Table2.output[1:11,4],1)
  
  ###Create a new row and column for total crab and pounds
  Table2.output$`Total Crab`=rowSums(Table2.output[,2:4])
  Table2.output$`Pounds (lbs.)`=Table2.output$`Total Crab`*1.8
  Total=c(colSums(Table2.output[1:11,2:6]))
  Total=c("Total",Total)
  Table2.output=as.data.frame(rbind(Table2.output,Total))
  
  #Round total pounds and update
  #Ensure all counts of crab are numeric not a character
  Table2.output[,2:6]=Table2.output[,2:6] %>%
    mutate_all(as.numeric)
  Table2.output[1:11,6]=round_any(Table2.output[1:11,6],1)
  
  #Update Total Pounds after rounding
  Table2.output[12,6]=sum(Table2.output$`Pounds (lbs.)`[1:11])
  
############Calculate Late Card Catch ############

#If late card data is not available for the current CRC year, the following will run:
  #This has already been defined based on what you input ("Yes" or "No") to whether late card data was available prior to running the function.
if(late.catch.available=="No")
{
  #Pull in data frame for late card catch estimation
  Late="Late_Card_Inputs.xlsx"
  current.directory3<-paste0(getwd(),"/CRC Estimate Function Inputs/")
  late.data=read_excel(paste0(current.directory3,Late))
  
  late.data$yr.vec.late=as.numeric(late.data$yr.vec.late) #Ensure numeric

  #Filter late card inputs for current catch year
  late.data=late.data %>% filter(yr.vec.late==Year)
  #Filter late card inputs for current catch season
  late.data=late.data %>% filter(Season.filt==Season)
  
  #Estimate the number of late Cards
  late.card.total=round_any(late.data$total.crc * late.data$`5.year.late.ratio`,1)
  
  #Estimate mean catch per late CRC
  catch.per.late=late.data$`3.or.1.year.avg.catch.per.late.card.ratio` * late.data$respondent.catch.per.card.current.season

  #if more data exists, the ratio is a 3-year avg.
  
  #In-season late catch estimate
  late.catch.total=round_any(late.card.total * catch.per.late,1)
}
  
#If late card data is available for the current CRC year, the following will run:

if(late.catch.available=="Yes")
{ 
  #Pull in late card data for the current CRC year
  Late=paste0("Late_Catch_",Season,"_",Year,".xlsx")
  current.directory3<-paste0(getwd(),"/CRC Estimate Function Inputs/")
  late.data=read_excel(paste0(current.directory3,Late))
  
  #Total number of late cards
  late.card.total=length(unique(late.data$ID))
  
  ##Select only Puget Sound MA's (including unknown's)
  late.data=late.data %>% filter(area=="4"|area=="5"|area=="6"|area=="7"|area=="8"|area=="81"|area=="82"|area=="9"|
                                area=="10"|area=="11"|area=="12"|area=="13"|area=="192")
  late.data$area=as.numeric(late.data$area) # convert area to numeric
  crabs.late.PS=sum(late.data$num_crab)
  crabs.late.PS #Total reported crabs from Puget Sound
  
  #Some CRC data sets will not have any catch reported for a specific MA. Because catch is reported/summarized for all areas (even if the catch is 0),
  #we need each area to be present in the catch file.
  #Apply zero catch to one trip for each MA.
  define.area=c(4,5,6,7,8,81,82,9,10,11,12,13,192)
  
  for (i in 1:13)
  {
    late.data[nrow(late.data) + 1,1] = c(define.area[i])
    late.data[nrow(late.data),5] = c(0)
  }
  
  detach("package:plyr", unload = TRUE)
  late.summary.area=late.data %>%
    group_by(area) %>%
    summarise(crabs=sum(num_crab))
  
  late.summary.area=as.data.frame(late.summary.area)
  late.summary.area$crabs=as.numeric(late.summary.area$crabs)
  
  #Order By Marine Area
  late.summary.area=late.summary.area %>% arrange(factor(area, levels = c('4', '5', '6', '7', '8', '81', '82','9','10','11','12','13','192','Total')))
  
  #Distribute MA 8 late catch to 8-1 and 8-2
  #Find catch proportions for 81 and 82
  eight.prop.late=late.summary.area$crabs[6:7]/sum(late.summary.area$crabs[6:7])
  #Apportion MA8 catch to 81 and 82 based on proportions
  late.summary.area[6:7,2]=(eight.prop.late*late.summary.area$crabs[5])+(late.summary.area[6:7,2])
  #Now remove MA 8 row
  late.summary.area=late.summary.area[-c(5),]
  
  #Distribute unknown area (192) late catch to all areas
  #Find catch proportions for all areas
  unknown.prop.late=late.summary.area$crabs[1:11]/sum(late.summary.area$crabs[1:11])
  #Apportion MA8 catch to 81 and 82 based on proportions
  late.summary.area[1:11,2]=(unknown.prop.late*late.summary.area$crabs[12])+(late.summary.area[1:11,2])
  #Now remove MA 8 row
  late.summary.area=late.summary.area[-c(12),]
  
  #Round and calculate total late pounds
  #Ensure all counts of crab are numeric not a character
  #late.summary.area[,2]=late.summary.area[,2] %>%
  #  mutate_all(as.numeric)
  
  #Round numbers
  library(plyr)
  late.summary.area[1:11,2]=round_any(late.summary.area[1:11,2],1)
  
  ###Create a new row and column for total crab and pounds
  late.summary.area$`Late Pounds (lbs.)`=late.summary.area$crabs*1.8
  Total=c(colSums(late.summary.area[1:11,2:3]))
  Total=c("Total",Total)
  late.summary.area=as.data.frame(rbind(late.summary.area,Total))
  
  #Round total pounds and update
  #Ensure all counts of crab are numeric not a character
  late.summary.area[,2:3]=late.summary.area[,2:3] %>%
    mutate_all(as.numeric)
  late.summary.area[1:11,3]=round_any(late.summary.area[1:11,3],1)
  
  #Update Total Pounds after rounding
  late.summary.area[12,3]=sum(late.summary.area$`Late Pounds (lbs.)`[1:11])
  
  #Update column names
  colnames(late.summary.area) <- c("Marine Area", "Total Late Crabs", "Total Late Pounds (lbs.)")
  
  late.catch.total=late.summary.area[12,3]
}
  
###########Calculate Non-respondent Catch ############
  
  #Pull in data frame with CRC Reporting Rates by Year
  CRCs="CRC_Reporting_Rates.xlsx"
  current.directory3<-paste0(getwd(),"/CRC Estimate Function Inputs/")
  yr.data=read_excel(paste0(current.directory3,CRCs))
  yr.data$yr.vec=as.numeric(yr.data$yr.vec)
  total <- yr.data[yr.data$yr.vec==Year, paste0("total.crc.",Season)]
  reported=yr.data[yr.data$yr.vec==Year,paste0("reported.crc.",Season)]
  total=as.numeric(total)
  reported=as.numeric(reported)
  
  Total.crc=total                        #Total card holders
  Reported.crc=reported                  #Total respondents
  non.respondents=Total.crc-Reported.crc #Total non-respondents
  Resp.rate=Reported.crc/Total.crc       #Response rate

  #Re-calculate non-respondents with late CRC holders
  non.respondents.adjusted=non.respondents-late.card.total
  
  #Pull in data frame for non-respondent catch estimation
  NR="NR_Inputs.xlsx"
  NR.data=read_excel(paste0(current.directory3,NR))
  
  #Filter NR inputs for current catch year
  NR.data=NR.data %>% filter(year==Year)
  #Filter NR inputs for current catch season
  NR.data=NR.data %>% filter(season.filt==Season)
  
  #Estimate NR catch using updated TWG method #1
  NR.catch.total.1=round_any((NR.data$avg.proportion.successful.NR)*non.respondents.adjusted*(NR.data$successful.respondent.mean.catch.per.crc),1)
  
  #Estimate NR catch using updated TWG method #2
  NR.catch.total.2=round_any((NR.data$avg.proportion.successful.NR)*non.respondents.adjusted*(NR.data$avg.mean.successful.trips.per.NR.read)*(NR.data$successful.respondent.catch.per.trip),1)
  
  # Average of these two methods
  NR.catch.total.Final=(NR.catch.total.1 + NR.catch.total.2)/2
  
  
########### Summarize late card and Non-respondent catch in a table ############
  
  #Assemble into a table
  Table3.output=as.data.frame(cbind(Year,Season,Total.crc,Reported.crc,Resp.rate,non.respondents.adjusted,late.card.total,late.catch.total,NR.catch.total.Final))

###########Create Table: Distribute Late Catch and Non-respondent Catch to a Marine Area and combine with Respondent catch ############
 
  Table4=Table2 #Create new table
  
  Table4$Total.Respondent.Crabs=rowSums(Table4[,2:4]) #Create new column with total census crabs
  
  Table4$Resp.catch.prop=Table4$Total.Respondent.Crabs/sum(Table4$Total.Respondent.Crabs) #By area, calculate the proportion of total catch based on respondent catch.
  
if(late.catch.available=="No")
{
  #Distribute late card catch to each Marine Area
  Table4$Late.Crabs=Table4$Resp.catch.prop*late.catch.total
  
  #Distribute non-respondent catch to a Marine Area
  Table4$NR.Crabs=Table4$Resp.catch.prop*NR.catch.total.Final
  
  Table4=Table4[,-c(2:4,6)] #Now can remove census catch by date category and catch proportion table
  
}
  
if(late.catch.available=="Yes")
{
  #Distribute non-respondent catch to a Marine Area
  Table4$NR.Crabs=Table4$Resp.catch.prop*NR.catch.total.Final
  
  Table4=Table4[,-c(2:4,6)] #Now can remove census catch by date category and catch proportion table
  
  #Add late crabs by marine area
  Table4=cbind(Table4,late.summary.area[1:11,2])
  #Change column name
  colnames(Table4)[colnames(Table4) == 'late.summary.area[1:11, 2]'] <- 'Late.Crabs'
  
}  
  
  
  ########## Add in Charter Catch for each Marine Area ############
  
  charter.data.name=paste0("Charter crab catch.xlsx")
  charter.catch=read_excel(paste0(current.directory3,charter.data.name),sheet = paste0(Year))
  
  charter.catch=charter.catch %>% filter(Season.filt==Season) #Filter for correct season
  
  unique(charter.catch$Area) #check areas in charter data
  
  #Apply zero catch to one trip for each MA.
  define.area.2=c(4,5,6,7,81,82,9,10,11,12,13)
  
  for (i in 1:11)
  {
    charter.catch[nrow(charter.catch) + 1,3] = c(define.area.2[i])
    charter.catch[nrow(charter.catch),2] = c(0)
  }
  
  #Sum charter catch by area
  detach("package:plyr", unload = TRUE)
  charter.catch=charter.catch %>%
    group_by(Area) %>%
    summarise(crabs=sum(Catch))
  
  #Re-order Marine Areas column and attach to Table
  library("plyr")
  charter.catch=charter.catch %>% arrange(factor(Area, levels = c('4', '5', '6', '7', '81', '82','9','10','11','12','13')))
  Table4$Charter.Catch=charter.catch$crabs
  
  ####Round, total crab, calculate pounds
  
  Table4.output=Table4
  
  #Ensure all counts of crab are numeric not a character
  Table4.output[,2:5]=Table4.output[,2:5] %>%
    mutate_all(as.numeric)
  
  #Round numbers
  Table4.output[1:11,2]=round_any(Table4.output[1:11,2],1)
  Table4.output[1:11,3]=round_any(Table4.output[1:11,3],1)
  Table4.output[1:11,4]=round_any(Table4.output[1:11,4],1)
  Table4.output[1:11,5]=round_any(Table4.output[1:11,5],1)
  
  #Create total crabs column
  Table4.output$Total.Crabs=rowSums(Table4.output[,2:5])
  
  #Create total pounds column
  Table4.output$`Pounds (lbs.)`=Table4.output$Total.Crabs*1.8
  
  #Round pounds
  Table4.output[1:11,7]=round_any(Table4.output[1:11,7],1)
  
  #Create total row for each column
  Total=c(colSums(Table4.output[,2:7]))
  Total=c("Total",Total)
  Table4.output=as.data.frame(rbind(Table4.output,Total))
  
########### Create Table: Split Marine Areas into catch areas and add EUC ############
  
  catch.splits.data.name=paste0("Region_Partition_Inputs.xlsx")
  catch.splits=read_excel(paste0(current.directory3,catch.splits.data.name),sheet = paste0(Year))
  unique(catch.splits$Catch.Area)
  
  Table5=Table4 #Define new table
  
  # Marine Area 4; Region 3-4:
  Table5[1,c(2:5)]=Table5[1,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA4.R34"]
  Table5$`Marine Area`[Table5$`Marine Area`=="4"]=catch.splits$Catch.Area[1]
  
  # Marine Area 5; Region 3-4:
  Table5[,c(2:5)][Table5$`Marine Area`=="5"]=Table5[,c(2:5)][Table5$`Marine Area`=="5"]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA5.R34"]
  Table5$`Marine Area`[Table5$`Marine Area`=="5"]=catch.splits$Catch.Area[2]
  
  # Marine Area 6; Region 1 Portion:
  Table5[12,c(2:5)] = Table5[3,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA6.R1"]
  Table5[12,1] = "MA6.R1"

  # Marine Area 6; Region 3-1 Portion:
  Table5[13,c(2:5)] = Table5[3,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA6.R31"]
  Table5[13,1] = "MA6.R31"
  
  # Marine Area 6; Region 3-2 Portion:
  Table5[14,c(2:5)] = Table5[3,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA6.R32"]
  Table5[14,1] = "MA6.R32"
  
  # Marine Area 6; Region 3-3 Portion:
  Table5[15,c(2:5)] = Table5[3,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA6.R33"]
  Table5[15,1] = "MA6.R33"
  
  # Marine Area 7; Region 1 Portion:
  Table5[16,c(2:5)] = Table5[4,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA7.R1"]
  Table5[16,1] = "MA7.R1"
  
  # Marine Area 6; Region 3-1 Portion:
  Table5[17,c(2:5)] = Table5[4,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA7.R31"]
  Table5[17,1] = "MA7.R31"
  
  # Marine Area 8-1; Region 2E Portion:
  Table5[18,c(2:5)] = Table5[5,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA81.R2E"]
  Table5[18,1] = "MA81.R2E"
  
  # Marine Area 8-2; Region 2E Portion:
  Table5[19,c(2:5)] = Table5[6,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA82.R2E"]
  Table5[19,1] = "MA82.R2E"
  
  # Marine Area 9; Region 2W Portion:
  Table5[20,c(2:5)] = Table5[7,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA9.R2W"]
  Table5[20,1] = "MA9.R2W"
  
  # Marine Area 9; Region 5 Portion:
  Table5[21,c(2:5)] = Table5[7,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA9.R5"]
  Table5[21,1] = "MA9.R5"
  
  # Marine Area 10; Region 4 Portion:
  Table5[22,c(2:5)] = Table5[8,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA10.R4"]
  Table5[22,1] = "MA10.R4"
  
  # Marine Area 11; Region 6 Portion:
  Table5[23,c(2:5)] = Table5[9,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA11.R6"]
  Table5[23,1] = "MA11.R6"
  
  # Marine Area 12; Region 5 Portion:
  Table5[24,c(2:5)] = Table5[10,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA12.R5"]
  Table5[24,1] = "MA12.R5"
  
  # Marine Area 13; Region 7 Portion:
  Table5[25,c(2:5)] = Table5[11,c(2:5)]*catch.splits$catch.portion[catch.splits$Catch.Area=="MA13.R7"]
  Table5[25,1] = "MA13.R7"
  
  #Remove Marine Areas
  Table5=Table5[-c(3:11),]
  
  ################################################EUC#########################
  #Add in EUC for each Marine Area ####
  
  euc.data.name=paste0("Region_EUC.xlsx")
  EUC.value=read_excel(paste0(current.directory3,euc.data.name),sheet = paste0(Year))
  
  EUC.value$EUC=as.numeric(EUC.value$EUC)
  
  Table5$EUC=EUC.value$EUC*(as.numeric(Table5$Total.Respondent.Crabs+Table5$Late.Crabs+Table5$NR.Crabs))
  
  #Move EUC in front of charter catch
  Table5 <- Table5 %>% relocate(EUC, .before = Charter.Catch)
  
  ####Round, total crab, calculate pounds
  
  Table5.output=Table5
  
  #Ensure all counts of crab are numeric not a character
  Table5.output[,2:6]=Table5.output[,2:6] %>%
    mutate_all(as.numeric)
  
  #Round numbers
  Table5.output[1:16,2]=round_any(Table5.output[1:16,2],1)
  Table5.output[1:16,3]=round_any(Table5.output[1:16,3],1)
  Table5.output[1:16,4]=round_any(Table5.output[1:16,4],1)
  Table5.output[1:16,5]=round_any(Table5.output[1:16,5],1)
  Table5.output[1:16,6]=round_any(Table5.output[1:16,6],1)
  
  #Create total crabs column
  Table5.output$Total.Crabs=rowSums(Table5.output[,2:6])
  
  #Create total pounds column
  Table5.output$`Pounds (lbs.)`=Table5.output$Total.Crabs*1.8
  
  #Round pounds
  Table5.output[1:16,8]=round_any(Table5.output[1:16,8],1)
  
  #Create total row for each column
  Total=c(colSums(Table5.output[,2:8]))
  Total=c("Total",Total)
  Table5.output=as.data.frame(rbind(Table5.output,Total))

###########Create Table: Convert to Crab Management Regions ############
  
  Table6=Table5.output[,c(1,8)] #Define new table
  
  Table6$`Pounds (lbs.)`=as.numeric(Table6$`Pounds (lbs.)`)
  
  # CMR 3-4:
  Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA4.R34"]=Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA4.R34"]+Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA5.R34"]
  Table6$`Marine Area`[Table6$`Marine Area`=="MA4.R34"]="3-4"
  Table6=Table6 %>% filter(!(`Marine Area`=="MA5.R34"))
  
  # CMR 3-1:
  Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA6.R31"]=Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA6.R31"]+Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA7.R31"]
  Table6$`Marine Area`[Table6$`Marine Area`=="MA6.R31"]="3-1"
  Table6=Table6 %>% filter(!(`Marine Area`=="MA7.R31"))
  
  # CMR 3-2:
  Table6$`Marine Area`[Table6$`Marine Area`=="MA6.R32"]="3-2"

  # CMR 3-3:
  Table6$`Marine Area`[Table6$`Marine Area`=="MA6.R33"]="3-3"
  
  # CMR 3-1:
  Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA6.R1"]=Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA6.R1"]+Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA7.R1"]
  Table6$`Marine Area`[Table6$`Marine Area`=="MA6.R1"]="1"
  Table6=Table6 %>% filter(!(`Marine Area`=="MA7.R1"))
  
  # CMR 2E:
  Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA81.R2E"]=Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA81.R2E"]+Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA82.R2E"]
  Table6$`Marine Area`[Table6$`Marine Area`=="MA81.R2E"]="2E"
  Table6=Table6 %>% filter(!(`Marine Area`=="MA82.R2E"))
  
  # CMR 2W:
  Table6$`Marine Area`[Table6$`Marine Area`=="MA9.R2W"]="2W"
  
  # CMR 5:
  Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA9.R5"]=Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA9.R5"]+Table6$`Pounds (lbs.)`[Table6$`Marine Area`=="MA12.R5"]
  Table6$`Marine Area`[Table6$`Marine Area`=="MA9.R5"]="5"
  Table6=Table6 %>% filter(!(`Marine Area`=="MA12.R5"))
  
  # CMR 4:
  Table6$`Marine Area`[Table6$`Marine Area`=="MA10.R4"]="4"
  
  # CMR 6:
  Table6$`Marine Area`[Table6$`Marine Area`=="MA11.R6"]="6"
  
  # CMR 7:
  Table6$`Marine Area`[Table6$`Marine Area`=="MA13.R7"]="7"
  
  
  #Re-order by Area
  Table6=Table6 %>% arrange(factor(Table6$`Marine Area`, levels = c('1', '2E', '2W', '3-1', '3-2', '3-3', '3-4','4','5','6','7','Total')))
  
  #Sum Pounds
  Table6$`Pounds (lbs.)`[12]=sum(Table6$`Pounds (lbs.)`[1:11])
  
  #Change column name
  colnames(Table6)[colnames(Table6) == 'Marine Area'] <- 'Crab Management Region'
  
  
#########Export as table############
  
  current.directory4<-paste0(getwd())
  
  dataset_names <- list('Sheet1' = Table1.output, 'Sheet2' = Table2.output, 'Sheet3' = Table3.output, 
                        'Sheet4' = Table4.output , 'Sheet5' = Table5.output, 'Sheet6' = Table6)
  
  Output.directory=paste0(current.directory4,paste0("/",Year,"_",Season,"_CRC_Tables.xlsx"))
  
  write_xlsx(dataset_names, Output.directory)
  
  
}


#!!!!!!!!!!!!!!#

#Manually adjust CRC year of interest and ensure input xlsx files are up to date

# 1) Ensure catch file is in correct format with columns ordered and name by: area, month, day, year, num_crab, ID
# 2) Ensure catch file name is: "Catch Data Season YEAR.xlsx"
# 3) Ensure "Seasons_Areas_Inputs.xlsx" file is up to date with the correct seasons by area for the correct year
# 4) Ensure "Closed_Areas_Inputs.xlsx" file is up to date with the correct closed areas listed as inputs for the correct year
# 5) Input year of interest below
# 6) Input season of interest below
# 7) If late CRC data is available for the year of interest, input 'Yes'.

#!!!!!!!!!!!!!!#

##Define the CRC year and season of interest
Year=2025
Season="Summer" #("Summer" or "Winter")

late.catch.available="Yes" 
#"Yes" (Entered late catch is available for the CRC year)
#"No" (Late catch is not available and need to estimate late catch)

#Define the CRC file name (Example: Catch Data Summer 2022.xlsx)
data.name=paste0("Catch Data"," ",Season," ",Year,".xlsx")


######RUN FUNCTION
CRC.Estimate.function(Year, Season, data.name)




