
#Load Packages
library(readxl)
library(rio)
library(dplyr)
library(data.table)
library(tidyr)
library(magrittr)
library(eeptools)
library(ggplot2)
library(cmocean)
library(patchwork)
library(R.utils)
library(tools)
library(writexl)
library(installr)
library(chron)
library(scales)
library(boot)
library(forcats)

#Assign directory to global inputs
current.directory<-paste0("ENTER DIRECTORY HERE")
#current.directory<-paste0(getwd())
setwd(current.directory)

############################################################################
############################################################################
############################################################################
############################################################################

set.seed(54321)

#2025 Data

# reading data

data.name=paste0("Creel_Data_Shared_2025_Updated.xlsx")
Input.directory<-paste0(current.directory,"//",data.name)
boat <- read_excel(Input.directory)
boat=as.data.frame(boat)

#Filter data for summer fishery only
boat.25=boat %>% filter(creel_date <= "2025-09-01")

#Combine 7N and 7S into 7
boat.25$marine_area[boat.25$marine_area=="7N"]="7"
boat.25$marine_area[boat.25$marine_area=="7S"]="7"

#More crab weighed than in possession?
check1=boat.25 %>% filter(n_dungeness_weighed>n_dungeness_retained) #No

#Check odd values
unique(boat.25$n_dungeness_weighed) #5.77 crab weighed
check2=boat.25 %>% filter(n_dungeness_weighed==5.77)
#One data entry error. Lets fix (after chat with sampler):
#Total weight should be: 17.15 lbs
#Total crab weighed should be: 8
boat.25$dungeness_total_weight[boat.25$n_dungeness_weighed==5.77]=17.15
boat.25$n_dungeness_weighed[boat.25$n_dungeness_weighed==5.77]=8


areas.25=unique(boat.25$marine_area)
areas.25=c(na.omit(areas.25))
MA.data.list.25=list()
boat.weights.25=list()
n.boats.25=c()
dung.weighed.25=c()
dung.weight.25=c()
weight.per.crab.25=c()
Year.25=c()
boot.weight.25=list()
weight.boot.lowerCI.25=c()
weight.boot.upperCI.25=c()
margin_error.25=c()
N.boats.25=c()
weight.boot.mean.25=c()
weight.boot.SD.25=c()

#set the number of bootstrap samplings
nboots<-10000

for(i in 1:length(areas.25))
{
  MA.data.list.25[[i]]=boat.25 %>% filter(marine_area==areas.25[i])
  
  #Total boats with a weight taken
  boat.weights.25[[i]]= MA.data.list.25[[i]] %>% filter(!(is.na(n_dungeness_weighed)))
  boat.weights.25[[i]]= boat.weights.25[[i]] %>% filter(!(is.na(dungeness_total_weight)))
  n.boats.25[i]=length(unique(na.omit(boat.weights.25[[i]]$Boat_ID)))
  
  #Total crabs weighed
  dung.weighed.25[i]=sum(MA.data.list.25[[i]]$n_dungeness_weighed,na.rm=T)
  
  #Total weight
  dung.weight.25[i]=sum(MA.data.list.25[[i]]$dungeness_total_weight,na.rm=T)
  
  #Mean weight per crab
  weight.per.crab.25[i]=dung.weight.25[i]/dung.weighed.25[i]
  
  Year.25[i]=2025
  
  #complete a bootstrap calculation for the means
  
   boot.weight.25[[i]]<-data.frame(matrix(NA,nrow=nboots,ncol=3))
  
  for(j in 1:nboots)
  {
    
    boot_dat<-MA.data.list.25[[i]][sample(nrow(MA.data.list.25[[i]]),n.boats.25[i],replace=T),]

    #Total crabs weighed
    dung.weighed.boot.25=sum(boot_dat$n_dungeness_weighed,na.rm=T)
    
    #Total weight
    dung.weight.boot.25=sum(boot_dat$dungeness_total_weight,na.rm=T)
    
    #Mean weight per crab
    boot.val.25=dung.weight.boot.25/dung.weighed.boot.25
    
    boot.weight.25[[i]][j,1]=areas.25[i]
    boot.weight.25[[i]][j,2]=j
    boot.weight.25[[i]][j,3]=boot.val.25
  }
  
  N.boats.25[i]=n.boats.25[i]
  weight.boot.mean.25[i]=mean(boot.weight.25[[i]][,3],na.rm=T)
  weight.boot.SD.25[i]=sd(boot.weight.25[[i]][,3],na.rm=T)
   
  weight.boot.lowerCI.25[i]=quantile(boot.weight.25[[i]][,3], probs = 0.025,na.rm=T)
  weight.boot.upperCI.25[i]=quantile(boot.weight.25[[i]][,3], probs =  0.975,na.rm=T)
  margin_error.25[i] <- (weight.boot.upperCI.25[i]  - weight.boot.lowerCI.25[i] ) / 2
}


table.25=as.data.frame(cbind(areas.25,Year.25,n.boats.25,dung.weighed.25,dung.weight.25,weight.per.crab.25,weight.boot.SD.25,margin_error.25,weight.boot.lowerCI.25,weight.boot.upperCI.25))
table.25$weight.per.crab.25=as.numeric(table.25$weight.per.crab.25)
table.25$weight.boot.SD.25=as.numeric(table.25$weight.boot.SD.25)

table.25=table.25 %>% arrange(factor(areas.25, levels = c('6', '7', '8_1', '8_2','9','10','11','12')))


#2024 Data

# reading data

data.name=paste0("/Creel_Data_Shared_2024.xlsx")
Input.directory<-paste0(current.directory,data.name)
boat <- read_excel(Input.directory)
boat=as.data.frame(boat)

#Filter data for summer fishery only
boat.24=boat %>% filter(creel_date <= "2024-09-02")

#Combine 7N and 7S into 7
boat.24$marine_area[boat.24$marine_area=="7N"]="7"
boat.24$marine_area[boat.24$marine_area=="7S"]="7"

#More crab weighed than in possession?
check1=boat.24 %>% filter(n_dung_weighed>n_dung_boat) #No

#Check odd values
unique(boat.24$n_dung_weighed) #13.57 and 7.11 crab weighed

check2=boat.24 %>% filter(n_dung_weighed==13.57)
#First data entry error. Lets fix (after chat with sampler):
#Total weight should be: 31.19 lbs
#Total crab weighed should be: 22
boat.24$dung_total_weight[boat.24$n_dung_weighed==13.57]=31.19
boat.24$n_dung_weighed[boat.24$n_dung_weighed==13.57]=22

#Second data entry error. Lets fix (after chat with sampler):
check2=boat.24 %>% filter(n_dung_weighed==7.11)
#Total weight should be: 12.13 lbs
#Total crab weighed should be: 8
boat.24$dung_total_weight[boat.24$n_dung_weighed==7.11]=12.13
boat.24$n_dung_weighed[boat.24$n_dung_weighed==7.11]=8

areas.24=unique(boat.24$marine_area)
areas.24=na.omit(areas.24)
MA.data.list.24=list()
n.boats.24=c()
dung.weighed.24=c()
dung.weight.24=c()
weight.per.crab.24=c()
Year.24=c()
boat.weights.24=list()
boot.weight.24=list()
weight.boot.lowerCI.24=c()
weight.boot.upperCI.24=c()
margin_error.24=c()
N.boats.24=c()
weight.boot.mean.24=c()
weight.boot.SD.24=c()
#set the number of bootstrap samplings
nboots<-10000

for(i in 1:length(areas.24))
{
  MA.data.list.24[[i]]=boat.24 %>% filter(marine_area==areas.24[i])
  
  #Total boats with a weight taken
  boat.weights.24[[i]]= MA.data.list.24[[i]] %>% filter(!(is.na(n_dung_weighed)))
  boat.weights.24[[i]]= boat.weights.24[[i]] %>% filter(!(is.na(dung_total_weight)))
  n.boats.24[i]=length(unique(na.omit(boat.weights.24[[i]]$Boat_ID)))
  
  #Total crabs weighed
  dung.weighed.24[i]=sum(MA.data.list.24[[i]]$n_dung_weighed,na.rm=T)
  
  #Total weight
  dung.weight.24[i]=sum(MA.data.list.24[[i]]$dung_total_weight,na.rm=T)
  
  #Mean weight per crab
  weight.per.crab.24[i]=dung.weight.24[i]/dung.weighed.24[i]
  
  Year.24[i]=2024
  
  #complete a bootstrap calculation for the means
  
  boot.weight.24[[i]]<-data.frame(matrix(NA,nrow=nboots,ncol=3))
  
  for(j in 1:nboots)
  {
    
    boot_dat.24<-MA.data.list.24[[i]][sample(nrow(MA.data.list.24[[i]]),n.boats.24[i],replace=T),]
    
    #Total crabs weighed
    dung.weighed.boot.24=sum(boot_dat.24$n_dung_weighed,na.rm=T)
    
    #Total weight
    dung.weight.boot.24=sum(boot_dat.24$dung_total_weight,na.rm=T)
    
    #Mean weight per crab
    boot.val.24=dung.weight.boot.24/dung.weighed.boot.24
    
    boot.weight.24[[i]][j,1]=areas.24[i]
    boot.weight.24[[i]][j,2]=j
    boot.weight.24[[i]][j,3]=boot.val.24
  }
  
  N.boats.24[i]=n.boats.24[i]
  weight.boot.mean.24[i]=mean(boot.weight.24[[i]][,3],na.rm=T)
  weight.boot.SD.24[i]=sd(boot.weight.24[[i]][,3],na.rm=T)
  
  weight.boot.lowerCI.24[i]=quantile(boot.weight.24[[i]][,3], probs = 0.025,na.rm=T)
  weight.boot.upperCI.24[i]=quantile(boot.weight.24[[i]][,3], probs =  0.975,na.rm=T)
  margin_error.24[i] <- (weight.boot.upperCI.24[i]  - weight.boot.lowerCI.24[i] ) / 2
  
}


table.24=as.data.frame(cbind(areas.24,Year.24,n.boats.24,dung.weighed.24,dung.weight.24,weight.per.crab.24,weight.boot.SD.24,margin_error.24,weight.boot.lowerCI.24,weight.boot.upperCI.24))
table.24$weight.per.crab.24=as.numeric(table.24$weight.per.crab.24)
table.24$weight.boot.SD.24=as.numeric(table.24$weight.boot.SD.24)

table.24=table.24 %>% arrange(factor(areas.24, levels = c('6', '7', '8_1', '8_2','9','10','11','12')))



#2023 Data

# reading data

data.name=paste0("/Creel_Data_Shared_2023_Updated.xlsx")
Input.directory<-paste0(current.directory,data.name)
boat <- read_excel(Input.directory)
boat=as.data.frame(boat)

#Filter data for summer fishery only
boat=boat %>% filter(creel_date <= "2023-09-04")

#Combine 7N and 7S into 7
boat$marine_area[boat$marine_area=="7N"]="7"
boat$marine_area[boat$marine_area=="7S"]="7"

boat.23=boat %>% filter(species=="dungeness")
boat.23=boat.23 %>% filter(weight > 0)

#means=boat.23 %>%
#  group_by(marine_area) %>%
#  summarise(crabs.weighed=length(unique(na.omit(crab_ID))),
#            weight=mean(weight),
#            vessels=length(unique(na.omit(boat_intveriew_ID))))


#Bootstrap to get variance

areas.23=unique(boat.23$marine_area)
MA.data.list.23=list()
n.boats.23=c()
crabs.weighed.23=c()
weight.per.crab.23=c()

nboots=10000
boot.weight.23=list()
boot_dat.23=list()
N.boats.23=c()
weight.boot.mean.23=c()
weight.boot.SD.23=c()
weight.boot.lowerCI.23=c()
weight.boot.upperCI.23=c()
margin_error.23=c()
Year.23=c()

for(i in 1:length(areas.23))
{
  MA.data.list.23[[i]]=boat.23 %>% filter(marine_area==areas.23[i])
  
  n.boats.23[i]=length(unique(na.omit(MA.data.list.23[[i]]$boat_intveriew_ID)))
  crabs.weighed.23[i]=length(unique(na.omit(MA.data.list.23[[i]]$crab_ID)))
  weight.per.crab.23[i]=mean(MA.data.list.23[[i]]$weight,na.rm=T)
  
  Year.23[i]=2023
  
  #complete a bootstrap calculation for the means

  boot.weight.23[[i]]<-data.frame(matrix(NA,nrow=nboots,ncol=3))
  
  for(j in 1:nboots)
  {
    
    boot_dat.23<-MA.data.list.23[[i]][sample(nrow(MA.data.list.23[[i]]),n.boats.23[i],replace=T),]
    
    #Total crabs weighed
    dung.weighed.boot.23=length(unique(na.omit(boot_dat.23$crab_ID)))
    
    #Mean weight per crab
    boot.val.23=mean(boot_dat.23$weight,na.rm=T)
    
    boot.weight.23[[i]][j,1]=areas.23[i]
    boot.weight.23[[i]][j,2]=j
    boot.weight.23[[i]][j,3]=boot.val.23
  }
  
  N.boats.23[i]=n.boats.23[i]
  weight.boot.mean.23[i]=mean(boot.weight.23[[i]][,3],na.rm=T)
  weight.boot.SD.23[i]=sd(boot.weight.23[[i]][,3],na.rm=T)
  
  weight.boot.lowerCI.23[i]=quantile(boot.weight.23[[i]][,3], probs = 0.025,na.rm=T)
  weight.boot.upperCI.23[i]=quantile(boot.weight.23[[i]][,3], probs =  0.975,na.rm=T)
  margin_error.23[i] <- (weight.boot.upperCI.23[i]  - weight.boot.lowerCI.23[i] ) / 2
  
  
}

table.23=as.data.frame(cbind(areas.23,Year.23,n.boats.23,crabs.weighed.23,weight.per.crab.23,weight.boot.SD.23,margin_error.23,weight.boot.lowerCI.23,weight.boot.upperCI.25))
table.23$weight.per.crab.23=as.numeric(table.23$weight.per.crab.23)
table.23$weight.boot.SD.23=as.numeric(table.23$weight.boot.SD.23)

table.23=table.23 %>% arrange(factor(areas.23, levels = c('6', '7', '8_1', '8_2','9','10','11','12')))


write.csv(table.25,'weight_table_2025.csv')
write.csv(table.24,'weight_table_2024.csv')
write.csv(table.23,'weight_table_2023.csv')


#####################################################
# Updated MOE analysis - running only the 2025 data
#####################################################
set.seed(123456)

#testing this all with all the data


#set parameters
#n boots for original MOE calcs
n.boots=2000 #choosing this number for computation efficiency

#lower and upper SS for experiments
min.n<-50
max.n<-1500
bin<-10
target.moe.per<-0.05

#Selecting data for the loops - only using the 2025 data.
dat.select<-boat.weights.25
area.dat<-areas.25


#make a container to save the results for each area
save.results <- vector("list", length(area.dat))

#iterating through the areas
for(i in 1:length(area.dat)){
  
  #selecting the data to use and the n boats for the final estimate
  dat<-data.frame(dat.select[[i]])
  n.boats<-as.numeric(length(unique(na.omit(dat$Boat_ID))))
  
  # EUC calculation function
  test.fun<-function(dat=dat){

      #Total crabs weighed
      dung.weighed=sum(dat$n_dungeness_weighed,na.rm=T)
      
      #Total weight
      dung.weight=sum(dat$dungeness_total_weight,na.rm=T)
      
      #Mean weight per crab
      dung.weight/dung.weighed
      

  }
  
  test.fun(dat)
  
  #########################
  
  # Calculating the bootstrap MOE
  bootstrap_moe <- function(dat=dat,
                            n.boats=n.boats,
                            estimator=test.fun,
                            n.boot = n.boots,
                            conf.level = 0.95){
    
    estimates <- numeric(n.boot)
    

      for(b in 1:n.boot){
        
        # resample boats
        boot_dat <- dat[sample(
          1:nrow(dat),
          size = n.boats,
          replace = TRUE
        ), ]
        
        estimates[b] <- estimator(boot_dat)
      }
      
      alpha <- 1 - conf.level
      critical_value <- qt(1 - alpha / 2, df = n.boats - 1L)
      estimate = mean(estimates)
      MOE = critical_value * sd(estimates)
      lower = estimate - MOE
      upper = estimate + MOE
      
      list(
        estimate = mean(estimates),
        SE = sd(estimates),
        MOE = MOE,
        lower = lower,
        upper = upper)
    }
  
  
  
  
  
 #bootstrap_moe(dat = dat,n.boats = n.boats, estimator = test.fun)
  
  ############
  
  find_sample_size_boot <- function(dat,
                                    estimator=test.fun,
                                    target.moe,
                                    start.n = min.n,
                                    max.n = max.n,
                                    n.boot = n.boots,
                                    conf.level = 0.95){
    
    results <- data.frame(
      n.boats = integer(),
      MOE = numeric(),
      estimate=numeric()
    )
    
    for(n in seq(start.n, max.n, by = bin)){
      
      boot <- bootstrap_moe(
        dat = dat,
        n.boats = n,
        estimator = estimator,
        n.boot = n.boot,
        conf.level = conf.level
      )
      
      results <- rbind(
        results,
        data.frame(
          n.boats = n,
          MOE = boot$MOE,
          estimate=boot$estimate
        )
      )
      
      cat("n =", n,
          "MOE =", round(boot$MOE,4), "\n")
      
      #if(boot$MOE <= target.moe){
      
      # return(list(
      #  required.n = n,
      #  results = results,
      #  final.bootstrap = boot
      #))
      #    }
    }
    
    #  warning("Target MOE not reached within available sample size")
    
    results
  }
  
  
  #ss calculator
  result <- find_sample_size_boot(
    dat = dat,
    estimator = test.fun,
    target.moe = target.moe.per * test.fun(dat),
    start.n = min.n,
    max.n = max.n,
    n.boot = n.boots)
  
  
  result<-as.data.frame(result)
  result$emp.est<-test.fun(dat)
  result$percentMOE<-result$MOE/result$emp.est
  result$area<-area.dat[i]
  
  save.results[[i]]<-result
  
}


#make a plot
moe.boot.data.wt<-do.call(rbind,save.results)
moe.boot.data.wt$area<-factor(moe.boot.data.wt$area,levels=c("6","7", "8_1", "8_2","9", "10", "11", "12"))

write.csv(moe.boot.data.wt,'moe_boot_data_25_wt.csv')

ggplot(moe.boot.data.wt, aes(x=n.boats,y=percentMOE,group=area))+
  geom_hline(yintercept=0.02,lty=2,linewidth=1,col='black')+
  geom_hline(yintercept=0.05,lty=2,linewidth=1,col='blue')+
  geom_hline(yintercept=0.10,lty=2,col='red')+
  geom_line(aes(colour = area),linewidth = 1)+
  theme_bw()


ggsave(paste0('moe_boot_data_wt.png'))








