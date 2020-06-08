library(tidyr)
library(data.table)
library(bit64)

GEOID_All = read.csv("E:/FloodMemory/Memory_ByCity/CBSA_MetroAreas.csv")
sitelist = GEOID_All[GEOID_All$Keep == 1,]
lookup = read.csv("E:/FloodMemory/Memory_ByCity/tracts-cbsa-lookup.csv")

#Read in FEMA Open Claims Data
infile = "E:/FloodMemory/FIMA_NFIP_Redacted_Claims_Data_Set/openFEMA_claims20190731.csv"
Claims = read.csv(infile)
Claims$amountpaidonbuildingclaim[is.na(Claims$amountpaidonbuildingclaim)] <- 0
Claims$amountpaidoncontentsclaim[is.na(Claims$amountpaidoncontentsclaim)] <- 0

NFIP_Count = 0
NFIP_USD = 0

for (i in 55:nrow(sitelist))
{
    GEOID = lookup[which(lookup$GEOID == sitelist$cbsa_geoid[i]),]
    print(paste(i,": GEOID ",GEOID$GEOID[1]," claims",sep=""))
    
    GEOID_Claims = Claims[Claims$reportedcity %in% as.character(GEOID$reportedcity),]
    GEOID_Claims = GEOID_Claims[GEOID_Claims$censustract %in% GEOID$censustract, ]
    
    #1. Process Claims for GEOID
    City_Claims = matrix(nrow=41, ncol=3)
    counter = 1
    for (year in 1979:2019)
    {
      GEOID_Claims_Year = GEOID_Claims[GEOID_Claims$yearofloss == year,]
      City_Claims[counter,1] = year  
      City_Claims[counter,2] = nrow(GEOID_Claims_Year)
      City_Claims[counter,3] = sum(GEOID_Claims_Year$amountpaidonbuildingclaim,GEOID_Claims_Year$amountpaidoncontentsclaim)  
      counter = counter + 1
    }
    
    #Export claims records
    write.csv(City_Claims,paste("E:/FloodMemory/Memory_ByCity/FEMA_Claims_Processed/FEMA_Claims_",GEOID$GEOID[1],".csv",sep=""))

    
    #2. Process Policies
    #File format to merge in to
    GEOID_Policies = read.csv(file="E:/FloodMemory/FEMA_Policy/openFEMA_policies20190831_1.csv",nrows=1,skip=1,header=FALSE)
    
    #Read in FEMA Open policies, keep only GEOID of interest
    for (plcy in 1:11)
    {
      print(paste(i,": GEOID ",GEOID$GEOID[1]," policy: ",plcy," of 11",sep=""))
      infile = paste("E:/FloodMemory/FEMA_Policy/openFEMA_policies20190831_",plcy,".csv",sep="")
      Policies = fread(infile,integer64="double",skip=1,header=FALSE)
      #Policies = read.csv(infile,header=FALSE)
      GEOID_Policies_temp = Policies[Policies$V41 %in% as.character(GEOID$reportedcity),]
      GEOID_Policies_temp = GEOID_Policies_temp[GEOID_Policies_temp$V5 %in% GEOID$censustract,]
      GEOID_Policies = rbind(GEOID_Policies,GEOID_Policies_temp)
      gc()
    }

    GEOID_Policies = data.frame(GEOID_Policies)
    GEOID_Policies$V32 = as.Date(GEOID_Policies$V32)
    GEOID_Policies$V33 = as.Date(GEOID_Policies$V33)
    
    #Summarize policies by year
    GEOID_Policies_Summary = matrix(nrow=11, ncol=2)
    counter = 1
    for (year in 2009:2019)
    {
      City_year = GEOID_Policies[which(as.Date(GEOID_Policies$V32) < as.Date(ISOdate(year, 12, 31))),]
      City_year = City_year[which(as.Date(City_year$V33) > as.Date(ISOdate(year, 1, 1))),]
      GEOID_Policies_Summary[counter,1] = year
      GEOID_Policies_Summary[counter,2] = nrow(City_year)
      counter = counter + 1
    }
    
    #Export policy records
    write.csv(GEOID_Policies_Summary,paste("E:/FloodMemory/Memory_ByCity/FEMA_Policies_Processed/FEMA_Policies_",GEOID$GEOID[1],".csv",sep=""))
  
}  

