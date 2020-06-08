

cbsa = read.csv("E:/FloodMemory/Memory_ByCity/CBSA_MetroAreas.csv")
cbsa = cbsa[cbsa$Keep == 1,]

AllData = read.csv("E:/FloodMemory/Memory_ByCity/cbsa_population_long.csv")

Output = data.frame("Year" = rep(0,9),"Population" = rep(0,9))
for (i in 1:nrow(cbsa))
{
  
  CBSA_Pop = AllData[AllData$CBSA == cbsa$cbsa_geoid[i],]  
  
  counter = 1
  for (year in 2010:2018)
  {
    CBSA_Pop_Year = CBSA_Pop[CBSA_Pop$year == year,]  
    if (year == 2010) {CBSA_Pop_Year = CBSA_Pop_Year[CBSA_Pop_Year$estimate_type == "CENSUS",]}
    else {CBSA_Pop_Year = CBSA_Pop_Year[CBSA_Pop_Year$estimate_type == "POPESTIMATE",]}
    Population = sum(CBSA_Pop_Year$population)
    
    Output$Year[counter] = year
    Output$Population[counter] = Population
    counter = counter + 1
    
  }
  
  write.table(Output,paste("E:/FloodMemory/Memory_ByCity/US_Census_Processed/Census_",cbsa$cbsa_geoid[i],".csv",sep=""),row.names = FALSE,col.names = FALSE,sep=",")
  
}
