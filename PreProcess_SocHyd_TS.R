preprocess <- function(i)
{
  
  master = read.csv("//nfs/jknighton-data/SocHyd/MasterList_Cities.csv")
  riverine = master[master$Keep == 1,]
  
  #City properties
  GEOID = riverine$GEOID[i]
  state = as.character(riverine$State[i])
  usgsgage = as.character(riverine$USGS[i])
  if (nchar(usgsgage) == 7) {usgsgage = paste("0",usgsgage,sep="")}
  
  #Read census populations
  Census = read.csv(paste("//nfs/jknighton-data/SocHyd/US_Census_Processed/Census_",GEOID,".csv",sep=""),header=FALSE)
  Census[,3] = Census[,2]/(1.25*max(Census[,2]))
  
  #Read Median Home Values
  HV = read.csv("//nfs/jknighton-data/SocHyd/State_HouseValue.csv")
  State_HV = HV[HV[,1] == state,]
  x = matrix(nrow = 8,ncol=2)
  x[,1] = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2017)
  x[,2] = t(State_HV[1,2:9])
  MHV <- approx(x[,1], x[,2],xout=seq(from=1950,to=2017,by=1))
  
  #Read Insurance Claims
  Claims = read.csv(paste("//nfs/jknighton-data/SocHyd/FEMA_Claims_Processed/FEMA_Claims_",GEOID,".csv",sep=""),skip=0,header=TRUE)
  for (rec in 1:length(Claims[,1]))
  {
    year = Claims$V1[rec]
    pop = 0.1*max(Census[,2])
    if (year <= 2017) {MHV_year = MHV$y[MHV$x == year]}
    if (year > 2017) {MHV_year = MHV$y[MHV$x == 2017]}
    THV = MHV_year*pop/2.5 #assumes 2.5 people per home
    Claims[rec,5] = THV 
    Claims[rec,6] = Claims[rec,4]/THV
  }
  
  #Read Insurance Policies
  Policies = read.csv(paste("//nfs/jknighton-data/SocHyd/FEMA_Policies_Processed/FEMA_Policies_",GEOID,".csv",sep=""),skip=0,header=TRUE)
  for (year in 2010:2018)
  {Policies[Policies$V1 == year,4] = Policies[Policies$V1 == year,3]/(0.1*Census$V2[Census$V1 == year])}
  Policies[Policies$V1 == 2019,4] = Policies[Policies$V1 == 2019,3]/(0.1*Census$V2[Census$V1 == 2018])
  Policies[Policies$V1 == 2009,4] = Policies[Policies$V1 == 2009,3]/(0.1*Census$V2[Census$V1 == 2010])  
  
  TS = data.frame(matrix(nrow=2019-1978))
  TS$year = seq(1979,2019,1)
  TS$census = array(0,2019-1978)
  TS$claims = array(0,2019-1978)
  TS$policies = array(0,2019-1978)
  counter = 1
  for (year in 1979:2019)
  {
    if (year >= 2010 & year < 2019) {TS$census[counter] = Census$V3[Census$V1 == year]}
    TS$claims[counter] = Claims$V6[Claims$V1 == year]
    if (year >= 2010) {TS$policies[counter] = Policies$V4[Policies$V1 == year]}
    counter = counter + 1
  }
  TS = TS[,-1]
  
  write.table(TS,paste("//nfs/jknighton-data/SocHyd/CalibData/Data_TS_",GEOID,".csv",sep=""),col.names=FALSE,row.names = FALSE,sep=",")
  return(TS)

} 
