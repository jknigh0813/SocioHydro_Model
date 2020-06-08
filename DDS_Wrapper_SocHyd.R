
library(dataRetrieval)

# Returns numIter length list of entries to be peturbed
probPeturb<-function(x, numIter){
  # Input is xBounds & numIter.  
  # Returns numIter entry list with the indices which will be peturbed
  xDims<-nrow(x)
  probabilityVector<-1-log(1:numIter)/log(numIter)
  peturbIdx<-apply(matrix(unlist(lapply(probabilityVector, function(x) as.logical(rbinom(xDims, 1, x)))), byrow=TRUE, ncol=xDims), 1, which)
  return(peturbIdx)
}

#Read in index
master = read.csv("//nfs/jknighton-data/SocHyd/MasterList_Cities.csv")
riverine = master[master$Keep == 1,]

#DDS parameters
r= 0.2
numIter = 50000

source("/research-home/jknighton/SocHyd/Socio_Hyd_Model.R")
source("/research-home/jknighton/SocHyd/PreProcess_SocHydTS.R")
source("/research-home/jknighton/SocHyd/NSE.R")
source("/research-home/jknighton/SocHyd/RMSE.R")

for (j in 1:nrow(riverine))
{
  
  GEOID = riverine$GEOID[j]
  state = as.character(riverine$State[j])
  usgsgage = as.character(riverine$USGS[j])
  if (nchar(usgsgage) == 7) {usgsgage = paste("0",usgsgage,sep="")}
  
  #Read in processed time series data for city population, claims, and policies
  CalibData = preprocess(j)
  
  #Read in USGS daily discharge -> peak annual maxima
  Q <- readNWISdv(usgsgage, "00060", "1950-01-01", "2019-12-31")
  Years = as.numeric(format(Q$Date,'%Y'))
  Q_Peak = matrix(nrow = 70,ncol=2)
  counter = 1
  for (year in 1950:2019)
  {
    Q_Peak[counter,1] = year
    Q_Peak[counter,2] = max(Q$X_00060_00003[Years == year])
    counter = counter + 1
  }
  Q_Peak[Q_Peak[,2] < 0,2] = 0 #Replace missing USGS discharge data with 0
  Q_Peak[is.na(Q_Peak[,2]),2] = 0
  
  W = Q_Peak[,2]*0.000408735      #convert cfs to m^3day^-1
  Wmin = max(0,0.9*min(W))
  Wmax = 1.1*max(W)               #Discharge causing probable maximum loss
  
  #Set calibration parameter boundaries
  xBounds.df = data.frame(matrix(ncol=2,nrow=8))
  colnames(xBounds.df)<-c("min", "max")
  
  #H
  xBounds.df$min[1] = 0
  xBounds.df$max[1] = Wmax
  
  #alphad
  xBounds.df$min[2] = 0.001
  xBounds.df$max[2] = 100
  
  #alphaa
  xBounds.df$min[3] = 0.001
  xBounds.df$max[3] = 25
  
  #alphap
  xBounds.df$min[4] = 0.001
  xBounds.df$max[4] = 3
  
  #alphar
  xBounds.df$min[5] = 0.001
  xBounds.df$max[5] = 3
  
  #mewa
  xBounds.df$min[6] = 0.001
  xBounds.df$max[6] = 3
  
  #mewp
  xBounds.df$min[7] = 0.001
  xBounds.df$max[7] = 3
  
  #U_rate
  xBounds.df$min[8] = 0.001
  xBounds.df$max[8] = 5
  
  # Generate initial first guess
  x_init<-c(1, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)
  x_best = data.frame(x_init)
  NSE_best <- -999
  
  peturbIdx<-probPeturb(xBounds.df, numIter)
  # Peturb each entry by N(0,1)*r(x_max - x_min) reflecting if beyond boundaries
  sigma<-xBounds.df$max - xBounds.df$min
  
  for (i in 1:numIter){
    # Set up test parameter values as x_test
    x_test<-as.matrix(x_best)
    
    # Get entries we will peturb
    idx<-peturbIdx[[i]]
    if (sum(idx) == 0) {idx = round(runif(1,1,8))}
    
    # Initialize vector of peturbations initially zeros with same length of x so we will add this vector to peturb x
    peturbVec<-rep(0, length(x_test))
    # Generate the required number of random normal variables
    N<-rnorm(length(x_test), mean=0, sd=1)
    
    # Set up vector of peturbations
    peturbVec[idx]<-r*N[idx]*sigma[idx]
    
    # Temporary resulting x value if we peturbed it
    testPeturb<-x_test + peturbVec  
    # Find the values in testPeturb that have boundary violations.  Store the indices in boundaryViolationsIdx
    boundaryViolationIdx<-which(testPeturb<xBounds.df$min | testPeturb > xBounds.df$max)
    
    # Reset those violated indices to the opposite peturbation direction
    peturbVec[boundaryViolationIdx]<-(-1*r*N[boundaryViolationIdx]*sigma[boundaryViolationIdx])
    
    # Find values still at violations of min or max and set them to the minimum or maximum values
    x_test<-x_test + peturbVec
    minViolationIdx<-which(x_test<xBounds.df$min)
    maxViolationIdx<-which(x_test>xBounds.df$max)
    x_test[minViolationIdx]<-xBounds.df$min[minViolationIdx]
    x_test[maxViolationIdx]<-xBounds.df$max[maxViolationIdx]
    
    #Socio-hydrological model    
    Results = SocHydModel(W, x_test[1], x_test[2], x_test[3], x_test[4], x_test[5], 0.8, x_test[6], x_test[7], 
                          0.8, 0.8, 0.8, 0.8, x_test[8])
    
    #Obj. Function Claims
    NSE_C = NSE(Results[30:70,3],CalibData$claims)
    NSE_D = NSE(Results[61:69,4],CalibData$census[32:40])
    NSE_P = NSE(Results[61:70,8],CalibData$policies[32:41])
    RMSE_C = RMSE(Results[30:70,3],CalibData$claims)
    RMSE_D = RMSE(Results[61:69,4],CalibData$census[32:40])
    RMSE_P = RMSE(Results[61:70,8],CalibData$policies[32:41])
    NSE_Test = NSE_C + NSE_P
    
    #Check if this simulation is better
    if (NSE_Test > NSE_best)
    {
      x_best = x_test
      NSE_best = NSE_Test
      RMSE_C_best = RMSE_C
      RMSE_P_best = RMSE_P
      RMSE_D_best = RMSE_D
      NSE_C_best = NSE_C
      NSE_D_best = NSE_D
      NSE_P_best = NSE_P
    }
  }
  print_str = paste("GEOID:",GEOID,"   NSE Best:",round(NSE_best,digits=5))
  print(print_str)
  
  Results = SocHydModel(W, x_best[1], x_best[2], x_best[3], x_best[4], x_best[5], 0.8, x_best[6], x_best[7], 
                        0.8, 0.8, 0.8, 0.8, x_best[8])
  
  par(mfrow=c(4,1))
  par(mar = c(2,5,1,1))
  plot(seq(1,70),Results$W,type="l",col="blue")
  lines(seq(1,70),rep(x_best[1],70))
  plot(seq(1,70),Results$L,ylim=c(0,max(Results$L,CalibData$claims)),type="l")
  points(seq(30,70),CalibData$claims,col="red")
  plot(seq(1,70),Results$D,ylim=c(0,1),type="l")
  points(seq(30,70),CalibData$census,col="red")
  plot(seq(1,70),Results$P,ylim=c(0,max(Results$P,CalibData$policies)),type="l")
  points(seq(30,70),CalibData$policies,col="red")
  
  #Save results
  riverine$H[j] = x_best[1]
  riverine$alphad[j] = x_best[2]
  riverine$alphaa[j] = x_best[3]
  riverine$alphap[j] = x_best[4]
  riverine$alphar[j] = x_best[5]
  riverine$mewa[j] = x_best[6]
  riverine$mewp[j] = x_best[7]
  riverine$U_rate[j] = x_best[8]
  riverine$RMSE_C[j] = RMSE_C_best
  riverine$RMSE_P[j] = RMSE_P_best
  riverine$RMSE_D[j] = RMSE_D_best
  riverine$NSE_C[j] = NSE_C_best
  riverine$NSE_P[j] = NSE_P_best
  riverine$NSE_D[j] = NSE_D_best
  
  write.table(riverine,"//nfs/jknighton-data/SocHyd/DDS_SocHyd_CP_v2.csv",sep=",")
}    
