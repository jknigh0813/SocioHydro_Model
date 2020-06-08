SocHydModel <- function(
  W,             #annual maxima discharge
  H = 1000,      #flooding threshold (cfs)
  alphad = 1,    #risk taking attitude
  alphaa = 15,   #anxiousness
  alphap = 1,    #activeness
  alphar = 0.25, #effectiveness of preparedness
  Br = 1,        #discharge to loss
  mewa = 0.10,   #forgetfullness
  mewp = 0.04,   #decay rate of precautionary measures
  Dmax = 0.8,    #maximum settlement density
  Pmax = 0.8,    #maximum preparedness
  Rmax = 0.8,    #maximum relative loss
  Amax = 1.0,    #maximum awareness
  U_rate = 0.5  #growth rate
){
  
  #Initialize vectors
  POT = rep(0,length(W))          #Peak over Threshold
  L = rep(0,length(W))            #L - loss
  D = rep(0.5*Dmax,length(W))     #D - population density
  R = rep(0,length(W))            #R - relative loss
  U = rep(U_rate,length(W))       #U - growth rate of settlement density
  A = rep(0*Amax,length(W))     #A - awareness
  P = rep(0*Pmax,length(W))     #P - preparedness
  dDdt = rep(0,length(W))
  dPdt = rep(0,length(W))
  dAdt = rep(0,length(W))
  POTmax = 1.5*max(W) - H
    
  for (t in 2:length(W))
  {
    
    POT[t] = max(0,W[t] - H)
    
    #Relative loss
    if (POT[t] > 0)
    {R[t] = max(0,Rmax - Br*exp(-alphar*(Pmax - P[t-1])*(POT[t]/POTmax)))}
    else
    {R[t] = 0}

    L[t] = R[t]*D[t-1]
    
    dDdt[t] = U[t]*(1 - alphad*A[t-1])*D[t-1]*(1 - D[t-1]/Dmax)
    
    dAdt[t] = alphaa*L[t]*(1 - A[t-1]/Amax) - mewa*A[t-1]
    
    if (R[t] > 0)
    {dPdt[t] = alphap*dAdt[t]*(1 - P[t-1]/Pmax) - mewp*P[t-1]}
    else
    {dPdt[t] = -1*mewp*P[t-1]}
    
    D[t] = max(0,min(D[t-1] + dDdt[t],Dmax))
    A[t] = max(0,min(A[t-1] + dAdt[t],Amax))
    P[t] = max(0,min(P[t-1] + dPdt[t],Pmax))
    
  }
  
  SocHydResults<-data.frame(Q_Peak[,1],W,L,D,R,U,A,P) 
  return(SocHydResults)
}
