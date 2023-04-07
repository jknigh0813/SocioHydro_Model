setwd("C:/SANDEEP/SocioHydro_Model/Data")
# Battery = read.csv("Max_Water_Level_Battery.csv")
# precip = read.table("Max_precip_LIS.csv")

SocHydModel_SurgePrecipHousing <- function(
        W,             #annual maxima discharge
        Precip,        #annual maxima rainfall
        Surgethreshold = 2,      #flooding threshold (storm surge)
        Rainthreshold = 0.1, #flooding threshold (precipitation)
        alphad = 1,    #risk taking attitude
        alphaa = 15,   #anxiousness
        alphap = 1,    #activeness
        alphar = 0.25, #effectiveness of preparedness
        Br = 0.2,        #flood to loss
        mewa = 0.10,   #forgetfulness
        mewp = 0.04,   #decay rate of precautionary measures
        Dmax = 1.0,    #maximum settlement density
        Pmax = 1.0,    #maximum preparedness
        Rmax = 1.0,    #maximum relative loss
        Amax = 1.0,    #maximum awareness
        U_rate = 0.5,  #growth rate
        POT_S_max = 3, #3.5 m
        POT_R_max = 0.15, #0.15 m
        
        #housing
        phi = 10, #factor by which housing price drops after loss, phi ranges from 20 to 100?
        HPmax = 1
){
    #Initialize vectors
    POT = rep(0,length(W))          #Peak over Threshold
    POT_S = max(W) - Surgethreshold
    POT_R = max(Precip) - Rainthreshold
    L = rep(0,length(W))            #L - loss
    D = rep(0.1 * Dmax,length(W))     #D - population density
    D[1] = Claims_Rescaled_d[1]
    R = rep(0,length(W))            #R - relative loss
    U = rep(U_rate,length(W))       #U - growth rate of settlement density
    A = rep(0.3*Amax,length(W))     #A - awareness
    P = rep(0.3*Pmax,length(W))     #P - preparedness
    dDdt = rep(0,length(W))
    dPdt = rep(0,length(W))
    dAdt = rep(0,length(W))
    
    #housing
    HP = rep(0.25 * HPmax,length(W)) #what should be our initial HP values ?
    tflood = rep(50,length(W))
    Sloss = 0
    
    
    for (t in 1:length(W))
    { 
        POT_S[t] = max(0,1*W[t] - Surgethreshold)
        POT_R[t] = max(0,1*Precip[t] - Rainthreshold)
    }
    
    POT_S_Scaled = POT_S/POT_S_max
    POT_R_Scaled = POT_R/POT_R_max
    
    
    for (t in 1:length(W))
    {
        POT[t] = max(POT_S_Scaled[t],POT_R_Scaled[t])
    }
    
    #sociohydro loop
    for (t in 2:length(W))
    {
        #Relative loss
        if( POT[t] > 0)
        {R[t] = max(0,Rmax - Br*exp(-alphar*(Pmax - P[t-1])*POT[t]))}
        else
        {R[t] = 0}
        
        L[t] = R[t] * D[t-1] * HP[t-1] #housing
        
        dDdt[t] = U[t]*(1 - alphad*A[t-1])*D[t-1]*(1 - D[t-1]/Dmax)
        
        dAdt[t] = alphaa*L[t]*(1 - A[t-1]/Amax) - mewa*A[t-1]
        
        if (R[t] > 0)
        {dPdt[t] = alphap*dAdt[t]*(1 - P[t-1]/Pmax) - mewp*P[t-1]
        tflood[t] = 0}
        else
        {dPdt[t] = -1*mewp*P[t-1]
        tflood[t] = tflood[t-1] + 1}
        
        #housing
        
        if (R[t] == 0)
        {
            HP[t] = HP[t-1] + ((1/(1 + exp(-1*tflood[t-1]+6)) - 1/(1 + exp(-1*tflood[t-1]+7))))*(HPmax - Sloss) 
            tflood[t] = tflood[t-1] + 1
        }
        
        else
        {
            
            HP[t] = HP[t-1] - L[t]*HP[t-1]*phi #Reduces S by a factor phi (However in this case phi is independent to loss so the drop in housing price becomes independent of the magnitude of loss ?)
            tflood[t] = 0 #restarts counter from last loss event
            Sloss = HP[t] #marks the new sigmoid minimum as S immediately after loss
        }
        
        D[t] = max(0.5 * D[1], min(D[t-1] + dDdt[t],Dmax))
        A[t] = max(0, min(A[t-1] + dAdt[t],Amax))
        P[t] = max(0, min(P[t-1] + dPdt[t],Pmax))
        HP[t] = max(0.1, min(HP[t],HPmax))
        
    }
    
    SocHydResults<-data.frame(Year = Battery$Year, W, Precip, L, D, R, U, A, P, HP)
    return(SocHydResults)
}
