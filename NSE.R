NSE <- function(sim, obs)
{
  numer = 0
  denom = 0
  for (j in 1:length(sim))
  {numer = numer + (sim[j] - obs[j])^2
  denom = denom + (obs[j] - mean(obs))^2}
  NSE_Yearly = 1 - numer/denom
  return(NSE_Yearly)
}