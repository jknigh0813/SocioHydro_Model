RMSE <- function(sim, obs)
{
  
  N = length(sim)
  res2 = (sim - obs)^2
  RMSE_Val = sqrt(mean(res2))
  
  return(RMSE_Val)
}