# Socio-Hydrologic Model - FEMA NFIP / Census Data 
This repository contains the following: 
* SocHydModel.R: socio-hydrological model of Barendrecht et al. (2019)
* PreProcess_ClaimsPolicies.R: pre-processing functions to aggregate NFIP claims and policy records by US metropolitan area and year
* PreProcess_CensusPop.R: pre-processing function to aggregate census population totals by US metropolitan area and year 
* PreProcess_SocHyd_TS.R: merges separate policy, claims, and census data into an annual time series
* DDS_Wrapper.R: integrates Socio-hydrological model into the Dynamically Dimensioned Search (DDS) algorithm of Tolson & Shoemaker (2007). 
* NSE.R: computes Nash Sutcliffe Efficiency 
* RMSE.R: computes Root Mean Square Error

## References
Barendrecht, M. H., Viglione, A., Kreibich, H., Merz, B., Vorogushyn, S., & Blöschl, G. (2019). The value of empirical data for estimating the parameters of a sociohydrological flood risk model. Water resources research, 55(2), 1312-1336.

Tolson, B. A., & Shoemaker, C. A. (2007). Dynamically dimensioned search algorithm for computationally efficient watershed model calibration. Water Resources Research, 43(1).
