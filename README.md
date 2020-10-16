# Introduction

GOANIM (Global Organic Agriculutre NutrIent siMulator) is a spatially explicit, biophysical and linear optimisation model simulating cropland soil nitrogen (N)  cycling in organic farming systems and its feedback effects on food production at the global scale. GOANIM includes different compartments (organically managed croplands, livestock animals and permanent grasslands) and accounts the biomass and N flows among these compartments (as feedstuff, grazed biomass and animal manure) as well as the N flows between cropland soils and the environment. GOANIM simulates the supply side of the global food system -i.e. food production-, by maximizing production from both cropland and livestock food commodities. 



# Prerequisites

Due to the nature of the input files (raster maps) the model requires an important amount of remporary memory (RAM) in order to run. Therefore, the model cannot be executed on a personal PC or laptop, but it requires the access to a server. We use to run the model using the Mésocentre de Calcul Intensif Aquitain (MCIA) and the CURTA Unix Server (https://redmine.mcia.fr/projects/cluster-curta/wiki)

The model also requires the COIN-OR CLP solver to be installed in order to be able to solve the linear optimisation problems (https://projects.coin-or.org/Clp)

List of R packages needed to run the model: raster, clpAPI, ROI.plugin.clp, ROI, slam, doParallel, foreach, unixtools, pryr
