# Introduction

GOANIM (Global Organic Agriculutre NutrIent siMulator) is a spatially explicit, biophysical and linear optimisation model simulating cropland soil nitrogen (N)  cycling in organic farming systems and its feedback effects on food production at the global scale. GOANIM includes different compartments (organically managed croplands, livestock animals and permanent grasslands) and accounts the biomass and N flows among these compartments (as feedstuff, grazed biomass and animal manure) as well as the N flows between cropland soils and the environment. GOANIM simulates the supply side of the global food system -i.e. food production-, by maximizing production from both cropland and livestock food commodities. 



# Prerequisites

Due to the nature of the input files (raster maps) the model requires an important amount of remporary memory (RAM) in order to run. Therefore, the model cannot be executed on a personal PC or laptop, but it requires the access to a server. We use to run the model using the Mésocentre de Calcul Intensif Aquitain (MCIA) and the CURTA Unix Server (https://redmine.mcia.fr/projects/cluster-curta/wiki)

The model also requires the COIN-OR CLP solver to be installed in order to be able to solve the linear optimisation problems (https://projects.coin-or.org/Clp)

List of R packages needed to run the model: raster, clpAPI, ROI.plugin.clp, ROI, slam, doParallel, foreach, unixtools, pryr

# Model Versions

GOANIM is provided in two main version: version #1 is based on the optimisation of N flows, crop yields and livestock populations; version #2 is based only on the optimisation of N flows and crop yields for cropland production. Livestock manure data as to be inputed to the model. Check out the model documentation for further details.

# Model Updates

GOANIM version #1 as been updated since its version used to generate the scenarios described in "Barbieri et al (2021). The global option space fpr organic agriculture is delimited by nitrogen availability. _Nature Food, 2, 363-372_"

These updates includes:

* Update of IPCC coefficients from IPCC 2016 to IPCC 2019, including a better refining of the N flows distribution amoung temporary and permanent pastures.
* Changes in the dietary requirements and productivity of animals in organic farming, as based on results from "Gaudare et al (2020). Comparing productivity and feed-use efficiency between organic and conventional livestock animals. _Environmental Research letters 16(2)_"
* Add limitation for the share of permanent pasture biomass that is grazable by livestock, following "Erb et al. (2016). Exploring the biophysical option space for feeding the world without deforestation. _Nature Communications, 7_"
* The simulation of livestock populations accounts now for youg animals (which were previosly not simulated)

This most recent version can be found in this repository under the name of GOANIM_organic_model_version2021.R
