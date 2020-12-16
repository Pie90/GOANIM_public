#
#  Copyright 2018 Pietro Barbieri <pbarbieri@avakas-frontend2>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#  MA 02110-1301, USA.
#
#



library(raster)
library(clpAPI, lib.loc="/home/pbarbieri/R/x86_64-pc-linux-gnu-library/3.2")
#library(Rglpk)
library(ROI.plugin.clp, lib.loc="/home/pbarbieri/R/x86_64-pc-linux-gnu-library/3.2")
#library(ROI.plugin.lpsolve)
library(ROI)
library(slam)


## Parallel computing core packages
library(doParallel)
library(foreach)

# ----------- Set Working Directory and Temporary files directory in server scratch  --------------------

setwd("/home/pbarbieri/global/Version_I_first_inclusion_of_land_use_change_rules/")
getwd()
library(unixtools)
set.tempdir("/scratch/pbarbieri/R_temporary_files")
tempdir()


#.libPaths(c("/home/pbarbieri/R/x86_64-pc-linux-gnu-library/3.2"))

# -------------------------------------------------   ATT! CORE CONTROLS   ------------------------------------------
detectCores()
cluster<-makeCluster(23)
registerDoParallel(cluster) #start cluster
getDoParWorkers() #check number of cores used
#registerDoSEQ() # Stop cluster



# -------------------------------------------- 1 Binary coefficients to activate/disactivate variables and global scenario coefficients -------------------------------------

		global_production_share_coefficient_conv <- 0
		global_production_share_coefficient_org <- 1 - global_production_share_coefficient_conv


			K_conv_manure_surplus <- 0  # 0-1 Desactivate/Activate the use of conventional surplus manure. Manure as already been accounted for losses in the conventional-submodel
			K_waste_water <- 0  # 0-1 Desactivate/Activate the use of conventional surplus manure. Wastewater as already been accounted for losses in the conventional-submodel


# -------------------------------------------------  2 DATA LOADING   ------------------------------------------

	crop_info <- read.csv("crop_list_60.csv", header=T)


	##  creates emplty vector for data loading
		matrix_yield_max_vec <- array(dim=c(1,9331200,61))   # Organic potential yield
		matrix_area_vec <- array(dim=c(1,9331200,61))        # Organic areas hectares


#     *************************** ATT!! Correction of Organic Area according to global organic share **************************
	# Load organic potential area t/ha (Ponisio modified coefficients, see model documentation for details), Organic land use (areas ha)
	foreach (i = 1:61, .packages="raster") %do% {
        matrix_yield_max_vec[,,i] <- as.vector(raster(paste0("Organic yield files/Modified_Ponisio_organic_yield_gap/", crop_info$CropName[i], "_organic_yield_modified_Ponisio.tif"))*crop_info$dry_matter[i])                  # Load organic max yields with modified Ponision coefficents and tranform them in DM
        matrix_area_vec[,,i] <- as.vector(raster(paste0("/home/pbarbieri/global/Land Use Change/100% conversion/Area_hectares/", crop_info$CropName[i], "_total_area_hectares1.tif"))* global_production_share_coefficient_org)  # Load organic areas in ha
	}




## ________  CROP YIELDS and coefficients - Yield curves slopes, RPR coefficients, N-fixed for legumes and cyanobacterias _________
#Crops_yield_curve_slope <- 1/crop_info$N
	Crops_yield_curve_slope <- 1/crop_info$N
	Stovers_yield_curve_slope <- 1/crop_info$N_crop_residues
	Stovers_yield_curve_slope[Stovers_yield_curve_slope==(Inf)] <- 0
	#Stovers_RPR <- crop_info$calculated_RPR                                         # RPR coefficients to estimate crop residues from harvested biomass -- OLD COEFFICIENTS
	Stovers_RPR <- crop_info$RPR_corrected_optimisation_model                        # RPR coefficients to estimate crop residues from harvested biomass

    crop_res_avail_org <- array(dim=c(1, 9331200))
    crop_res_avail_org[,] <- as.vector((1-raster("Coefficients/map_coeff_residues_recycle_organic.tif")/1.45))    # Crop residues recycling in organic farming
                                                                                                # ******** ATTENTION the coefficient 1.45 set the same recycle ratios as in conventional!! ****


																								
																							
	Fixed_N <- crop_info$NiC * crop_info$X1.NHI * crop_info$Proot_NOT_TO_BE_USED * crop_info$Ndfa
		# **** ATTENTION DOUBLE!  It is true that roots stays in the soil,
		# i.e. they are not removed, so they do not represent a demand in terms of N, but the nitrogen that is fixed
		# by the roots (i.e. the share of the NBF that is fixed by roots) is coming from the Atmosphere and not from the soil,
		# and therefore it represents a N input to the system!!!


	cereals_tuber_oils <- 0.012 # /average yeald max         # in tons/ha, Liu et al. 2010
	roots <- which(crop_info$CropGroup=="roots")
	cereal <- which(crop_info$CropGroup=="cereal")
	sec.cereal <- which(crop_info$CropGroup=="sec.cereal")
	oilcrops <- which(crop_info$CropGroup=="oilcrops")
	N_cyanobacteria <- array(dim=c(1,61))
	N_cyanobacteria[roots] <- N_cyanobacteria[cereal] <- N_cyanobacteria[sec.cereal] <- N_cyanobacteria[oilcrops]  <- cereals_tuber_oils
	N_cyanobacteria[50] <-  0.1               #  sugarcane in tons/ha, Liu et al. 2010
	N_cyanobacteria[43] <-  0.02              #  rice in tons/ha, Liu et al. 2010
	N_cyanobacteria <- ifelse(!is.na(N_cyanobacteria), N_cyanobacteria, 0)


## ____________  ORGANIC MANURE INPUTS - % of current (i.e. conventional) standing livestock numbers  _____________


	total_N_manure <- raster("Script_generated_files/total_N_manure_correct_list_countries.tif")
			rclmat <- matrix(c(3000, Inf, 3000), ncol=3, byrow=T) # correction of errors in Manure maps --> whaterver more than 5000 t gridcell give 5000
	total_N_manure <- reclassify(total_N_manure, rclmat)  # correct for values above 3000 --> if x>3000, give 3000
	total_N_manure <- extend(total_N_manure, extent(-180,180,-90,90))

	total_N_manure_corrected_95crops <- total_N_manure * 0.96 * (1 -  (0.0166 + 0.2))
	# ********* above coefficients description: 1) correction to account for the 5% of crops (in terms of cropland share) that we did not include in the 61 crops
	#                                           2) correction to estimate 1% losses due to direct N2O emissions and N2 (0.66%)
	#											3) correction to estimate the 20% losses due to NH3 and NO volatilisation --> range coefficient: 0.05-0.5
	leaching_areas_mask <- raster("/home/pbarbieri/global/Version_I_first_inclusion_of_land_use_change_rules/Climatic and irrigation data/mask_area_leaching_final_irrigation_above_005.tif") # Load leaching areas. The areas where leaching occurs were estimated according to the IPCC procedure. See "Estimation_areas_of_N_leaching.R" in /Pietro/R Analysis/ Global convetion.../ Estimation areas of.../R_code
	mask_correction_leaching <- leaching_areas_mask * 0.7                                                           # 0.3: correction to estimate the 30% losses due to leaching
	mask_correction_leaching <- reclassify(mask_correction_leaching, cbind(0,1))
	mask_correction_leaching_vec <- array(dim=c(1, 9331200))

	total_N_manure_corrected_95crops <- total_N_manure_corrected_95crops - (total_N_manure * 0.96 * (1 - mask_correction_leaching))


	N_organic_manure <- array(dim=c(1,9331200))                           # N from conventional manure surplus computed in the conventional submodel - non residistributed
	N_organic_manure[,] <- as.vector(total_N_manure_corrected_95crops * global_production_share_coefficient_org)


## ____________  CONVENTIONAL FARMING INPUTS - from conventional submodel results _____________

	N_conventional_manure <- array(dim=c(1,9331200))                           # N from conventional manure surplus computed in the conventional submodel - non residistributed
	N_conventional_manure[,] <- as.vector(raster("Script_generated_files/manure_N_surplus_total_100_manure_cropland_correct_list_countries.tif"))

	# N_conventional_manure[,] <- as.vector(raster("Script_generated_files/N_redistributed_manure_surplus_100_harvested_area.tif"))



	N_conventional_waste_water <- array(dim=c(1,9331200))                      # N from conventional waste water surplus computed in the conventional submodel residistributed; current population and diets
		waste_water <- extend(raster("Script_generated_files/final_wastewater_corrected_losses.tif"), extent(-180,180,-90,90))
	N_conventional_waste_water[,] <- as.vector(waste_water)


   ## ____________  LEACHING MAPS for correction of manure and N from stovers recycled and N natmospheric depositions _____________

mask_correction_leaching_vec[,] <- as.vector(mask_correction_leaching)              # leaching coefficients


        N_deposition <- array(dim=c(1,9331200,1))                                   # N atmospheric depostions 1993 per hectare and cut per cropland area corrected by leaching --> to be multiplied by crop area to get amoun in tons, dentner et al.
        N_deposition[,,1] <- as.vector(raster("N atmospheric deposition/N_deposition_per_hectare_of_cropland2000.tif") * mask_correction_leaching)


Energy_crops <- crop_info$metabolisable_Energy_MJ.kgDM*1000    # transformed into MJ/tons


 ## Generate arrays for storing the optimisation results  --------------------------

    matrix_final_N_assigned_to_crop <- matrix_final_yield <- array(dim=c(1,9331200,61))                            # Final organic yiels, N assigned to crops
    counter <- array(dim=c(1,9331200))                                                                             # Counter_for_model_failures_in_finding_a_solution  [0-1]
    BNF <- array(dim=c(1,9331200,length(which(Fixed_N!=0))))                                                       # Crop fixed BNF


# -------------------------------------------------  3 OPTIMISATION PROCEDURE   ------------------------------------------

    
i <- 1

## sensitivity initalisation, script initialisation.
sensitivity_no <- 51
script_no <- 1


for (j in 1:1060363) {
  

if (is.na(matrix_area_vec[i,j,1])==FALSE) {   # Run optimisation only if there is some  land in the gridcell (excuding oceans)




# 1st part of the matrix giving the 61 yield curve constraint for the HARVESTED BIOMASS
sparse_matrix_1 <- simple_triplet_diag_matrix(rep(1,61), nrow=61)                                                      #  Matrix square 1
sparse_matrix_2 <- simple_triplet_diag_matrix(-Crops_yield_curve_slope[], nrow=61)                                     #  Matrix square 2
sparse_matrix_3 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 3
sparse_matrix_4 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 4
sparse_matrix_5 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 5
sparse_matrix_6 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 6


# 2nd part of the matrix giving the 61 yield curve PLATEAU constraint for the HARVESTED BIOMASS
sparse_matrix_7 <- simple_triplet_diag_matrix(rep(1, 61), nrow=61)                                                      #  Matrix square 9
sparse_matrix_8 <- simple_triplet_zero_matrix(61)                                                                       #  Matrix square 10
sparse_matrix_9 <- simple_triplet_zero_matrix(61)                                                                       #  Matrix square 11
sparse_matrix_10 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 12
sparse_matrix_11 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 13
sparse_matrix_12 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 14



# 3rd part of the matrix giving the 61 N required by EXPORTED STOVERS BIOMASS + LOSSES constraint (losses=leaching and volatilisation stovers returned to soils)
sparse_matrix_13 <- simple_triplet_diag_matrix(Stovers_RPR[], nrow=61)                                                  #  Matrix square 17
sparse_matrix_14 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 18
sparse_matrix_15  <- simple_triplet_diag_matrix((ifelse(-((Stovers_yield_curve_slope*1)/(crop_res_avail_org[i,j]+(2-(0.99+mask_correction_leaching_vec[i,j]))*(1-crop_res_avail_org[i,j])))==-Inf, 0,
-((Stovers_yield_curve_slope*1)/(crop_res_avail_org[i,j]+(2-(0.99+mask_correction_leaching_vec[i,j]))*(1-crop_res_avail_org[i,j]))))),nrow=61)
                                                                                                                       #  Matrix square 19   # According to IPCC crop residues losses are only direct N2O emissions and leaching but NOT indirect N2O emissions
sparse_matrix_16 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 20
sparse_matrix_17 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 21
sparse_matrix_18 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 22


# 4th part of the matrix giving the 61 constraint for fixed nitrogen by pulses crops and by roots/cereals/oilcrops/sugarcane
sparse_matrix_19 <- simple_triplet_diag_matrix(Fixed_N[], nrow=61)                                                     #  Matrix square 25
sparse_matrix_20 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 26
sparse_matrix_21 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 27
sparse_matrix_22 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 28
sparse_matrix_23 <- simple_triplet_diag_matrix(rep(-1,61), nrow=61)                                                    #  Matrix square 29
sparse_matrix_24 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 30


# 5th part of the matrix giving the 61 constraint for nitrogen balancing for each crop including Ngrain, Nres, Npool, Nfixed, and Nfertilisers
sparse_matrix_25 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 33
sparse_matrix_26 <- simple_triplet_diag_matrix(matrix_area_vec[i,j,], nrow=61)                                         #  Matrix square 34    N_grain
sparse_matrix_27 <- simple_triplet_diag_matrix(matrix_area_vec[i,j,], nrow=61)                                         #  Matrix square 35    N_residues
sparse_matrix_28 <- simple_triplet_diag_matrix(matrix_area_vec[i,j,], nrow=61)                                         #  Matrix square 36    N_pool
sparse_matrix_29 <- simple_triplet_diag_matrix(-matrix_area_vec[i,j,], nrow=61)                                        #  Matrix square 37    N_fixed
sparse_matrix_30 <- simple_triplet_diag_matrix(-matrix_area_vec[i,j,], nrow=61)                                        #  Matrix square 38    N_fertilisers


# 6th part of the matrix giving the 1 constraint for nitrogen availability for N from fertilisers
sparse_matrix_31 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 41
sparse_matrix_32 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 42
sparse_matrix_33 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 43
sparse_matrix_34 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))           #c(-matrix_area_vec[i,j,]))      #  Matrix square 44
sparse_matrix_35 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 45
sparse_matrix_36 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(matrix_area_vec[i,j,]))                             #  Matrix square 46



## Built up full matrix binding all submatrix parts created until now
crop_yield_curve_sparse_matrix <- cbind(sparse_matrix_1, sparse_matrix_2, sparse_matrix_3, sparse_matrix_4, sparse_matrix_5, sparse_matrix_6)
crop_yield_plateau_sparse_matrix <- cbind(sparse_matrix_7,sparse_matrix_8, sparse_matrix_9, sparse_matrix_10, sparse_matrix_11, sparse_matrix_12)
crop_stovers_yield_curve_sparse_matrix <- cbind(sparse_matrix_13,sparse_matrix_14, sparse_matrix_15, sparse_matrix_16, sparse_matrix_17, sparse_matrix_18)
crop_nitrogen_fixation_sparse_matrix <- cbind(sparse_matrix_19, sparse_matrix_20, sparse_matrix_21, sparse_matrix_22, sparse_matrix_23, sparse_matrix_24)
crop_nitrogen_budgets_sparse_matrix <- cbind(sparse_matrix_25, sparse_matrix_26, sparse_matrix_27, sparse_matrix_28, sparse_matrix_29, sparse_matrix_30)
N_total_available_for_crops_sparse_matrix <- cbind(sparse_matrix_31, sparse_matrix_32, sparse_matrix_33, sparse_matrix_34, sparse_matrix_35, sparse_matrix_36)


final_sparse_matrix <- rbind(crop_yield_curve_sparse_matrix, crop_yield_plateau_sparse_matrix, crop_stovers_yield_curve_sparse_matrix, crop_nitrogen_fixation_sparse_matrix, crop_nitrogen_budgets_sparse_matrix, N_total_available_for_crops_sparse_matrix)




## Set directions of the contrains inequalities
dir <- c(rep("==", 61),                                         # yield curve
c("==", rep("<=", 10),"==", rep("<=", 10), "==", rep("<=", 6), "==", rep("<=", 27), "==", rep("<=", 3)),   # yield plateau  --> Leguminous crops are forced to yield max (since they fix all their requirement of N by fixation). I did this to avoid the model assigning for any reason a lower yield I guess, but I dont remember exactlry wgy this was happening.
rep("==", 183), c("<=")) #  budgeting, total available fertiliser



## Set right hand side values of the inequalities
rhs <- c(rep(0,61),matrix_yield_max_vec[i,j,], rep(0,122),                                              # yield curve, plateau, N stovers and N fix
(N_deposition[i,j,1]*matrix_area_vec[i,j,]+N_cyanobacteria*matrix_area_vec[i,j,]),                      # N budget
(+(N_conventional_manure[i,j]*K_conv_manure_surplus*global_production_share_coefficient_conv)+(N_conventional_waste_water[i,j]*K_waste_water)+(N_organic_manure[i,j])))  # N available for fertilisers


## Build ROI package contraint
contst1 <- L_constraint(final_sparse_matrix, dir, rhs)


## set ROI objective function
obj <- (matrix_area_vec[i,j,]*Energy_crops)
obj[c(1, 12, 18, 21, 23, 27, 30, 33, 58, 44, 53, 54, 16)] <- 0.0000001                                           # set fodder crops to zero AND cotton

obj <- c(obj, rep(0,305))


obj <- L_objective(obj, names = c(paste0("Y_", crop_info$CropName[1:61]),
                                  paste0("harv_N_", crop_info$CropName[1:61]),
                                  paste0("stov_N_", crop_info$CropName[1:61]),
                                  paste0("pool_N_", crop_info$CropName[1:61]),
                                  paste0("fix_N_", crop_info$CropName[1:61]),
                                  paste0("fert_N", crop_info$CropName[1:61])))

## Set continuos variable type (clp solver only accepts continuos variables)
types <- rep("C", 366)

## Set  variables upper and lower bounds
bounds <- V_bound(li=seq(1:366), lb=c(rep(0, 366)), ui=seq(1:366), ub=c(rep(10e13, 366)))

## Print vector position (optimised gridcell vector number)
print(j)

## Built and solve optimisation problem using ROI package
prob <- OP(obj, contst1, types = types, maximum= TRUE, bounds=bounds)
solution <- ROI_solve(prob, solver = "clp", control=list(amount=1, iterations=5000, seconds=10))





counter[i,j] <- solution$status$code
matrix_final_yield[i,j,] <- solution$solution[1:61]
matrix_final_N_assigned_to_crop[i,j,] <- solution$solution[306:366]
BNF[i,j,] <- solution$solution[(244+which(Fixed_N!=0))]
}

else {			matrix_final_yield[i,j,] <- rep(NA, 61)
				matrix_final_N_assigned_to_crop[i,j,] <- rep(NA, 61)
                counter[i,j] <- NA
                BNF[i,j,] <- rep(NA,length(which(Fixed_N!=0)))
   }
}


# -------------------------------------------------  FREE UP MEMORY DELETING VARIABLES   ------------------------------------------
library(pryr)
print(mem_used())
print(mem_change(rm(matrix_area_vec, matrix_yield_max_vec)))
gc()

# -------------------------------------------------  4 DATA SAVING   ------------------------------------------

crs <- c("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# Total crops yield
for (i in 1:61) {
    raster_1 <- raster(matrix(matrix_final_yield[,,i], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
	writeRaster(raster_1, filename = paste0("Organic_optimisation_sub_model/results/script_", script_no, "/scenario_S_", sensitivity_no , "/script_", script_no, "_yield_1_", crop_info$CropName[i], ".tif"), format="GTiff", overwrite=T)
}

# Error counter
raster_2 <- raster(matrix(counter[,], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_2, filename = paste0("Organic_optimisation_sub_model/results/script_", script_no,  "/scenario_S_", sensitivity_no , "/script_", script_no, "_optimisation_counter"), format="GTiff", overwrite=T)



# Total N from organic manure allocate to each crop
for (i in 1:61) {
    raster_3 <- raster(matrix(matrix_final_N_assigned_to_crop[,,i], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
    writeRaster(raster_3, filename = paste0("Organic_optimisation_sub_model/results/script_", script_no, "/scenario_S_", sensitivity_no,  "/script_", script_no, "_N_MANURE_assigned_to_crop_1_", crop_info$CropName[i], ".tif"), format="GTiff", overwrite=T)
}

rm(raster_3, matrix_final_N_assigned_to_crop)
gc()


# BNF
for (i in 1:length(which(Fixed_N!=0))) {
    raster_4 <- raster(matrix(BNF[,,i], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
    writeRaster(raster_4, filename = paste0("Organic_optimisation_sub_model/results/script_", script_no,  "/scenario_S_", sensitivity_no , "/script_", script_no, "_BNF_", crop_info$CropName[which(crop_info$CropClass=="N-fixing")[i]], ".tif"), format="GTiff", overwrite=T)
}

rm(raster_4, BNF)
gc()


