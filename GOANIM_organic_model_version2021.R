#
#  Copyright 2018 Pietro Barbieri <ugaudare@avakas-frontend2>
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
#



library(raster)
library(clpAPI, lib.loc="/gpfs/home/ugaudare/R/x86_64-pc-linux-gnu-library/3.5")
#library(Rglpk)
library(ROI.plugin.clp, lib.loc="/gpfs/home/ugaudare/R/x86_64-pc-linux-gnu-library/3.5")
#library(ROI.plugin.lpsolve)
library(ROI)
library(slam)


	## Parallel computing core packages
	library(doParallel)
	library(foreach)

# ----------- Set Working Directory and Temporary files directory in server scratch  --------------------

setwd("/gpfs/home/ugaudare/GOANIM6/")
getwd()
library(unixtools)
set.tempdir("/scratch/ugaudare/R_temporary_files")
tempdir()

#.libPaths(c("/gpfs/home/ugaudare/R/x86_64-pc-linux-gnu-library/3.2"))

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

		crop_info <- read.csv("Input_data/Organic/Crop/crop_list_60.csv", header=T,sep=";")
		#Livestock_info <- read.csv

 ##  creates emplty vector for data loading
    matrix_yield_max_vec <- array(dim=c(1,9331200,61))   # Organic potential yield
    matrix_area_vec <- array(dim=c(1,9331200,61))        # Organic areas hectares
    #matrix_N_yield_max_vec <- array(dim=c(1,9331200,61)) # Required N at poteential yield
    EF_org<- array(dim=c(1,9331200,61))        # EF for manure

#     *************************** ATT!! Correction of Organic Area according to global organic share **************************
#     *************************** ATT!! Correction of Organic Area according to global organic share **************************
# Load organic potential area t/ha (Ponisio modified coefficients, see model documentation for details), Organic land use (areas ha)
        foreach (i = 1:61, .packages="raster") %do% {
      matrix_yield_max_vec[,,i] <- as.vector(raster(paste0("Input_data/Organic/Crop/Organic_yield/", crop_info$CropName[i], "_organic_yield_modified_Ponisio.tif"))*crop_info$dry_matter[i]*crop_info$RPR[i])                  # Load organic max yields (corrected for fodder crops by 0.8) with modified Ponision coefficents and tranform them in DM
      matrix_area_vec[,,i] <- as.vector(raster(paste0("Input_data/Organic/Crop/Organic_land_use/", crop_info$CropName[i], "_total_area_hectares1.tif"))* global_production_share_coefficient_org)  # Load organic areas in ha
      #matrix_N_yield_max_vec[,,i] <- as.vector((raster(paste0("Script_generated_files/rasters_organic_yield_DM/", crop_info$CropName[i], ".tif")) * crop_info$N[i]))                                                          # Load organic max yields in DM and multiply by N content
      EF_org[,,i] <- as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/EF_Organic.tif"))  # Load EF N manure in direct N2O esmissions
	}
      EF_org[,,43] <-  as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/EF_N_rice.tif"))    # Load EF N manure in direct N2O esmissions for rice


## Load dietary requirements maps ---   ATT PROTEIN TRANSFORMED IN TONS !!!! --------------------------
  E_grain_dietary_req <- array(dim=c(1,9331200,9))
				E_grain_dietary_req[,,1] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/dairy_grain_req_energy.tif"))
				E_grain_dietary_req[,,2] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/beef_grain_req_energy.tif"))
				E_grain_dietary_req[,,3] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/goats_milk_grain_req_energy.tif"))
				E_grain_dietary_req[,,4] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/goats_meat_grain_req_energy.tif"))
				E_grain_dietary_req[,,5] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/sheep_milk_grain_req_energy.tif"))
				E_grain_dietary_req[,,6] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/sheep_meat_grain_req_energy.tif"))
				E_grain_dietary_req[,,7] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/pigs_grain_req_energy.tif"))
   				E_grain_dietary_req[,,8] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/broilers_grain_req_energy.tif"))
				E_grain_dietary_req[,,9] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/layers_grain_req_energy.tif"))
   P_grain_dietary_req <- array(dim=c(1,9331200,9))
				P_grain_dietary_req[,,1] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/dairy_grain_req_protein.tif")/1000)
				P_grain_dietary_req[,,2] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/beef_grain_req_protein.tif")/1000)
				P_grain_dietary_req[,,3] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/goats_milk_grain_req_protein.tif")/1000)
				P_grain_dietary_req[,,4] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/goats_meat_grain_req_protein.tif")/1000)
				P_grain_dietary_req[,,5] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/sheep_milk_grain_req_protein.tif")/1000)
				P_grain_dietary_req[,,6] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/sheep_meat_grain_req_protein.tif")/1000)
				P_grain_dietary_req[,,7] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/pigs_grain_req_protein.tif")/1000)
  				P_grain_dietary_req[,,8] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/broilers_grain_req_protein.tif")/1000)
				P_grain_dietary_req[,,9] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Grain_requirements/layers_grain_req_protein.tif")/1000)
  E_grass_dietary_req <- array(dim=c(1,9331200,9))
				E_grass_dietary_req[,,1] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/dairy_grass_req_energy.tif"))
				E_grass_dietary_req[,,2] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/beef_grass_req_energy.tif"))
				E_grass_dietary_req[,,3] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/goats_milk_grass_req_energy.tif"))
				E_grass_dietary_req[,,4] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/goats_meat_grass_req_energy.tif"))
				E_grass_dietary_req[,,5] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/sheep_milk_grass_req_energy.tif"))
				E_grass_dietary_req[,,6] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/sheep_meat_grass_req_energy.tif"))
				E_grass_dietary_req[,,7] <- 0
  				E_grass_dietary_req[,,8] <- 0
				E_grass_dietary_req[,,9] <- 0
  P_grass_dietary_req <- array(dim=c(1,9331200,9))
				P_grass_dietary_req[,,1] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/dairy_grass_req_protein.tif")/1000)
				P_grass_dietary_req[,,2] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/beef_grass_req_protein.tif")/1000)
				P_grass_dietary_req[,,3] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/goats_milk_grass_req_protein.tif")/1000)
				P_grass_dietary_req[,,4] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/goats_meat_grass_req_protein.tif")/1000)
				P_grass_dietary_req[,,5] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/sheep_milk_grass_req_protein.tif")/1000)
				P_grass_dietary_req[,,6] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Fodders_requirements/sheep_meat_grass_req_protein.tif")/1000)
				P_grass_dietary_req[,,7] <- 0
				P_grass_dietary_req[,,8] <- 0
				P_grass_dietary_req[,,9] <- 0
  E_stovers_dietary_req <- array(dim=c(1,9331200,9))
				E_stovers_dietary_req[,,1] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/dairy_stovers_req_energy.tif"))
				E_stovers_dietary_req[,,2] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/beef_stovers_req_energy.tif"))
				E_stovers_dietary_req[,,3] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/goats_milk_stovers_req_energy.tif"))
				E_stovers_dietary_req[,,4] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/goats_meat_stovers_req_energy.tif"))
				E_stovers_dietary_req[,,5] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/sheep_milk_stovers_req_energy.tif"))
				E_stovers_dietary_req[,,6] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/sheep_meat_stovers_req_energy.tif"))
				E_stovers_dietary_req[,,7] <- 0
  				E_stovers_dietary_req[,,8] <- 0
				E_stovers_dietary_req[,,9] <- 0
  P_stovers_dietary_req <- array(dim=c(1,9331200,9))
				P_stovers_dietary_req[,,1] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/dairy_stovers_req_protein.tif")/1000)
				P_stovers_dietary_req[,,2] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/beef_stovers_req_protein.tif")/1000)
				P_stovers_dietary_req[,,3] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/goats_milk_stovers_req_protein.tif")/1000)
				P_stovers_dietary_req[,,4] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/goats_meat_stovers_req_protein.tif")/1000)
				P_stovers_dietary_req[,,5] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/sheep_milk_stovers_req_protein.tif")/1000)
				P_stovers_dietary_req[,,6] <- as.vector(raster("Input_data/Organic/Livestock/Dietary_requirements/Stovers_requirements/sheep_meat_stovers_req_protein.tif")/1000)
				P_stovers_dietary_req[,,7] <- 0
  				P_stovers_dietary_req[,,8] <- 0
				P_stovers_dietary_req[,,9] <- 0

## Load N excreta available for cropland coefficients maps -------------------------- FOR RUMINANTS, PASTURE LOSSES ARE SEPARATED FOR CORRECTION!!! (see next step)
		# Losses due to storage condition, other uses of manure, etc, are here already included in the available manure. --> I.E available manure for application to soil. Losses after manure application to soils have to be incorporated. These losses are accounted in the optimisation matrix code.
  Nyear_livestock_production_available <- array(dim=c(1,9331200,9))
#				Nyear_livestock_production_available[,,1] <- as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/cow_Nitrogen_excreta_IPCC_medium.tif"))
#				Nyear_livestock_production_available[,,2] <- as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/cow_Nitrogen_excreta_IPCC_medium.tif"))
#				Nyear_livestock_production_available[,,3] <- as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/goats_Nitrogen_excreta_IPCC_medium_coefficient_map.tif"))
#				Nyear_livestock_production_available[,,4] <- as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/goats_Nitrogen_excreta_IPCC_medium_coefficient_map.tif"))
#				Nyear_livestock_production_available[,,5] <- as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/sheep_Nitrogen_excreta_IPCC_medium_coefficient_map.tif"))
#				Nyear_livestock_production_available[,,6] <- as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/sheep_Nitrogen_excreta_IPCC_medium_coefficient_map.tif"))
#				Nyear_livestock_production_available[,,7] <- as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/pigs_Nitrogen_excreta_IPCC_medium_coefficient_map.tif"))
#				Nyear_livestock_production_available[,,8] <- as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/poultry_Nitrogen_excreta_IPCC_medium_coefficient_map.tif"))
#				Nyear_livestock_production_available[,,9] <- as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/poultry_Nitrogen_excreta_IPCC_medium_coefficient_map.tif"))
                Nyear_livestock_production_available[,,1] <- as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/cow_MILK_Nitrogen_excreta_IPCC_medium_WITHOUT_PASTURE_LOSSES_coefficient_map_CORRECTED_N_IN_OUT_V1.tif"))   # for all ruminants: The excreta coefficient here are the same as for conventional but they exclude the fraction of manure 'lost' on pasture for IPCC. This fraction is accounted in the next calculation steps, and takes into consideration the increase in organic temporary fodder over the current -i.e. conventional- baseline
				Nyear_livestock_production_available[,,2] <- as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/cow_BEEF_Nitrogen_excreta_IPCC_medium_WITHOUT_PASTURE_LOSSES_coefficient_map_CORRECTED_N_IN_OUT_V1.tif"))
				Nyear_livestock_production_available[,,3] <- as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/goats_MILK_Nitrogen_excreta_IPCC_medium_WITHOUT_PASTURE_LOSSES_coefficient_map_CORRECTED_N_IN_OUT_V1.tif"))
				Nyear_livestock_production_available[,,4] <- as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/goats_MEAT_Nitrogen_excreta_IPCC_medium_WITHOUT_PASTURE_LOSSES_coefficient_map_CORRECTED_N_IN_OUT_V1.tif"))
				Nyear_livestock_production_available[,,5] <- as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/sheep_MILK_Nitrogen_excreta_IPCC_medium_WITHOUT_PASTURE_LOSSES_coefficient_map_CORRECTED_N_IN_OUT_V1.tif"))
				Nyear_livestock_production_available[,,6] <- as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/sheep_MEAT_Nitrogen_excreta_IPCC_medium_WITHOUT_PASTURE_LOSSES_coefficient_map_CORRECTED_N_IN_OUT_V1.tif"))
				Nyear_livestock_production_available[,,7] <- as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/pig_MEAT_Nitrogen_excreta_IPCC_medium_WITHOUT_PASTURE_LOSSES_coefficient_map_CORRECTED_N_IN_OUT_V1.tif"))       # for all monogastric: The exreta coefficient here are the same as for conventional.
				Nyear_livestock_production_available[,,8] <- as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/broilers_MEAT_Nitrogen_excreta_IPCC_medium_WITHOUT_PASTURE_LOSSES_coefficient_map_CORRECTED_N_IN_OUT_V1.tif"))
				Nyear_livestock_production_available[,,9] <- as.vector(raster("Input_data/Organic/Livestock/Livestock_excreta/hens_EGG_Nitrogen_excreta_IPCC_medium_WITHOUT_PASTURE_LOSSES_coefficient_map_CORRECTED_N_IN_OUT_V1.tif"))
  #EF_org <- array(dim=c(1,9331200,9))				
				#EF_org[,,1]<-as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/EF_Organic.tif"))
				#EF_org[,,2]<-as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/EF_Organic.tif"))
				#EF_org[,,3]<-as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/EF_Organic.tif"))
				#EF_org[,,4]<-as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/EF_Organic.tif"))
				#EF_org[,,5]<-as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/EF_Organic.tif"))
				#EF_org[,,6]<-as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/EF_Organic.tif"))
				#EF_org[,,7]<-as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/EF_Organic.tif"))
				#EF_org[,,8]<-as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/EF_Organic.tif"))
				#EF_org[,,9]<-as.vector(raster("organic_optimisation_sub_model/spatial_livestock_excreta/EF_Organic.tif"))				
###  FOR RUMINANTS, Preparation files for correction of manure "losses" on pastures due to grazing / Put aside by UG to avoid counting twice %P
                #Nyear_livestock_PASTURE_LOSSES_1 <- raster("Input_data/Organic/Livestock/Livestock_excreta/cow_MILK_Nitrogen_IPCC_medium_PASTURE_LOSSES_coefficient_map.tif")
                #Nyear_livestock_PASTURE_LOSSES_2 <- raster("Input_data/Organic/Livestock/Livestock_excreta/cow_BEEF_Nitrogen_IPCC_medium_PASTURE_LOSSES_coefficient_map.tif")      # Load IPCC coefficients accounting for the fraction of manure 'lost' on permanent pastures
				#Nyear_livestock_PASTURE_LOSSES_3_4 <- raster("Input_data/Organic/Livestock/Livestock_excreta/goats_Nitrogen_IPCC_medium_PASTURE_LOSSES_coefficient_map.tif")
				#Nyear_livestock_PASTURE_LOSSES_5_6 <- raster("Input_data/Organic/Livestock/Livestock_excreta/sheep_Nitrogen_IPCC_medium_PASTURE_LOSSES_coefficient_map.tif")
				#stack <- stack(Nyear_livestock_PASTURE_LOSSES_1,Nyear_livestock_PASTURE_LOSSES_2, Nyear_livestock_PASTURE_LOSSES_3_4, Nyear_livestock_PASTURE_LOSSES_5_6)

  		#continents_shape <- raster("Input_data/Organic/Crop/map_coeff_residues_recycle_organic.tif")>=0                                                                                              # compute shape of continents (adds cells in non agriculuture/pasture areas)
		#continents_shape <- reclassify(continents_shape, cbind(1,0))
		#Coefficient_decreased_share_manure_losses_pasture <- raster("Input_data/Organic/Pasture/correction_coefficient_for_IPCC_manure_loss_on_pastures.tif")                         # This coefficients account for the % increase of temporary fodder in the organic scenario compared to the baseline. It was calculated as the % increase in temporary fodder after the land use change estimations in organic farming compared to the conventional baseline
																																							                              # We assume that such increase is directly proportional to the amount of manure which is not anymore lost on pastures.
		#Coefficient_decreased_share_manure_losses_pasture <- merge(Coefficient_decreased_share_manure_losses_pasture, continents_shape)

				#stack_corrected <- (1-stack)       # (1-manure_lost_on pasture) = manure_available_after_pasture_losses * Increase_in_availablility
					#rclmat <- matrix(c(1, Inf, 1), ncol=3, byrow=T)                                      # double bind corrections : values above 1 are set to 1.
				#stack_corrected <- reclassify(stack_corrected, rclmat)

  #Nyear_livestock_PASTURE_LOSSES <- array(dim=c(1,9331200,9))                                            # This is the IPCC coefficient giving the share of manure available on Pasture. To be directly used in the model optimisation code to estimate the final fractio of manure available for cropland.
				#Nyear_livestock_PASTURE_LOSSES[,,1] <- as.vector(stack_corrected[[1]])
				#Nyear_livestock_PASTURE_LOSSES[,,2] <- as.vector(stack_corrected[[2]])
				#Nyear_livestock_PASTURE_LOSSES[,,3] <- as.vector(stack_corrected[[3]])
				#Nyear_livestock_PASTURE_LOSSES[,,4] <- as.vector(stack_corrected[[3]])
				#Nyear_livestock_PASTURE_LOSSES[,,5] <- as.vector(stack_corrected[[4]])
				#Nyear_livestock_PASTURE_LOSSES[,,6] <- as.vector(stack_corrected[[4]])
				#Nyear_livestock_PASTURE_LOSSES[,,7] <- 1
				#Nyear_livestock_PASTURE_LOSSES[,,8] <- 1
				#Nyear_livestock_PASTURE_LOSSES[,,9] <- 1

## Load calculation Coefficients / maps      -------------------

  ## ________  CROP YIELDS and coefficients - Yield curves slopes, RPR coefficients, N-fixed for legumes and cyanobacterias _________

  Crops_yield_curve_slope <- 1/crop_info$N
  Stovers_yield_curve_slope <- 1/crop_info$N_crop_residues
			Stovers_yield_curve_slope[Stovers_yield_curve_slope==(Inf)] <- 0
    #Stovers_RPR <- crop_info$calculated_RPR                                         # RPR coefficients to estimate crop residues from harvested biomass -- OLD COEFFICIENTS
  Stovers_RPR <- crop_info$RPR_corrected_optimisation_model                        # RPR coefficients to estimate crop residues from harvested biomass

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


  Energy_crops_obj_fod <- Energy_crops_obj <- Energy_crops <- crop_info$metabolisable_Energy_MJ.kgDM*1000    # transformed into MJ/tons
  Energy_stovers <- crop_info$metabolisable_Energy_MJ.kgDM*1000  # transformed into MJ/tons
  Protein_crops <- crop_info$protein_content                     # As % of total DM
  Protein_stovers <- crop_info$N_crop_residues * 6.25

  crop_res_avail_org <- array(dim=c(1, 9331200))
  crop_res_avail_org[,] <- as.vector((1-raster("Input_data/Organic/Crop/map_coeff_residues_recycle_organic.tif")/1.45))    # Crop residues recycling in organic farming
																							# ******** ATTENTION the coefficient 1.45 set the same recycle ratios as in conventional!! ****

 share_available_feed <- crop_info$Fraction_used_for_feed
 #share_available_feed <- ifelse(share_available_feed==1, 1, share_available_feed*1.2)

   ## ________  PASTURES coefficients - available energy and protein from Permanent pastures _________

  permanent_pastures_available_biomass_t_DM <- array(dim=c(1, 9331200))
  permanent_pastures <- raster("Input_data/Organic/Pasture/Average_year_2015_tonsDMPERMANENTPASTURES_year_gridcell.tif")
    origin(permanent_pastures) <- 0
  		rclmat <- matrix(c(-Inf, 0, 0), ncol=3, byrow=T)    # correction of errors in Pasture maps delete areas where pasture is negative (negative Primary Productivity maps)
  permanent_pastures <- reclassify(permanent_pastures, rclmat)
  		continents_shape <- raster("Input_data/Organic/Crop/map_coeff_residues_recycle_organic.tif")>=0
		continents_shape <- reclassify(continents_shape, cbind(1,0))
		permanent_pastures <- merge(permanent_pastures, continents_shape)
  Pasture_limit<-raster("Input_data/Organic/Pasture/Max_grazing_Erb.tif")
  permanent_pastures_available_biomass_t_DM[,] <- as.vector(permanent_pastures*Pasture_limit)

    permanent_pastures_energy_protein_densities <- array(dim=c(1, 9331200, 2))
    permanent_pastures_energy_protein_densities[,,1] <- as.vector(raster("Input_data/Organic/Pasture/global_energy_pasture.tif")*1000) # in MJ/tons
    permanent_pastures_energy_protein_densities[,,2] <- as.vector(raster("Input_data/Organic/Pasture/global_protein_pasture.tif")/100)     # in % DM

  #Energy_pastures <- 9.93 * 1000   # in MJ/tons - OLD COEFFICIENT
  #Protein_pastures <- 0.18         # in % DM - OLD COEFFICIENT

   ## ________  LIVESTOCK coefficients to calculate Energy into livestock products (meat, milk, eggs) _________

  livestock_products_coefficients <- read.csv("Input_data/Organic/Livestock/Animal_products_energy_and_proteins_coefficients.csv")
  E_livestock_products <- livestock_products_coefficients$Energy.MJ...KG   #1: cow milk, 2: cow meat, 3: sheep milk, 4: sheep meat, 5: goat milk, 6: goat meat, 7: pigs, 8: broilers, 9: eggs


    matrix_livestock_production <- array(dim=c(1,9331200,8))              # 1: cow milk, 2: cow meat, 3: goats milk, 4: goats meat, 5: sheep milk, 6: sheep meat, 7: pig meat
			matrix_livestock_production[,,1] <- as.vector(raster("Input_data/Organic/Livestock/Production/cows_milk_liters_day_weigth.tif")*365)
			matrix_livestock_production[,,2] <- as.vector(raster("Input_data/Organic/Livestock/Production/cows_carcass_weigth.tif"))
			matrix_livestock_production[,,3] <- as.vector(raster("Input_data/Organic/Livestock/Production/goat_milk_liters_year.tif"))
			matrix_livestock_production[,,4] <- as.vector(raster("Input_data/Organic/Livestock/Production/goats_carcass_weigth.tif"))
			matrix_livestock_production[,,5] <- as.vector(raster("Input_data/Organic/Livestock/Production/sheep_milk_liters_year.tif"))
			matrix_livestock_production[,,6] <- as.vector(raster("Input_data/Organic/Livestock/Production/sheep_carcass_weigth.tif"))
			matrix_livestock_production[,,7] <- as.vector(raster("Input_data/Organic/Livestock/Production/pigs_carcass_weigth.tif"))
			matrix_livestock_production[,,8] <- as.vector(raster("Input_data/Organic/Livestock/Production/hens_production_egg.tif"))
   ## ________  LIVESTOCK  coefficients to calculate effective livestick numbers in one year (for livestock species where the anima l turnover is lower then one year)_________

    matrix_goats_meat_heads_year <- array(dim=c(1,9331200))                     # to be used to calculate correct animal numbers for production
    			matrix_goats_meat_heads_year[,] <- as.vector(raster("Input_data/Organic/Livestock/Production/goats_meat_head_year.tif"))
    matrix_sheep_meat_production <- array(dim=c(1,9331200))                     # to be used to calculate correct animal numbers for production
    			matrix_sheep_meat_production[,] <- as.vector(raster("Input_data/Organic/Livestock/Production/sheep_meat_head_year.tif"))
    matrix_pigs_production <- array(dim=c(1,9331200))                           # to be used to calculate correct animal numbers for production
			matrix_pigs_production[,] <- as.vector(raster("Input_data/Organic/Livestock/Production/pigs_meat_head_year.tif"))


	cattle_TLU <- array(dim=c(1,9331200))
			cattle_TLU[,] <- as.vector(raster("Input_data/Organic/Livestock/Production/cows_TLU.tif"))

   ## ____________  LEACHING MAPS for correction of manure and N from stovers recycled _____________
	  leaching_areas_mask <- raster("Input_data/Organic/Nitrogen/mask_area_leaching_final_irrigation_above_005.tif") # Load leaching areas. The areas where leaching occurs were estimated according to the IPCC procedure. See "Estimation_areas_of_N_leaching.R" in /Pietro/R Analysis/ Global convetion.../ Estimation areas of.../R_code
	  mask_correction_leaching <- leaching_areas_mask * 0.86                                                           # 0.24: correction to estimate the 24% losses due to leaching
	  mask_correction_leaching <- reclassify(mask_correction_leaching, cbind(0,1))
	  mask_correction_leaching_vec <- array(dim=c(1, 9331200))
	  mask_correction_leaching_vec[,] <- as.vector(mask_correction_leaching)

  N_deposition <- array(dim=c(1,9331200,1))                                                                         # N atmospheric depostions 1993 per hectare and cut per cropland area corrected by leaching --> to be multiplied by crop area to get amoun in tons, dentner et al.
		N_deposition[,,1] <- as.vector(raster("Input_data/Organic/Nitrogen/N_deposition_per_hectare_of_cropland2000.tif") * mask_correction_leaching)


   ## ____________  CONVENTIONAL FARMING INPUTS - from conventional submodel results _____________

  N_conventional_manure <- array(dim=c(1,9331200))                           # N from conventional manure surplus computed in the conventional submodel - non residistributed
		N_conventional_manure[,] <- as.vector(raster("Input_data/Conventional/manure_N_surplus_total_100_manure_cropland_correct_list_countries.tif"))

		# N_conventional_manure[,] <- as.vector(raster("Script_generated_files/N_redistributed_manure_surplus_100_correct_list_countries.tif"))



  N_conventional_waste_water <- array(dim=c(1,9331200))                      # N from conventional waste water surplus computed in the conventional submodel residistributed; current population and diets
		waste_water <- extend(raster("Input_data/Conventional/wastewater_N_surplus_total_100_manure_cropland_correct_list_countries.tif"), extent(-180,180,-90,90))
		N_conventional_waste_water[,] <- as.vector(waste_water)





  ## Generate arrays for storing the optimisation results  --------------------------

matrix_final_yield <- array(dim=c(1,9331200,61))                          # Final organic yiels
matrix_final_N_assigned_to_crop <- array(dim=c(1,9331200,61))             # Quantity of Na vailable repartitioned to crops
matrix_sub_food_yield <- array(dim=c(1,9331200,61))                       # Sub FOOD yield
matrix_sub_feed_yield <- array(dim=c(1,9331200,61))                       # Sub FEED yield
number_of_livestock_heads <- array(dim=c(1,9331200,9))                    # Numbers of heads of the different livestock species, i.e. sum of all producing heads in one year
number_of_livestock_standing_animals <- array(dim=c(1,9331200,4))         # Numbers of standing animal at the counting date (i.e. end of the year). Dairies species, beef and hens number of heads is the same then the standing numbers. Standing numbers differ for meat sheep and goats, pigs, and broilers
N_total_available <- array(dim=c(1,9331200,1))                            # N_total_available_in_orgnic_manure
BNF <- array(dim=c(1,9331200,length(which(Fixed_N!=0))))                  # Crop fixed BNF


statistics <- array(dim=c(1,9331200,11))                                  # array containing calculations for statistics and other infos
counter <- array(dim=c(1,9331200))                                        # counter_for_model_failures_in_finding_a_solution  [0-1]


  ## Diets limit share  --------------------------


Limit_share<-array(dim=c(1,9331200,2))
Limit_share[,,1]<-as.vector(raster("Input_data/Stat/Share_limit_dairy.tif"))
Limit_share[,,2]<-as.vector(raster("Input_data/Stat/Share_limit_meat.tif"))





# -------------------------------------------------  3 OPTIMISATION PROCEDURE   ------------------------------------------


i=1

                      #j=3656019  Location: Haiti           1869401      2893545    2335105   #186624
## sensitivity initalisation, script initialisation, value to modify 2911188:3142540.
scenario_S_no <- 1
script_no <- 1


for (j in 1:9331200) {

if (is.na(matrix_area_vec[i,j,1])==FALSE) {   # Run optimisation only if there is some  land in the gridcell (excuding oceans)


			# 1st part of the matrix giving the 61 yield curve constraint for the HARVESTED BIOMASS
sparse_matrix_1 <- simple_triplet_diag_matrix(rep(1,61), nrow=61)                                                      #  Matrix square 1
sparse_matrix_2 <- simple_triplet_diag_matrix(-Crops_yield_curve_slope[], nrow=61)                                     #  Matrix square 2
sparse_matrix_3 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 3
sparse_matrix_4 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 4
sparse_matrix_5 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 5
sparse_matrix_6 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 6
sparse_matrix_7 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 7
sparse_matrix_8 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 8


			# 2nd part of the matrix giving the 61 yield curve PLATEAU constraint for the HARVESTED BIOMASS
sparse_matrix_9 <- simple_triplet_diag_matrix(rep(1, 61), nrow=61)                                                      #  Matrix square 9
sparse_matrix_10 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 10
sparse_matrix_11 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 11
sparse_matrix_12 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 12
sparse_matrix_13 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 13
sparse_matrix_14 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 14
sparse_matrix_15 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 15
sparse_matrix_16 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 16



			# 3rd part of the matrix giving the 61 N required by EXPORTED STOVERS BIOMASS + LOSSES constraint (losses=leaching and volatilisation stovers returned to soils)
sparse_matrix_17 <- simple_triplet_diag_matrix(Stovers_RPR[], nrow=61)                                                  #  Matrix square 17
sparse_matrix_18 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 18
														#sparse_matrix_15 <- simple_triplet_diag_matrix((ifelse(-(Stovers_yield_curve_slope*((crop_res_avail_org[i,j]*(1-crop_res_avail_org[i,j]))/((1-crop_res_avail_org[i,j])+(2-(0.99+mask_correction_leaching_vec[i,j]))*crop_res_avail_org[i,j])))==-Inf,
														#														0, -(Stovers_yield_curve_slope*((crop_res_avail_org[i,j]*(1-crop_res_avail_org[i,j]))/((1-crop_res_avail_org[i,j])+(2-(0.99+mask_correction_leaching_vec[i,j]))*crop_res_avail_org[i,j]))))),
														#														nrow=61)
														                                                                #  Matrix square 15

#sparse_matrix_19  <- simple_triplet_diag_matrix((ifelse(-((Stovers_yield_curve_slope*1)/(crop_res_avail_org[i,j]+(2-(0.99+mask_correction_leaching_vec[i,j]))*(1-crop_res_avail_org[i,j])))==-Inf, 0,
#													   -((Stovers_yield_curve_slope*1)/(crop_res_avail_org[i,j]+(2-(0.99+mask_correction_leaching_vec[i,j]))*(1-crop_res_avail_org[i,j]))))),nrow=61)
sparse_matrix_19  <- simple_triplet_diag_matrix((ifelse(-((Stovers_yield_curve_slope*1)/(crop_res_avail_org[i,j]*crop_info$Fodders+(2-(0.99+mask_correction_leaching_vec[i,j]))*(1-crop_res_avail_org[i,j]*crop_info$Fodders)))==-Inf, 0,
													   -((Stovers_yield_curve_slope*1)/(crop_res_avail_org[i,j]*crop_info$Fodders+(2-(0.99+mask_correction_leaching_vec[i,j]))*(1-crop_res_avail_org[i,j]*crop_info$Fodders))))),nrow=61)  #This option consider the production of residues from fodder crops
														                                                               #  Matrix square 19   # According to IPCC crop residues losses are only direct N2O emissions and leaching but NOT indirect N2O emissions
sparse_matrix_20 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 20
sparse_matrix_21 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 21
sparse_matrix_22 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 22
sparse_matrix_23 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 23
sparse_matrix_24 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 24


			# 4th part of the matrix giving the 61 constraint for fixed nitrogen by pulses crops and by roots/cereals/oilcrops/sugarcane

#sparse_matrix_25 <- simple_triplet_diag_matrix(Fixed_N[], nrow=61)                                                     #  Matrix square 25
sparse_matrix_25 <- simple_triplet_diag_matrix(c(Fixed_N[1:3], ifelse(matrix_yield_max_vec[i,j,4]>0, Fixed_N[4]/matrix_yield_max_vec[i,j,4], 0),Fixed_N[5:6], ifelse(matrix_yield_max_vec[i,j,7]>0,Fixed_N[7]/matrix_yield_max_vec[i,j,7], 0), Fixed_N[8:9], ifelse(matrix_yield_max_vec[i,j,10]>0, Fixed_N[10]/matrix_yield_max_vec[i,j,10], 0), Fixed_N[11:13], ifelse(matrix_yield_max_vec[i,j,14]>0,Fixed_N[14]/matrix_yield_max_vec[i,j,14], 0),
												 Fixed_N[15:21], ifelse(matrix_yield_max_vec[i,j,22]>0,Fixed_N[22]/matrix_yield_max_vec[i,j,22], 0), Fixed_N[23:24], ifelse(matrix_yield_max_vec[i,j,25]>0,Fixed_N[25]/matrix_yield_max_vec[i,j,25], 0), ifelse(matrix_yield_max_vec[i,j,26]>0,Fixed_N[26]/matrix_yield_max_vec[i,j,26], 0), Fixed_N[27:28], ifelse(matrix_yield_max_vec[i,j,29]>0,Fixed_N[29]/matrix_yield_max_vec[i,j,29], 0),
												 Fixed_N[30], ifelse(matrix_yield_max_vec[i,j,31]>0,Fixed_N[31]/matrix_yield_max_vec[i,j,31], 0), ifelse(matrix_yield_max_vec[i,j,32]>0,Fixed_N[32]/matrix_yield_max_vec[i,j,32], 0), Fixed_N[33], ifelse(matrix_yield_max_vec[i,j,34]>0,Fixed_N[34]/matrix_yield_max_vec[i,j,34], 0), Fixed_N[35:39], ifelse(matrix_yield_max_vec[i,j,40]>0,Fixed_N[40]/matrix_yield_max_vec[i,j,40], 0),
												 Fixed_N[41], ifelse(matrix_yield_max_vec[i,j,42]>0,Fixed_N[42]/matrix_yield_max_vec[i,j,42], 0), ifelse(matrix_yield_max_vec[i,j,43]>0,Fixed_N[43]/matrix_yield_max_vec[i,j,43], 0), Fixed_N[44], ifelse(matrix_yield_max_vec[i,j,45]>0,Fixed_N[45]/matrix_yield_max_vec[i,j,45], 0), ifelse(matrix_yield_max_vec[i,j,46]>0,Fixed_N[46]/matrix_yield_max_vec[i,j,46], 0), ifelse(matrix_yield_max_vec[i,j,47]>0,Fixed_N[47]/matrix_yield_max_vec[i,j,47], 0),
												 Fixed_N[48:49], ifelse(matrix_yield_max_vec[i,j,50]>0,Fixed_N[50]/matrix_yield_max_vec[i,j,50], 0),  ifelse(matrix_yield_max_vec[i,j,51]>0,Fixed_N[51]/matrix_yield_max_vec[i,j,51], 0), ifelse(matrix_yield_max_vec[i,j,52]>0,Fixed_N[52]/matrix_yield_max_vec[i,j,52], 0), Fixed_N[53:55], ifelse(matrix_yield_max_vec[i,j,56]>0,Fixed_N[56]/matrix_yield_max_vec[i,j,56], 0), Fixed_N[57:59], ifelse(matrix_yield_max_vec[i,j,60]>0,Fixed_N[60]/matrix_yield_max_vec[i,j,60], 0),
												 ifelse(matrix_yield_max_vec[i,j,61]>0,Fixed_N[61]/matrix_yield_max_vec[i,j,61], 0)), nrow=61)
sparse_matrix_26 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 26
sparse_matrix_27 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 27
sparse_matrix_28 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 28
sparse_matrix_29 <- simple_triplet_diag_matrix(rep(-1,61), nrow=61)                                                    #  Matrix square 29
sparse_matrix_30 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 30
sparse_matrix_31 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 31
sparse_matrix_32 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 32


			# 5th part of the matrix giving the 61 constraint for nitrogen balancing for each crop including Ngrain, Nres, Npool, Nfixed, and Nfertilisers

sparse_matrix_33 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 33
sparse_matrix_34 <- simple_triplet_diag_matrix(matrix_area_vec[i,j,], nrow=61)                                         #  Matrix square 34    N_grain
sparse_matrix_35 <- simple_triplet_diag_matrix(matrix_area_vec[i,j,], nrow=61)                                         #  Matrix square 35    N_residues
sparse_matrix_36 <- simple_triplet_diag_matrix(matrix_area_vec[i,j,], nrow=61)                                         #  Matrix square 36    N_pool
sparse_matrix_37 <- simple_triplet_diag_matrix(-matrix_area_vec[i,j,], nrow=61)                                        #  Matrix square 37    N_fixed
sparse_matrix_38 <- simple_triplet_diag_matrix(-matrix_area_vec[i,j,], nrow=61)                                        #  Matrix square 38    N_fertilisers
sparse_matrix_39 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 39
sparse_matrix_40 <- simple_triplet_zero_matrix(61)                                                                     #  Matrix square 40


			# 6th part of the matrix giving the 1 constraint for nitrogen availability for N from fertilisers

sparse_matrix_41 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 41
sparse_matrix_42 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 42
sparse_matrix_43 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 43
sparse_matrix_44 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))           #c(-matrix_area_vec[i,j,]))      #  Matrix square 44
sparse_matrix_45 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 45
sparse_matrix_46 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(matrix_area_vec[i,j,] / (1 - (EF_org[i,j,]+0.0066 + 0.21 + (1- mask_correction_leaching_vec[i,j])))))                             #  Matrix square 46
sparse_matrix_47 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 47
sparse_matrix_48 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 48


																					#  *****  OLD ****   4th part of the matrix giving the total available nitrogen constraint for growing crops AND EXPORTED stovers
																		#sparse_matrix_10 <- simple_triplet_matrix(rep(1, 61), seq(1:61), rep(0,61))                                            #  Matrix square 10
																		#sparse_matrix_11 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(matrix_area_vec[i,j,]))                             #  Matrix square 11
																		#sparse_matrix_12 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(matrix_area_vec[i,j,]))                             #  Matrix square 12


			# 7th part of the matrix defining the 61 sub_food and sub_feed yields

sparse_matrix_49 <- simple_triplet_diag_matrix(rep(-1,61), nrow=61)                                                     #  Matrix square 49    Total_simulated_yields
sparse_matrix_50 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 50
sparse_matrix_51 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 51
sparse_matrix_52 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 52
sparse_matrix_53 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 53
sparse_matrix_54 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 54
sparse_matrix_55 <- simple_triplet_diag_matrix(rep(1,61), nrow=61)                                                      #  Matrix square 55    Food Sub_yields
sparse_matrix_56 <- simple_triplet_diag_matrix(rep(1,61), nrow=61)                                                      #  Matrix square 56    Feed Sub_yields

			# 8th part of the matrix defining the 61 maximum available Sub_yield for feed

sparse_matrix_57 <- simple_triplet_diag_matrix(rep(share_available_feed,61), nrow=61)                                   #  Matrix square 57    Share_available_yeidl_for_feed
sparse_matrix_58 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 58
sparse_matrix_59 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 59
sparse_matrix_60 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 60
sparse_matrix_61 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 61
sparse_matrix_62 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 62
sparse_matrix_63 <- simple_triplet_zero_matrix(61)                                                                      #  Matrix square 63
sparse_matrix_64 <- simple_triplet_diag_matrix(rep(-1,61), nrow=61)                                                     #  Matrix square 64    Feed Sub_yields maximum constraint



			# 9th part of the matrix giving the total available ENERGY from GRAIN CROPS
sparse_matrix_65 <- simple_triplet_matrix(rep(1,427), seq(1:427), rep(0,427))
sparse_matrix_66 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(0,matrix_area_vec[i,j,2]*Energy_crops[2],matrix_area_vec[i,j,3]*Energy_crops[3],matrix_area_vec[i,j,4]*Energy_crops[4],matrix_area_vec[i,j,5]*Energy_crops[5],matrix_area_vec[i,j,6]*Energy_crops[6],matrix_area_vec[i,j,7]*Energy_crops[7],0,0,matrix_area_vec[i,j,10]*Energy_crops[10],matrix_area_vec[i,j,11]*Energy_crops[11],0,0,0,0,matrix_area_vec[i,j,16]*Energy_crops[16],
																  matrix_area_vec[i,j,17]*Energy_crops[17],0,0,0,0,matrix_area_vec[i,j,22]*Energy_crops[22],0,matrix_area_vec[i,j,24]*Energy_crops[24],matrix_area_vec[i,j,25]*Energy_crops[25],matrix_area_vec[i,j,26]*Energy_crops[26],0,0,matrix_area_vec[i,j,29]*Energy_crops[29],0,matrix_area_vec[i,j,31]*Energy_crops[31],
																  matrix_area_vec[i,j,32]*Energy_crops[32],0,0,0,0,matrix_area_vec[i,j,37]*Energy_crops[37],matrix_area_vec[i,j,38]*Energy_crops[38],matrix_area_vec[i,j,39]*Energy_crops[39],matrix_area_vec[i,j,40]*Energy_crops[40],matrix_area_vec[i,j,41]*Energy_crops[41],matrix_area_vec[i,j,42]*Energy_crops[42],matrix_area_vec[i,j,43]*Energy_crops[43],0,
																  matrix_area_vec[i,j,45]*Energy_crops[45],matrix_area_vec[i,j,46]*Energy_crops[46],matrix_area_vec[i,j,47]*Energy_crops[47],matrix_area_vec[i,j,48]*Energy_crops[48],matrix_area_vec[i,j,49]*Energy_crops[49],matrix_area_vec[i,j,50]*Energy_crops[50],matrix_area_vec[i,j,51]*Energy_crops[51],matrix_area_vec[i,j,52]*Energy_crops[52],0,0,0,
																  matrix_area_vec[i,j,56]*Energy_crops[56],0,0,0, matrix_area_vec[i,j,60]*Energy_crops[60],matrix_area_vec[i,j,61]*Energy_crops[61]))
																  #  Matrix square GRAIN ENERGY feed coefficients crops --> Sub_feed_yield * Area * Energy_density

			# 10th part of the matrix giving the total available ENERGY from FODDER CROPS
sparse_matrix_67 <- simple_triplet_matrix(rep(1,427), seq(1:427), rep(0,427))
sparse_matrix_68 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(matrix_area_vec[i,j,1]*Energy_crops[1],0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,12]*Energy_crops[12],0,0,0,0,0,matrix_area_vec[i,j,18]*Energy_crops[18],
																0,0,matrix_area_vec[i,j,21]*Energy_crops[21],0,matrix_area_vec[i,j,23]*Energy_crops[23],0,0,0,matrix_area_vec[i,j,27]*Energy_crops[27],0,0,matrix_area_vec[i,j,30]*Energy_crops[30],0,0,matrix_area_vec[i,j,33]*Energy_crops[33],0,0,0,
																0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,58]*Energy_crops[58],0,
																0,0))   #  Matrix square FODDER ENERGY feed coefficients crops --> Sub_feed_yield * Area * Energy_density


#			sparse_matrix_67 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(matrix_area_vec[i,j,1]*Energy_crops[1],0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,12]*Energy_crops[12],0,0,0,0,0,matrix_area_vec[i,j,18]*Energy_crops[18],
#																			0,0,matrix_area_vec[i,j,21]*Energy_crops[21],0,matrix_area_vec[i,j,23]*Energy_crops[23],0,0,0,matrix_area_vec[i,j,27]*Energy_crops[27],0,0,matrix_area_vec[i,j,30]*Energy_crops[30],0,0,matrix_area_vec[i,j,33]*Energy_crops[33],0,0,0,
#																			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,58]*Energy_crops[58],0,
#																			0,0))           #  Matrix square FODDER ENERGY feed coefficients crops
#			sparse_matrix_68 <- simple_triplet_matrix(rep(1,427), seq(1:427), rep(0,427))


			# 11th part of the matrix giving the total available ENERGY from STOVERS CROPS
sparse_matrix_69 <- simple_triplet_matrix(rep(1,427), seq(1:427), rep(0,427))
sparse_matrix_70 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(0,0,0,matrix_area_vec[i,j,4]*Energy_stovers[4]*Stovers_RPR[4]*crop_res_avail_org[i,j],matrix_area_vec[i,j,5]*Energy_stovers[5]*Stovers_RPR[5]*crop_res_avail_org[i,j],matrix_area_vec[i,j,6]*Energy_stovers[6]*Stovers_RPR[6]*crop_res_avail_org[i,j],matrix_area_vec[i,j,7]*Energy_stovers[7]*Stovers_RPR[7]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,8]*Energy_stovers[8]*Stovers_RPR[8]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,10]*Energy_stovers[10]*Stovers_RPR[10]*crop_res_avail_org[i,j],matrix_area_vec[i,j,11]*Energy_stovers[11]*Stovers_RPR[11]*crop_res_avail_org[i,j],0,0,0,0,matrix_area_vec[i,j,16]*Energy_stovers[16]*Stovers_RPR[16]*crop_res_avail_org[i,j],matrix_area_vec[i,j,17]*Energy_stovers[17]*Stovers_RPR[17]*crop_res_avail_org[i,j],0,0,0,0,matrix_area_vec[i,j,22]*Energy_stovers[22]*Stovers_RPR[22]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,24]*Energy_stovers[24]*Stovers_RPR[24]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,25]*Energy_stovers[25]*Stovers_RPR[25]*crop_res_avail_org[i,j],matrix_area_vec[i,j,26]*Energy_stovers[26]*Stovers_RPR[26]*crop_res_avail_org[i,j],0,0,matrix_area_vec[i,j,29]*Energy_stovers[29]*Stovers_RPR[29]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,31]*Energy_stovers[31]*Stovers_RPR[31]*crop_res_avail_org[i,j],0,0,0,matrix_area_vec[i,j,35]*Energy_stovers[35]*Stovers_RPR[35]*crop_res_avail_org[i,j],0,
																  matrix_area_vec[i,j,37]*Energy_stovers[37]*Stovers_RPR[37]*crop_res_avail_org[i,j],matrix_area_vec[i,j,38]*Energy_stovers[38]*Stovers_RPR[38]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,40]*Energy_stovers[40]*Stovers_RPR[40]*crop_res_avail_org[i,j],matrix_area_vec[i,j,41]*Energy_stovers[41]*Stovers_RPR[41]*crop_res_avail_org[i,j],matrix_area_vec[i,j,42]*Energy_stovers[42]*Stovers_RPR[42]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,43]*Energy_stovers[43]*Stovers_RPR[43]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,45]*Energy_stovers[45]*Stovers_RPR[45]*crop_res_avail_org[i,j],matrix_area_vec[i,j,46]*Energy_stovers[46]*Stovers_RPR[46]*crop_res_avail_org[i,j],matrix_area_vec[i,j,47]*Energy_stovers[47]*Stovers_RPR[47]*crop_res_avail_org[i,j],matrix_area_vec[i,j,48]*Energy_stovers[48]*Stovers_RPR[48]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,49]*Energy_stovers[49]*Stovers_RPR[49]*crop_res_avail_org[i,j],matrix_area_vec[i,j,50]*Energy_stovers[50]*Stovers_RPR[50]*crop_res_avail_org[i,j],matrix_area_vec[i,j,51]*Energy_stovers[51]*Stovers_RPR[51]*crop_res_avail_org[i,j],matrix_area_vec[i,j,52]*Energy_stovers[52]*Stovers_RPR[52]*crop_res_avail_org[i,j],0,0,matrix_area_vec[i,j,55]*Energy_stovers[55]*Stovers_RPR[55]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,56]*Energy_stovers[56]*Stovers_RPR[56]*crop_res_avail_org[i,j],matrix_area_vec[i,j,57]*Energy_stovers[57]*Stovers_RPR[57]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,59]*Energy_stovers[59]*Stovers_RPR[59]*crop_res_avail_org[i,j],matrix_area_vec[i,j,60]*Energy_stovers[60]*Stovers_RPR[60]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,61]*Energy_stovers[61]*Stovers_RPR[61]*crop_res_avail_org[i,j]))
																  #  Matrix square STOVERS ENERGY feed coefficients crops --> Sub_feed_yield * Area * Energy_density

#			sparse_matrix_69 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(0,0,0,matrix_area_vec[i,j,4]*Energy_stovers[4]*Stovers_RPR[4]*crop_res_avail_org[i,j],matrix_area_vec[i,j,5]*Energy_stovers[5]*Stovers_RPR[5]*crop_res_avail_org[i,j],matrix_area_vec[i,j,6]*Energy_stovers[6]*Stovers_RPR[6]*crop_res_avail_org[i,j],matrix_area_vec[i,j,7]*Energy_stovers[7]*Stovers_RPR[7]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,8]*Energy_stovers[8]*Stovers_RPR[8]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,10]*Energy_stovers[10]*Stovers_RPR[10]*crop_res_avail_org[i,j],matrix_area_vec[i,j,11]*Energy_stovers[11]*Stovers_RPR[11]*crop_res_avail_org[i,j],0,0,0,0,matrix_area_vec[i,j,16]*Energy_stovers[16]*Stovers_RPR[16]*crop_res_avail_org[i,j],matrix_area_vec[i,j,17]*Energy_stovers[17]*Stovers_RPR[17]*crop_res_avail_org[i,j],0,0,0,0,matrix_area_vec[i,j,22]*Energy_stovers[22]*Stovers_RPR[22]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,24]*Energy_stovers[24]*Stovers_RPR[24]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,25]*Energy_stovers[25]*Stovers_RPR[25]*crop_res_avail_org[i,j],matrix_area_vec[i,j,26]*Energy_stovers[26]*Stovers_RPR[26]*crop_res_avail_org[i,j],0,0,matrix_area_vec[i,j,29]*Energy_stovers[29]*Stovers_RPR[29]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,31]*Energy_stovers[31]*Stovers_RPR[31]*crop_res_avail_org[i,j],0,0,0,matrix_area_vec[i,j,35]*Energy_stovers[35]*Stovers_RPR[35]*crop_res_avail_org[i,j],0,
#																			  matrix_area_vec[i,j,37]*Energy_stovers[37]*Stovers_RPR[37]*crop_res_avail_org[i,j],matrix_area_vec[i,j,38]*Energy_stovers[38]*Stovers_RPR[38]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,40]*Energy_stovers[40]*Stovers_RPR[40]*crop_res_avail_org[i,j],matrix_area_vec[i,j,41]*Energy_stovers[41]*Stovers_RPR[41]*crop_res_avail_org[i,j],matrix_area_vec[i,j,42]*Energy_stovers[42]*Stovers_RPR[42]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,43]*Energy_stovers[43]*Stovers_RPR[43]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,45]*Energy_stovers[45]*Stovers_RPR[45]*crop_res_avail_org[i,j],matrix_area_vec[i,j,46]*Energy_stovers[46]*Stovers_RPR[46]*crop_res_avail_org[i,j],matrix_area_vec[i,j,47]*Energy_stovers[47]*Stovers_RPR[47]*crop_res_avail_org[i,j],matrix_area_vec[i,j,48]*Energy_stovers[48]*Stovers_RPR[48]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,49]*Energy_stovers[49]*Stovers_RPR[49]*crop_res_avail_org[i,j],matrix_area_vec[i,j,50]*Energy_stovers[50]*Stovers_RPR[50]*crop_res_avail_org[i,j],matrix_area_vec[i,j,51]*Energy_stovers[51]*Stovers_RPR[51]*crop_res_avail_org[i,j],matrix_area_vec[i,j,52]*Energy_stovers[52]*Stovers_RPR[52]*crop_res_avail_org[i,j],0,0,matrix_area_vec[i,j,55]*Energy_stovers[55]*Stovers_RPR[55]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,56]*Energy_stovers[56]*Stovers_RPR[56]*crop_res_avail_org[i,j],matrix_area_vec[i,j,57]*Energy_stovers[57]*Stovers_RPR[57]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,59]*Energy_stovers[59]*Stovers_RPR[59]*crop_res_avail_org[i,j],matrix_area_vec[i,j,60]*Energy_stovers[60]*Stovers_RPR[60]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,61]*Energy_stovers[61]*Stovers_RPR[61]*crop_res_avail_org[i,j]))   #  Matrix square STOVERS ENERGY feed coefficients crops
#			sparse_matrix_70 <- simple_triplet_matrix(rep(1,427), seq(1:427), rep(0,427))


			# 12th part of the matrix giving the total available PROTEIN from GRAIN CROPS
sparse_matrix_71 <- simple_triplet_matrix(rep(1,427), seq(1:427), rep(0,427))
sparse_matrix_72 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(0,matrix_area_vec[i,j,2]*Protein_crops[2],matrix_area_vec[i,j,3]*Protein_crops[3],matrix_area_vec[i,j,4]*Protein_crops[4],matrix_area_vec[i,j,5]*Protein_crops[5],matrix_area_vec[i,j,6]*Protein_crops[6],matrix_area_vec[i,j,7]*Protein_crops[7],0,0,matrix_area_vec[i,j,10]*Protein_crops[10],matrix_area_vec[i,j,11]*Protein_crops[11],0,0,0,0,matrix_area_vec[i,j,16]*Protein_crops[16],
																  matrix_area_vec[i,j,17]*Protein_crops[17],0,0,0,0,matrix_area_vec[i,j,22]*Protein_crops[22],0,matrix_area_vec[i,j,24]*Protein_crops[24],matrix_area_vec[i,j,25]*Protein_crops[25],matrix_area_vec[i,j,26]*Protein_crops[26],0,0,matrix_area_vec[i,j,29]*Protein_crops[29],0,matrix_area_vec[i,j,31]*Protein_crops[31],
																  matrix_area_vec[i,j,32]*Protein_crops[32],0,0,0,0,matrix_area_vec[i,j,37]*Protein_crops[37],matrix_area_vec[i,j,38]*Protein_crops[38],matrix_area_vec[i,j,39]*Protein_crops[39],matrix_area_vec[i,j,40]*Protein_crops[40],matrix_area_vec[i,j,41]*Protein_crops[41],matrix_area_vec[i,j,42]*Protein_crops[42],matrix_area_vec[i,j,43]*Protein_crops[43],0,
																  matrix_area_vec[i,j,45]*Protein_crops[45],matrix_area_vec[i,j,46]*Protein_crops[46],matrix_area_vec[i,j,47]*Protein_crops[47],matrix_area_vec[i,j,48]*Protein_crops[48],matrix_area_vec[i,j,49]*Protein_crops[49],matrix_area_vec[i,j,50]*Protein_crops[50],matrix_area_vec[i,j,51]*Protein_crops[51],matrix_area_vec[i,j,52]*Protein_crops[52],0,0,0,
																  matrix_area_vec[i,j,56]*Protein_crops[56],0,0,0, matrix_area_vec[i,j,60]*Protein_crops[60],matrix_area_vec[i,j,61]*Protein_crops[61]))
																  #  Matrix square GRAIN Protein feed coefficients crops --> Sub_feed_yield * Area * Protein_density



			# 13th part of the matrix giving the total available PROTEINS from FODDER CROPS
sparse_matrix_73 <- simple_triplet_matrix(rep(1,427), seq(1:427), rep(0,427))
sparse_matrix_74 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(matrix_area_vec[i,j,1]*Protein_crops[1],0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,12]*Protein_crops[12],0,0,0,0,0,matrix_area_vec[i,j,18]*Protein_crops[18],
																0,0,matrix_area_vec[i,j,21]*Protein_crops[21],0,matrix_area_vec[i,j,23]*Protein_crops[23],0,0,0,matrix_area_vec[i,j,27]*Protein_crops[27],0,0,matrix_area_vec[i,j,30]*Protein_crops[30],0,0,matrix_area_vec[i,j,33]*Protein_crops[33],0,0,0,
																0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,58]*Protein_crops[58],0,
																0,0))   #  Matrix square FODDER PROTEINS feed coefficients crops --> Sub_feed_yield * Area * Protein_density

#			sparse_matrix_73 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(matrix_area_vec[i,j,1]*Protein_crops[1],0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,12]*Protein_crops[12],0,0,0,0,0,matrix_area_vec[i,j,18]*Protein_crops[18],
#																			0,0,matrix_area_vec[i,j,21]*Protein_crops[21],0,matrix_area_vec[i,j,23]*Protein_crops[23],0,0,0,matrix_area_vec[i,j,27]*Protein_crops[27],0,0,matrix_area_vec[i,j,30]*Protein_crops[30],0,0,matrix_area_vec[i,j,33]*Protein_crops[33],0,0,0,
#																			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,58]*Protein_crops[58],0,
#																			0,0))           #  Matrix square FODDER PROTEIN feed coefficients crops
#			sparse_matrix_74 <- simple_triplet_matrix(rep(1,427), seq(1:427), rep(0,427))


			# 14th part of the matrix giving the total available PROTEINS from STOVERS CROPS
sparse_matrix_75 <- simple_triplet_matrix(rep(1,427), seq(1:427), rep(0,427))
sparse_matrix_76 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(0,0,0,matrix_area_vec[i,j,4]*Protein_stovers[4]*Stovers_RPR[4]*crop_res_avail_org[i,j],matrix_area_vec[i,j,5]*Protein_stovers[5]*Stovers_RPR[5]*crop_res_avail_org[i,j],matrix_area_vec[i,j,6]*Protein_stovers[6]*Stovers_RPR[6]*crop_res_avail_org[i,j],matrix_area_vec[i,j,7]*Protein_stovers[7]*Stovers_RPR[7]*crop_res_avail_org[i,j],
																matrix_area_vec[i,j,8]*Protein_stovers[8]*Stovers_RPR[8]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,10]*Protein_stovers[10]*Stovers_RPR[10]*crop_res_avail_org[i,j],matrix_area_vec[i,j,11]*Protein_stovers[11]*Stovers_RPR[11]*crop_res_avail_org[i,j],0,0,0,0,matrix_area_vec[i,j,16]*Protein_stovers[16]*Stovers_RPR[16]*crop_res_avail_org[i,j],matrix_area_vec[i,j,17]*Protein_stovers[17]*Stovers_RPR[17]*crop_res_avail_org[i,j],0,0,0,0,matrix_area_vec[i,j,22]*Protein_stovers[22]*Stovers_RPR[22]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,24]*Protein_stovers[24]*Stovers_RPR[24]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,25]*Protein_stovers[25]*Stovers_RPR[25]*crop_res_avail_org[i,j],matrix_area_vec[i,j,26]*Protein_stovers[26]*Stovers_RPR[26]*crop_res_avail_org[i,j],0,0,matrix_area_vec[i,j,29]*Protein_stovers[29]*Stovers_RPR[29]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,31]*Protein_stovers[31]*Stovers_RPR[31]*crop_res_avail_org[i,j],0,0,0,matrix_area_vec[i,j,35]*Protein_stovers[35]*Stovers_RPR[35]*crop_res_avail_org[i,j],0,
																  matrix_area_vec[i,j,37]*Protein_stovers[37]*Stovers_RPR[37]*crop_res_avail_org[i,j],matrix_area_vec[i,j,38]*Protein_stovers[38]*Stovers_RPR[38]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,40]*Protein_stovers[40]*Stovers_RPR[40]*crop_res_avail_org[i,j],matrix_area_vec[i,j,41]*Protein_stovers[41]*Stovers_RPR[41]*crop_res_avail_org[i,j],matrix_area_vec[i,j,42]*Protein_stovers[42]*Stovers_RPR[42]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,43]*Protein_stovers[43]*Stovers_RPR[43]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,45]*Protein_stovers[45]*Stovers_RPR[45]*crop_res_avail_org[i,j],matrix_area_vec[i,j,46]*Protein_stovers[46]*Stovers_RPR[46]*crop_res_avail_org[i,j],matrix_area_vec[i,j,47]*Protein_stovers[47]*Stovers_RPR[47]*crop_res_avail_org[i,j],matrix_area_vec[i,j,48]*Protein_stovers[48]*Stovers_RPR[48]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,49]*Protein_stovers[49]*Stovers_RPR[49]*crop_res_avail_org[i,j],matrix_area_vec[i,j,50]*Protein_stovers[50]*Stovers_RPR[50]*crop_res_avail_org[i,j],matrix_area_vec[i,j,51]*Protein_stovers[51]*Stovers_RPR[51]*crop_res_avail_org[i,j],matrix_area_vec[i,j,52]*Protein_stovers[52]*Stovers_RPR[52]*crop_res_avail_org[i,j],0,0,matrix_area_vec[i,j,55]*Protein_stovers[55]*Stovers_RPR[55]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,56]*Protein_stovers[56]*Stovers_RPR[56]*crop_res_avail_org[i,j],matrix_area_vec[i,j,57]*Protein_stovers[57]*Stovers_RPR[57]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,59]*Protein_stovers[59]*Stovers_RPR[59]*crop_res_avail_org[i,j],matrix_area_vec[i,j,60]*Protein_stovers[60]*Stovers_RPR[60]*crop_res_avail_org[i,j],
																  matrix_area_vec[i,j,61]*Protein_stovers[61]*Stovers_RPR[61]*crop_res_avail_org[i,j]))
																  #  Matrix square STOVERS PROTEINS feed coefficients crops --> Sub_feed_yield * Area * Protein_density

#			sparse_matrix_75 <- simple_triplet_matrix(rep(1, 61), seq(1:61), c(0,0,0,matrix_area_vec[i,j,4]*Protein_stovers[4]*Stovers_RPR[4]*crop_res_avail_org[i,j],matrix_area_vec[i,j,5]*Protein_stovers[5]*Stovers_RPR[5]*crop_res_avail_org[i,j],matrix_area_vec[i,j,6]*Protein_stovers[6]*Stovers_RPR[6]*crop_res_avail_org[i,j],matrix_area_vec[i,j,7]*Protein_stovers[7]*Stovers_RPR[7]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,8]*Protein_stovers[8]*Stovers_RPR[8]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,10]*Protein_stovers[10]*Stovers_RPR[10]*crop_res_avail_org[i,j],matrix_area_vec[i,j,11]*Protein_stovers[11]*Stovers_RPR[11]*crop_res_avail_org[i,j],0,0,0,0,matrix_area_vec[i,j,16]*Protein_stovers[16]*Stovers_RPR[16]*crop_res_avail_org[i,j],matrix_area_vec[i,j,17]*Protein_stovers[17]*Stovers_RPR[17]*crop_res_avail_org[i,j],0,0,0,0,matrix_area_vec[i,j,22]*Protein_stovers[22]*Stovers_RPR[22]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,24]*Protein_stovers[24]*Stovers_RPR[24]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,25]*Protein_stovers[25]*Stovers_RPR[25]*crop_res_avail_org[i,j],matrix_area_vec[i,j,26]*Protein_stovers[26]*Stovers_RPR[26]*crop_res_avail_org[i,j],0,0,matrix_area_vec[i,j,29]*Protein_stovers[29]*Stovers_RPR[29]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,31]*Protein_stovers[31]*Stovers_RPR[31]*crop_res_avail_org[i,j],0,0,0,matrix_area_vec[i,j,35]*Protein_stovers[35]*Stovers_RPR[35]*crop_res_avail_org[i,j],0,
#																			  matrix_area_vec[i,j,37]*Protein_stovers[37]*Stovers_RPR[37]*crop_res_avail_org[i,j],matrix_area_vec[i,j,38]*Protein_stovers[38]*Stovers_RPR[38]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,40]*Protein_stovers[40]*Stovers_RPR[40]*crop_res_avail_org[i,j],matrix_area_vec[i,j,41]*Protein_stovers[41]*Stovers_RPR[41]*crop_res_avail_org[i,j],matrix_area_vec[i,j,42]*Protein_stovers[42]*Stovers_RPR[42]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,43]*Protein_stovers[43]*Stovers_RPR[43]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,45]*Protein_stovers[45]*Stovers_RPR[45]*crop_res_avail_org[i,j],matrix_area_vec[i,j,46]*Protein_stovers[46]*Stovers_RPR[46]*crop_res_avail_org[i,j],matrix_area_vec[i,j,47]*Protein_stovers[47]*Stovers_RPR[47]*crop_res_avail_org[i,j],matrix_area_vec[i,j,48]*Protein_stovers[48]*Stovers_RPR[48]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,49]*Protein_stovers[49]*Stovers_RPR[49]*crop_res_avail_org[i,j],matrix_area_vec[i,j,50]*Protein_stovers[50]*Stovers_RPR[50]*crop_res_avail_org[i,j],matrix_area_vec[i,j,51]*Protein_stovers[51]*Stovers_RPR[51]*crop_res_avail_org[i,j],matrix_area_vec[i,j,52]*Protein_stovers[52]*Stovers_RPR[52]*crop_res_avail_org[i,j],0,0,matrix_area_vec[i,j,55]*Protein_stovers[55]*Stovers_RPR[55]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,56]*Protein_stovers[56]*Stovers_RPR[56]*crop_res_avail_org[i,j],matrix_area_vec[i,j,57]*Protein_stovers[57]*Stovers_RPR[57]*crop_res_avail_org[i,j],0,matrix_area_vec[i,j,59]*Protein_stovers[59]*Stovers_RPR[59]*crop_res_avail_org[i,j],matrix_area_vec[i,j,60]*Protein_stovers[60]*Stovers_RPR[60]*crop_res_avail_org[i,j],
#																			  matrix_area_vec[i,j,61]*Protein_stovers[61]*Stovers_RPR[61]*crop_res_avail_org[i,j]))             #  Matrix square STOVERS PROTEINS feed coefficients crops
#			sparse_matrix_76 <- simple_triplet_matrix(rep(1,427), seq(1:427), rep(0,427))


			# 15th part of the matrix giving the lines of zero before N production by animals contraint
sparse_matrix_77 <- simple_triplet_matrix(rep(1,488), seq(1:488), rep(0,488))



			# 16th part of the matrix giving the rows of zero before energy in animals products (9 animal species)
sparse_matrix_78 <- simple_triplet_matrix(c(rep(1,488), rep(2,488), rep(3,488), rep(4,488), rep(5,488), rep(6,488), rep(7,488), rep(8,488),rep(9,488)), rep(seq(1:488), 9), rep(0,4392))

			# 17th part of the matrix giving the rows of zero before calculation of animals TLU (9 animal species)
sparse_matrix_79 <- simple_triplet_matrix(c(rep(1,488), rep(2,488), rep(3,488), rep(4,488), rep(5,488), rep(6,488), rep(7,488), rep(8,488),rep(9,488)), rep(seq(1:488), 9), rep(0,4392))


			# 18th part of the matrix giving the 5 zero rows before the contraints of the minimum animal numbers by SPECIES type and PRODUCTION type
sparse_matrix_80 <- simple_triplet_matrix(c(rep(1,488), rep(2,488), rep(3,488), rep(4,488), rep(5,488)), rep(seq(1:488), 5), rep(0,2440))


			# 19th part of the matrix giving the 9 zero rows before the contraints of the maximum animal numbers by SPECIES
sparse_matrix_81 <- simple_triplet_matrix(c(rep(1,488), rep(2,488), rep(3,488), rep(4,488), rep(5,488), rep(6,488), rep(7,488), rep(8,488), rep(9,488)), rep(seq(1:488), 9), rep(0,4392))


			# 20th part of the matrix giving the contraints of the minimum yield for each clustered crop category (9 animal species)
sparse_matrix_82 <- simple_triplet_matrix(rep(1,488), seq(1:488), c(rep(0,25), 1, rep(0, 16), 1, rep(0,16), 1, 0, rep(0, 427)))                                                                       # primary cereals
sparse_matrix_83 <- simple_triplet_matrix(rep(1,488), seq(1:488), c(rep(0,3), 1, rep(0, 2), 1, rep(0,21), 1, 0, 1, rep(0,13), 1, 0, 1, rep(0,8), 1, rep(0,5), rep(0, 427)))                           # secondary cereals
sparse_matrix_84 <- simple_triplet_matrix(rep(1,488), seq(1:488), c(rep(0,4), 1, 1, rep(0, 4), 1, rep(0,5), 1, rep(0,6), 1, rep(0,12), 1, 1, rep(0,2), 1, rep(0,6), 1, rep(0,13), rep(0, 427)))       # pulses
sparse_matrix_85 <- simple_triplet_matrix(rep(1,488), seq(1:488), c(rep(0,13), 1, rep(0, 7), 1, rep(0,2), 1, rep(0,6), 1, 0, 1, rep(0,7), 1, rep(0,3), 1, rep(0,4), 1, rep(0,10), rep(0, 427)))       # oilcrops
sparse_matrix_86 <- simple_triplet_matrix(rep(1,488), seq(1:488), c(rep(0,9), 1, rep(0, 29), 1, rep(0,11), 1, rep(0,8), 1, rep(0, 427)))                                                              # roots
sparse_matrix_87 <- simple_triplet_matrix(rep(1,488), seq(1:488), c(rep(0,48), 1, 1, rep(0, 11),  rep(0, 427)) )                                                                                      # industrial
sparse_matrix_88 <- simple_triplet_matrix(rep(1,488), seq(1:488), c(1, rep(0,10), 1, rep(0, 5), 1, rep(0,2), 1, 0, 1, rep(0,3), 1, rep(0,2), 1, rep(0,2), 1, rep(0, 24), 1, rep(0,3),  rep(0, 427)))  # fodders
sparse_matrix_89 <- simple_triplet_matrix(rep(1,488), seq(1:488), c(rep(0,7), 1, rep(0, 26), 1, rep(0, 19), 1, 0, 1, 0, 1, rep(0,2),  rep(0, 427)))                                                   # vegetables
sparse_matrix_90 <- simple_triplet_matrix(rep(1,488), seq(1:488), c(0, 1, 1, rep(0, 5), 1, rep(0,3), 1, rep(0,5), 1, 1, rep(0,7), 1, rep(0,7), 1, rep(0,2), 1, rep(0,22),  rep(0, 427)))              # fruits
sparse_matrix_91 <- simple_triplet_matrix(rep(1,488), seq(1:488), c(rep(0,14), 1, 1, rep(0, 27), 1, rep(0,8), 1, 1, rep(0,7),  rep(0, 427)))                                                          # NON-FOOD crops




			# 21th Right side of the matrix defining the total N available, animal dietary requirements and N total available in cropland produced by livestock
sparse_matrix_92 <- simple_triplet_matrix(c(rep(306,28),                                                                                                                                 #  Available N for manure for fertilisation
											rep(429,28), rep(430,28), rep(431,28), rep(432,28), rep(433,28), rep(434,28), rep(435,28),                                                   #  Livestock dietary constraints, and manure production
											rep(436,28), rep(437,28), rep(438,28), rep(439,28), rep(440,28), rep(441,28), rep(442,28), rep(443,28), rep(444,28),                         #  Livestok number calculation 1
											rep(445,28), rep(446,28), rep(447,28), rep(448,28), rep(449,28), rep(450,28), rep(451,28), rep(452,28), rep(453,28),                         #  Livestok number calculation 2
											rep(454,28), rep(455,28), rep(456,28), rep(457,28), rep(458,28),                                                                             #  Livestock constraints categories (lower bound)
											rep(459,28), rep(460,28), rep(461,28), rep(462,28), rep(463,28), rep(464,28), rep(465,28), rep(466,28), rep(467,28)),    rep(seq(1:28),40),  #  Livestock constraints categories (upper bound)
																																											c(rep(0,9),-1, rep(0,9), rep(0,9),                           #  Available N for manure for fertilisation
																																											-E_grain_dietary_req[i,j,],0, rep(0,9), rep(0,9),       #  Energy Grain requirements
																																											-E_grass_dietary_req[i,j,], 0, rep(0,9), rep(0,9),      #  Energy Grass requirements
																																											-E_stovers_dietary_req[i,j,], 0, rep(0,9), rep(0,9),    #  Energy Stover requirements
																																											-P_grain_dietary_req[i,j,], 0, rep(0,9), rep(0,9),      #  Protein Grain requirements
																																											-P_grass_dietary_req[i,j,], 0, rep(0,9), rep(0,9),      #  Protein Grass requirements
																																											-P_stovers_dietary_req[i,j,], 0, rep(0,9), rep(0,9),    #  Protein Stover requirements
                                                                                                                                                                            (Nyear_livestock_production_available[i,j,] * 0.96),-1, rep(0,9), rep(0,9),  #  N in manure produced by animals corrected by losses (4% N used by other crops, EF_org + 0.66% direct N2O and N2 losses, 21% indirect NH3/NO losses, 24% leaching losses)

																																											1, rep(0,8), 0, -1, rep(0,8), rep(0,9),    #  Equalities for livestock numbers for calculate live production                              # dairy cows: 1 year manure correspond to 1 animal
																																											0, 1, rep(0,7), 0, 0, -1, rep(0,7), rep(0,9),                                                                                             # beef: 1 year manure correspond to 1 animal
																																											0, 0, 1, rep(0,6), 0, 0, 0, -1, rep(0,6), rep(0,9),                                                                                       # milk goats: 1 year manure correspond to 1 animal     
																																											0, 0, 0, matrix_goats_meat_heads_year[i,j], rep(0,5), 0, 0, 0, 0, -1, rep(0,5), rep(0,9),                                                 # meat goats
																																											0, 0, 0, 0, 1, rep(0,4),0, 0, 0, 0, 0, -1, rep(0,4), rep(0,9),                                                                            # milk sheep: 1 year manure correspond to 1 animal
																																											0, 0, 0, 0, 0, matrix_sheep_meat_production[i,j], rep(0,3), 0, 0, 0, 0, 0, 0, -1, rep(0,3), rep(0,9),                                     # meat sheep
																																											0, 0, 0, 0, 0, 0, matrix_pigs_production[i,j], rep(0,2), 0, 0, 0, 0, 0, 0, 0, -1, rep(0,2), rep(0,9),                                     # pigs
																																											0, 0, 0, 0, 0, 0, 0, 7.82/1.85, rep(0,1), 0, 0, 0, 0, 0, 0, 0, 0, -1, rep(0,1), rep(0,9),                                                      # broilers: 1 year manure correspond to 7.82 animals
																																											0, 0, 0, 0, 0, 0, 0, 0, 1,        0, 0, 0, 0, 0, 0, 0, 0, 0, -1, rep(0,9),                                                                # layers: 1 year manure correspond to 1 animal

																																											1*cattle_TLU[i,j], rep(0,8), 0, rep(0,9), -1, rep(0,8),              #  Equalities for livestock numbers for calculate livestock in  LU                   # dairy cows: 1 year manure correspond to 1 animal
																																											0, 1*cattle_TLU[i,j], rep(0,7), 0, rep(0,9), 0, -1, rep(0,7),                                                                                             # beef: 1 year manure correspond to 1 animal
																																											0, 0, 1*0.1, rep(0,6), 0, rep(0,9), 0, 0, -1, rep(0,6),                                                                                                   # milk goats: 1 year manure correspond to 1 animal
																																											0, 0, 0, matrix_goats_meat_heads_year[i,j]*0.1, rep(0,5), 0, rep(0,9), 0, 0, 0, -1, rep(0,5),                                                             # meat goats
																																											0, 0, 0, 0, 1*0.1, rep(0,4),0, rep(0,9), 0, 0, 0, 0, -1, rep(0,4),                                                                                        # milk sheep: 1 year manure correspond to 1 animal
																																											0, 0, 0, 0, 0, matrix_sheep_meat_production[i,j]*0.1, rep(0,3), 0, rep(0,9), 0, 0, 0, 0, 0, -1, rep(0,3),                                                 # meat sheep
																																											0, 0, 0, 0, 0, 0, matrix_pigs_production[i,j]*0.225, rep(0,2), 0, rep(0,9), 0, 0, 0, 0, 0, 0, -1, rep(0,2),                                               # pigs
																																											0, 0, 0, 0, 0, 0, 0, 7.82*0.01/1.85, rep(0,1), 0, rep(0,9), 0, 0, 0, 0, 0, 0, 0, -1, rep(0,1),                                                                 # broilers: 1 year manure correspond to 7.82 animals
																																											0, 0, 0, 0, 0, 0, 0, 0, 1*0.01,        0, rep(0,9), 0, 0, 0, 0, 0, 0, 0, 0, -1,                                                                           # layers: 1 year manure correspond to 1 animal

																																											1, 1, 1, matrix_goats_meat_heads_year[i,j], 1, matrix_sheep_meat_production[i,j], 0, 0, 0,   0, rep(-0,9), rep(0,9),                                       # Ruminants cannot be lower that 0.15 * all_animals head
																																											0, 0, 0, 0, 0, 0, matrix_pigs_production[i,j], 7.82/1.85, 1,    0, rep(-0,9), rep(0, 0,9),                                                                       # Monogastric cannot be lower that 0.15 * all_animals head
																																											E_livestock_products[1]* matrix_livestock_production[i,j,1] * 1.0315, 0, E_livestock_products[5]* matrix_livestock_production[i,j,3] * 1.09, 0, E_livestock_products[3]* matrix_livestock_production[i,j,5] * 1.04, 0, 0, 0, E_livestock_products[9]* matrix_livestock_production[i,j,8],                                    0, E_livestock_products[1]* matrix_livestock_production[i,j,1] * 1.0315*(-Limit_share[i,j,1]), E_livestock_products[2]* matrix_livestock_production[i,j,2]*(-Limit_share[i,j,1]), E_livestock_products[5]* matrix_livestock_production[i,j,3] * 1.09*(-Limit_share[i,j,1]), matrix_goats_meat_heads_year[i,j]*E_livestock_products[4]* matrix_livestock_production[i,j,4]*(-Limit_share[i,j,1]), E_livestock_products[3]* matrix_livestock_production[i,j,5] * 1.04*(-Limit_share[i,j,1]), matrix_sheep_meat_production[i,j]*E_livestock_products[6]* matrix_livestock_production[i,j,6]*(-Limit_share[i,j,1]), matrix_pigs_production[i,j]*E_livestock_products[7]* matrix_livestock_production[i,j,7]*(-Limit_share[i,j,1]), 7.82*E_livestock_products[8]*2*(-Limit_share[i,j,1])/1.85, E_livestock_products[9]* matrix_livestock_production[i,j,8]*(-Limit_share[i,j,1]), rep(0,9),                                                                      # Milk animals cannot be lower that 0.15 * all_animals head
																																											0, E_livestock_products[2]* matrix_livestock_production[i,j,2], 0, matrix_goats_meat_heads_year[i,j]*E_livestock_products[4]* matrix_livestock_production[i,j,4], 0, matrix_sheep_meat_production[i,j]*E_livestock_products[6]* matrix_livestock_production[i,j,6], matrix_pigs_production[i,j]*E_livestock_products[7]* matrix_livestock_production[i,j,7], 7.82*E_livestock_products[8]*2/1.85, 0,                  0, E_livestock_products[1]* matrix_livestock_production[i,j,1] * 1.0315*(-Limit_share[i,j,2]), E_livestock_products[2]* matrix_livestock_production[i,j,2]*(-Limit_share[i,j,2]), E_livestock_products[5]* matrix_livestock_production[i,j,3] * 1.09*(-Limit_share[i,j,2]), matrix_goats_meat_heads_year[i,j]*E_livestock_products[4]* matrix_livestock_production[i,j,4]*(-Limit_share[i,j,2]), E_livestock_products[3]* matrix_livestock_production[i,j,5] * 1.04*(-Limit_share[i,j,2]), matrix_sheep_meat_production[i,j]*E_livestock_products[6]* matrix_livestock_production[i,j,6]*(-Limit_share[i,j,2]), matrix_pigs_production[i,j]*E_livestock_products[7]* matrix_livestock_production[i,j,7]*(-Limit_share[i,j,2]), 7.82*E_livestock_products[8]*2*(-Limit_share[i,j,2])/1.85, E_livestock_products[9]* matrix_livestock_production[i,j,8]*(-Limit_share[i,j,2]), rep(0,9),          # Meat cannot be lower that 0.15 * all_animals head
																																											0, 0, 0, 0, 0, 0, 0, 0, 0,                              0, rep(-0,9), rep(0,9),                                                                            # Eggs animals cannot be lower that 0.15 * all_animals head

																																											#1*cattle_TLU[i,j], 1*cattle_TLU[i,j], 1*0, matrix_goats_meat_heads_year[i,j]*0, 1*0, matrix_sheep_meat_production[i,j]*0, 0, 0, 0,   0, rep(0,9), rep(-0.0,9),                         # Bovines cannot be lower that X% * all_animals LU
																																											#0, 0, 0, 0, 0, 0, matrix_pigs_production[i,j]*0.225, 7.82*0.01/1.85, 1*0.01,    0, rep(0,9), rep(-0.0, 0,9),                                                                                # Monogastric cannot be lower that X% * all_animals LU
																																											#1*cattle_TLU[i,j], 0, 1*0.1, 0, 1*0.1, 0, 0, 0, 0,                                    0, rep(0,9), rep(-0,9),                                                                             # DEACTIVE       # Milk animals cannot be lower that 0 * all_animals LU
																																											#0, 1*cattle_TLU[i,j], 0, matrix_goats_meat_heads_year[i,j]*0.1, 0, matrix_sheep_meat_production[i,j]*0.1, matrix_pigs_production[i,j]*0.225, 7.82*0.01/1.85, 0,   0, rep(0,9), rep(-0,9),      # DEACTIVE       # Meat cannot be lower that 0 * all_animals LU
																																											#0, 0, 0, 0, 0, 0, 0, 0, 1*0.01,                              0, rep(0,9), rep(-0.0,9),                                                                                                    # DEACTIVE       # Eggs animals cannot be lower that 0 * all_animals LU

																																											1, rep(0,8), 0,   rep(-1,9), rep(0,9),                                                                                                   # dairy cows maximum 80 % of total animal number head
																																											0, 1, rep(0,7), 0,   rep(-1,9), rep(0,9),                                                                                                # beef maximum 80 % of total animal number head
																																											0, 0, 1, rep(0,6), 0,  rep(-1,9), rep(0,9),                                                                                              # milk goats maximum 80 % of total animal number head
																																											0, 0, 0, matrix_goats_meat_heads_year[i,j], rep(0,5), 0,  rep(-1,9), rep(0,9),                                                           # meat goats maximum 80 % of total animal number head
																																											0, 0, 0, 0, 1, rep(0,4),0,  rep(-1,9), rep(0,9),                                                                                         # milk sheep maximum 80 % of total animal number head
																																											0, 0, 0, 0, 0, matrix_sheep_meat_production[i,j], rep(0,3), 0,  rep(-1,9),  rep(0,9),                                                    # meat sheep maximum 80 % of total animal number head
																																											0, 0, 0, 0, 0, 0, matrix_pigs_production[i,j], rep(0,2), 0,  rep(-1,9), rep(0,9),                                                        # pigs maximum 80 % of otal animal number head
																																											0, 0, 0, 0, 0, 0, 0, 7.82, rep(0,1), 0,  rep(-1,9), rep(0,9),                                                                            # broilers maximum 80 % of total animal number head
																																											0, 0, 0, 0, 0, 0, 0, 0, 1,        0,  rep(-1,9), rep(0,9)),                                                                              # layers maximum 80 % of total animal number head

																																											#1*cattle_TLU[i,j], rep(0,8), 0,  rep(0,9), rep(-1,9),                                                                                                # dairy cows maximum 80 % of total animal number LU
																																											#0, 1*cattle_TLU[i,j], rep(0,7), 0, rep(0,9),  rep(-1,9),                                                                                             # beef maximum 80 % of total animal number LU
																																											#0, 0, 1*0.1, matrix_goats_meat_heads_year[i,j]*0.1,rep(0,5), 0, rep(0,9), rep(-1,9),                                   # goats TOTAL maximum X % tof otal animal number LU
																																											#0, 0, 0, matrix_goats_meat_heads_year[i,j]*0.1, rep(0,5), 0, rep(0,9), rep(-1,9),                                                   # DEACTIVE                # meat goats maximum 80 % tof otal animal number LU
																																											#0, 0, 0, 0, 1*0.1, matrix_sheep_meat_production[i,j]*0.1,rep(0,3),0, rep(0,9), rep(-1,9),                              # sheep TOTAL maximum X % tof otal animal number LU
																																											#0, 0, 0, 0, 0, matrix_sheep_meat_production[i,j]*0.1, rep(0,3), 0, rep(0,9), rep(-1,9),                                             # DEACTIVE       # meat sheep maximum 80 % tof otal animal number LU
																																											#0, 0, 0, 0, 0, 0, matrix_pigs_production[i,j]*0.225, rep(0,2), 0, rep(0,9), rep(-1,9),                                              # DEACTIVE       # pigs maximum 80 % tof otal animal number LU
																																											#0, 0, 0, 0, 0, 0, 0, 7.82*0.01, 1*0.01, 0, rep(0,9), rep(-1,9),                                                        # Poultry TOTAL maximum X % of total animal number LU
																																											#0, 0, 0, 0, 0, 0, 0, 0, 1*0.01,        0, rep(0,9), rep(-1,9)),                                                                     # DEACTIVE       # layers maximum 80 % tof total animal number LU

																																										nrow=477, ncol=28)
																																										#  Right Matrix square containing dietary coefficients, N coefficient, N_total
																																										#  Manure is correcte by losses BEFORE application and AFTER application
																																										# *** above coefficients description: 0.96: correction to account for the 5% of crops (in terms of cropland share) that we did not include in the 61 crops
																																										# *** 0.2: correction to estimate the 20% losses due to NH3 and NO volatilisation --> range coefficient: 0.05-0.5
																																										# *** 0.3: correction to estimate the 30% losses due to leaching





			# 22th Lines summing up total crop energy and protein production for FOOD
			  food_energy <- (matrix_area_vec[i,j,]*Energy_crops)   #/ifelse(matrix_yield_max_vec[i,j,]>0, matrix_yield_max_vec[i,j,], 1)
			  food_energy[1] <- food_energy[12] <- food_energy[18] <- food_energy[21] <- food_energy[23] <- food_energy[27] <- food_energy[30] <- food_energy[33] <- food_energy[58] <- food_energy[16] <- 0                                   # set fodder crops to zero and COTTON

			  food_protein <- (matrix_area_vec[i,j,]*Protein_crops)  #/ifelse(matrix_yield_max_vec[i,j,]>0, matrix_yield_max_vec[i,j,], 1)
			  food_protein[1] <- food_protein[12] <- food_protein[18] <- food_protein[21] <- food_protein[23] <- food_protein[27] <- food_protein[30] <- food_protein[33] <- food_protein[58] <- food_protein[16] <- 0                           # set fodder crops to zero  and COTTON

sparse_matrix_93 <- simple_triplet_matrix(rep(1, 516), seq(1:516), c(rep(0,366), food_energy, rep(0, 61), rep(0,28)))   # Energy produced in FOOD
sparse_matrix_94 <- simple_triplet_matrix(rep(1, 516), seq(1:516), c(rep(0,366), food_protein, rep(0, 61), rep(0,28)))  # Protein produced in FOOD

			# 23th Lines summing up total crop (grain) energy and protein use by livestock
sparse_matrix_95 <- simple_triplet_matrix(rep(1, 516), seq(1:516), c(rep(0, 488), E_grain_dietary_req[i,j,], rep(0,19)))  # Energy used livestock
sparse_matrix_96 <- simple_triplet_matrix(rep(1, 516), seq(1:516), c(rep(0, 488), P_grain_dietary_req[i,j,], rep(0,19)))  # Protein used livestock (should be equal to the protein produced for feed)

			# 24th Line summing up total temporary fodder production in tons DM, Energy MJ and protein tons

sparse_matrix_97 <- simple_triplet_matrix(rep(1, 516), seq(1:516), c(matrix_area_vec[i,j,1],0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,12],0,0,0,0,0,matrix_area_vec[i,j,18],
																0,0,matrix_area_vec[i,j,21],0,matrix_area_vec[i,j,23],0,0,0,matrix_area_vec[i,j,27],0,0,matrix_area_vec[i,j,30],0,0,matrix_area_vec[i,j,33],0,0,0,
																0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,58],0,0,0,
																rep(0, 427), rep(0,28))) # **** Should give the same value when calculating using Sub_yield_feed ****

sparse_matrix_98 <- simple_triplet_matrix(rep(1, 516), seq(1:516), c(matrix_area_vec[i,j,1]*Energy_crops[1],0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,12]*Energy_crops[12],0,0,0,0,0,matrix_area_vec[i,j,18]*Energy_crops[18],
																0,0,matrix_area_vec[i,j,21]*Energy_crops[21],0,matrix_area_vec[i,j,23]*Energy_crops[23],0,0,0,matrix_area_vec[i,j,27]*Energy_crops[27],0,0,matrix_area_vec[i,j,30]*Energy_crops[30],0,0,matrix_area_vec[i,j,33]*Energy_crops[33],0,0,0,
																0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,58]*Energy_crops[58],0,matrix_area_vec[i,j,60],0,
																rep(0, 427), rep(0,28))) # **** Should give the same value when calculating using Sub_yield_feed ****

sparse_matrix_99 <- simple_triplet_matrix(rep(1, 516), seq(1:516), c(matrix_area_vec[i,j,1]*Protein_crops[1],0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,12]*Protein_crops[12],0,0,0,0,0,matrix_area_vec[i,j,18]*Protein_crops[18],
																0,0,matrix_area_vec[i,j,21]*Protein_crops[21],0,matrix_area_vec[i,j,23]*Protein_crops[23],0,0,0,matrix_area_vec[i,j,27]*Protein_crops[27],0,0,matrix_area_vec[i,j,30]*Protein_crops[30],0,0,matrix_area_vec[i,j,33]*Protein_crops[33],0,0,0,
																0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,matrix_area_vec[i,j,58]*Protein_crops[58],0,matrix_area_vec[i,j,60],0,
																rep(0, 427), rep(0,28))) # **** Should give the same value when calculating using Sub_yield_feed ****



			# 25th Lines summing up total temporary fodder energy and Protein consumption by livestock in MJ and tons

sparse_matrix_100 <- simple_triplet_matrix(rep(1,516), seq(1:516), c(rep(0,488), E_grass_dietary_req[i,j,], rep(0,19)))  # Energy
sparse_matrix_101 <- simple_triplet_matrix(rep(1,516), seq(1:516), c(rep(0,488), P_grass_dietary_req[i,j,], rep(0,19)))  # Protein

			# 24th Lines of zero before calculating the difference between produce temporary fodders and total consumed fodders (in ENERGY), and the ratio between the difference and the permanent pasture energy

sparse_matrix_102 <- simple_triplet_matrix(rep(1,516), seq(1:516), c(rep(0,488), rep(0,28)))
sparse_matrix_103 <- simple_triplet_matrix(rep(1,516), seq(1:516), c(rep(0,488), rep(0,28)))

			# 25th second Right side of the matrix defining statistical variable (exit calculations)
sparse_matrix_104 <- simple_triplet_matrix(c(rep(478,11), rep(479,11), rep(480,11), rep(481,11), rep(482,11), rep(483,11), rep(484,11), rep(485,11),
											rep(486,11), rep(487,11), rep(488,11)), rep(seq(1:11),11),
																		c(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                       # total_food_crops_energy_production
																		  0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0,                       # total_food_crops_protein_production
																		  0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0,                       # total_grain_energy_used_livestock
																		  0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0,                       # total_grain_protein_used_livestock
																		  0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0,                       # total_temporary_fodder_[tDM]
																		  0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0,                       # total_energy_temporary_fodder_[MJ]
																		  0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0,                       # total_protein_temporary_fodder_[tons]
																		  0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0,                       # total_fodder_energy_used_livestock
																		  0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0,                       # total_fodder_protein_used_livestock
																		  0, 0, 0, 0, 0, 1, 0, -1, 0, -1, 0,                      # difference produced_temporary_fodders - consumed in MJ
																		  0, 0, 0, 0, 0, 0, 0, 0, 0, ifelse(permanent_pastures_available_biomass_t_DM[i,j]>0, 1/(permanent_pastures_available_biomass_t_DM[i,j]*permanent_pastures_energy_protein_densities[,,1]), 0), -1),       # percentual surplus consumed fodder used by permanent pastures. TO BE USED ONLY IF VALUE OF VARIABLE IS NEGATIVE!!!!
																		nrow=488, ncol=11)


sparse_matrix_105 <- simple_triplet_matrix(rep(1,527), seq(1:527), c(rep(0,183), rep(-1, 61), rep(0,61), rep(1, 61),  rep(0,161)))

    ## Built up full matrix binding all submatrix parts created until now
	crop_yield_curve_sparse_matrix <- cbind(sparse_matrix_1, sparse_matrix_2, sparse_matrix_3, sparse_matrix_4, sparse_matrix_5, sparse_matrix_6, sparse_matrix_7, sparse_matrix_8)
	crop_yield_plateau_sparse_matrix <- cbind(sparse_matrix_9,sparse_matrix_10, sparse_matrix_11, sparse_matrix_12, sparse_matrix_13, sparse_matrix_14, sparse_matrix_15, sparse_matrix_16)
	crop_stovers_yield_curve_sparse_matrix <- cbind(sparse_matrix_17,sparse_matrix_18, sparse_matrix_19, sparse_matrix_20, sparse_matrix_21, sparse_matrix_22, sparse_matrix_23, sparse_matrix_24)
	crop_nitrogen_fixation_sparse_matrix <- cbind(sparse_matrix_25, sparse_matrix_26, sparse_matrix_27, sparse_matrix_28, sparse_matrix_29, sparse_matrix_30, sparse_matrix_31, sparse_matrix_32)
	crop_nitrogen_budgets_sparse_matrix <- cbind(sparse_matrix_33, sparse_matrix_34, sparse_matrix_35, sparse_matrix_36, sparse_matrix_37, sparse_matrix_38, sparse_matrix_39, sparse_matrix_40)
	N_total_available_for_crops_sparse_matrix <- cbind(sparse_matrix_41, sparse_matrix_42, sparse_matrix_43, sparse_matrix_44, sparse_matrix_45, sparse_matrix_46, sparse_matrix_47, sparse_matrix_48)
	crop_sub_food_yield <- cbind(sparse_matrix_49, sparse_matrix_50, sparse_matrix_51, sparse_matrix_52, sparse_matrix_53, sparse_matrix_54, sparse_matrix_55, sparse_matrix_56)
	crop_sub_feed_yield <- cbind(sparse_matrix_57, sparse_matrix_58, sparse_matrix_59, sparse_matrix_60, sparse_matrix_61, sparse_matrix_62, sparse_matrix_63, sparse_matrix_64)
	grain_energy_sparse_m <- cbind(sparse_matrix_65,sparse_matrix_66)
	fodder_energy_sparse_m <- cbind(sparse_matrix_67, sparse_matrix_68)
	stovers_energy_sparse_m <- cbind(sparse_matrix_69, sparse_matrix_70)
	grain_protein_sparse_m <- cbind(sparse_matrix_71, sparse_matrix_72)
	fodder_protein_sparse_m <- cbind(sparse_matrix_73, sparse_matrix_74)
	stovers_protein_sparse_m <- cbind(sparse_matrix_75, sparse_matrix_76)
	livestock_manure_sparse_m <- sparse_matrix_77
	livestock_products_energy_sparse_m <- sparse_matrix_78
	livestock_TLU_sparse_m<- sparse_matrix_79
	livestock_minimum_animals_number_low_boudary <- sparse_matrix_80
	livestock_minimum_animals_number_upper_boudary <- sparse_matrix_81
	contraint_min_yield_crop_categories_m <- rbind(sparse_matrix_82,sparse_matrix_83,sparse_matrix_84,sparse_matrix_85,sparse_matrix_86,sparse_matrix_87, sparse_matrix_88,sparse_matrix_89,sparse_matrix_90,sparse_matrix_91)


	final_sparse_matrix <- rbind(crop_yield_curve_sparse_matrix, crop_yield_plateau_sparse_matrix, crop_stovers_yield_curve_sparse_matrix, crop_nitrogen_fixation_sparse_matrix, crop_nitrogen_budgets_sparse_matrix,
								 N_total_available_for_crops_sparse_matrix, crop_sub_food_yield, crop_sub_feed_yield, grain_energy_sparse_m, fodder_energy_sparse_m, stovers_energy_sparse_m,
								 grain_protein_sparse_m, fodder_protein_sparse_m, stovers_protein_sparse_m, livestock_manure_sparse_m,livestock_products_energy_sparse_m, livestock_TLU_sparse_m,
								 livestock_minimum_animals_number_low_boudary, livestock_minimum_animals_number_upper_boudary, contraint_min_yield_crop_categories_m)
	final_sparse_matrix <- cbind(final_sparse_matrix, sparse_matrix_92)

	final_sparse_matrix_with_statistics <- rbind(final_sparse_matrix, sparse_matrix_93, sparse_matrix_94, sparse_matrix_95, sparse_matrix_96, sparse_matrix_97, sparse_matrix_98, sparse_matrix_99,
												sparse_matrix_100, sparse_matrix_101, sparse_matrix_102, sparse_matrix_103)
	final_sparse_matrix_with_statistics <- cbind(final_sparse_matrix_with_statistics, sparse_matrix_104)


	final_sparse_matrix_with_statistics <- rbind(final_sparse_matrix_with_statistics, sparse_matrix_105)

    ## Set directions of the contrains inequalities
	dir <- c(rep("==", 61),                                         # yield curve
			c("==", rep("<=", 10),"==", rep("<=", 10), "==", rep("<=", 6), "==", rep("<=", 27), "==", rep("<=", 3)),   # yield plateau --> Leguminous crops are forced to yield max (since they fix all their requirement of N by fixation). I did this to avoid the model assigning for any reason a lower yield I guess, but I dont remember exactlry wgy this was happening.
			rep("==", 183), c("<="), rep("==",61) , #  budgeting, total available fertiliser, SUB_food yield
			c("==", rep(">=", 10), "==", rep(">=", 5), "==", rep(">=", 2), "==", ">=", "==", rep(">=", 3), "==", rep(">=", 2), "==", rep(">=", 2), "==", rep(">=", 24), "==", rep(">=", 3)),  # share of maximum available SUB_feed yield. For FODDERS crops the Sub_feed yield is equal to the total yield!
			c(">=",">=",">=",">=",">=",">=","=="),                  # manure livestock
			rep("==", 9), rep("==", 9),                             # livestock numbers in head/TLU
			rep(">=", 5), rep("<=", 9),                             # livestock constraints
			rep(">=", 10),                                          # crop yield constraints
			rep("==", 11), ">")                                     # statistics

      ## Set right hand side values of the inequalities
		rhs <- c(rep(0,61),matrix_yield_max_vec[i,j,], rep(0,122),                                                   # yield curve, plateau, N stovers and N fix
			 (N_deposition[i,j,1]*matrix_area_vec[i,j,]+N_cyanobacteria*matrix_area_vec[i,j,]),                      # N budget
			 (+(N_conventional_manure[i,j]*K_conv_manure_surplus*global_production_share_coefficient_conv)+(N_conventional_waste_water[i,j]*K_waste_water)),  # N available for fertilisers
			 rep(0, 122), 0,-(permanent_pastures_available_biomass_t_DM[i,j] * permanent_pastures_energy_protein_densities[i,j,1] * global_production_share_coefficient_org),0,0,-(permanent_pastures_available_biomass_t_DM[i,j] * permanent_pastures_energy_protein_densities[i,j,2] * global_production_share_coefficient_org),0,0,    #*matrix_area_vec[i,j,]
			 rep(0,9),                                                                      # calculate real animal numbers
			 rep(0,9),                                                                      # calculate animal numbers TLU
			 rep(0,5),                                                                      # Animal numbers lower boundary
			 rep(0,9),                                                                      # Animal numbers upper boundary
			(sum(matrix_yield_max_vec[i,j,which(crop_info$CropGroup=="cereal")])*0.0),      # Primary cereals
			(sum(matrix_yield_max_vec[i,j,which(crop_info$CropGroup=="sec.cereal")])*0.0),  # Secondary cereals
			(sum(matrix_yield_max_vec[i,j,which(crop_info$CropGroup=="pulse")])*0.0),       # Pulses
			(sum(matrix_yield_max_vec[i,j,which(crop_info$CropGroup=="oilcrops")])*0.0),    # Oilcrops
			(sum(matrix_yield_max_vec[i,j,which(crop_info$CropGroup=="roots")])*0.0),       # Roots
			(sum(matrix_yield_max_vec[i,j,which(crop_info$CropGroup=="industrial")])*0.0),  # Industrial
			(sum(matrix_yield_max_vec[i,j,which(crop_info$CropGroup=="fodder")])*0.0),      # Fodder
			(sum(matrix_yield_max_vec[i,j,which(crop_info$CropGroup=="vegetable")])*0.0),   # Vegetable
			(sum(matrix_yield_max_vec[i,j,c(2, 3, 9, 13, 19, 20, 28, 36, 39)])*0.0),        # Fruits
			(sum(matrix_yield_max_vec[i,j,c(15, 16, 44, 53, 54)])*0.05),                    # NON - FOOD
			 rep(0,11), 0)																			# statistics

      ## Build ROI package contraint
        contst1 <- L_constraint(final_sparse_matrix_with_statistics, dir, rhs)

	  ## set ROI objective function
			  Energy_crops_obj_fod[c(1, 12, 18, 21, 23, 27, 30, 33, 58, 44, 53, 54, 16)] <- Energy_crops_obj[c(1, 12, 18, 21, 23, 27, 30, 33, 58, 44, 53, 54, 16)]  <- 0.00000010          # set fodder crops AND non food crops to a insignifican energy production
              Energy_crops_obj_fod[c(2:11,13:15,17,19:20,22,24:26,28:29,31:32,34:43,45:52,55:61)] <- Energy_crops_obj[c(1, 12, 18, 21, 23, 27, 30, 33, 58)] <- 0                           # set non-fodder crops to zero (for feed yield) AND fodder crops (for food yield)
			  obj <- c(rep(0,366), matrix_area_vec[i,j,] * Energy_crops_obj, matrix_area_vec[i,j,] * Energy_crops_obj_fod,  rep(0,9), 0,                                         # Energy_food_crops * Area * SUByield_food + Energy_fooder_Crops * Area * SUBYied_feed
																					 E_livestock_products[1]* matrix_livestock_production[i,j,1] * 1.0315,                      # cow milk in Liters * milk_density
																					 E_livestock_products[2]* matrix_livestock_production[i,j,2],                               # cow meat
																					 E_livestock_products[5]* matrix_livestock_production[i,j,3] * 1.09,                        # goat milk in Liters/year*milk_density
																					 E_livestock_products[6]* matrix_livestock_production[i,j,4],                               # goat meat
																					 E_livestock_products[3]* matrix_livestock_production[i,j,5] * 1.04,                        # sheep milk in Liters/year*milk_density
																					 E_livestock_products[4]* matrix_livestock_production[i,j,6],                               # sheep meat
																					 E_livestock_products[7]* matrix_livestock_production[i,j,7],                               # pigs meat
																					 E_livestock_products[8]* 2,                                                                # broilers meat   Kg_animal * number_animal_year
																					 E_livestock_products[9]* matrix_livestock_production[i,j,8],                                                       # heans eggs: num. eggs per layer times average 1 egg weight in Kg
																					 rep(0,9),                                                                          # Livestock in TLU
																					 rep(0,11))                                                                         # statistics

			  obj <- L_objective(obj, names = c(paste0("Y_", crop_info$CropName[1:61]),
												paste0("harv_N_", crop_info$CropName[1:61]),
												paste0("stov_N_", crop_info$CropName[1:61]),
												paste0("pool_N_", crop_info$CropName[1:61]),
												paste0("fix_N_", crop_info$CropName[1:61]),
												paste0("fert_N", crop_info$CropName[1:61]),
												paste0("Sub_food_Y", crop_info$CropName[1:61]),
												paste0("Sub_feed_Y", crop_info$CropName[1:61]),
												"No.dairy", "No.beef", "No.dairy_goats","No.meat_goats","No.dairy_sheep", "No.meat_sheep", "No.pigs", "No.broilers", "No.hens", "Ntotavailable",
												"No.milk", "No.meat", "No.milk_goats","No.meat_goats","No.milk_sheep", "No.meat_sheep", "No.meat_pigs", "No.meat_broilers", "No.eggs_hens",
												"TLU.dairy", "TLU.beef", "TLU.dairy_goats","TLU.meat_goats","TLU.dairy_sheep", "TLU.meat_sheep", "TLU.pigs", "TLU.broilers", "TLU.hens",
												"total_food/feed_crops_energy_production", "total_food/feed_crops_protein_production",
												"total_grain_energy_used_livestock", "total_grain_protein_used_livestock",
												"total_temporary_fodder_[tDM]", "total_energy_temporary_fodder_[MJ]", "total_protein_temporary_fodder_[tons]",
												"total_fodder_energy_used_livestock", "total_fodder_protein_used_livestock",
												"difference_produced_consumed_temp_fodder_MJ", "ratio_%_loss_manure_pasture_when_negative"))

			  ## Set continuos variable type (clp solver only accepts continuos variables)
			  types <- rep("C", 527)

			  ## Set  variables upper and lower bounds
			  #bounds <- V_bound(li=seq(1:527), lb=c(rep(0, 525), rep(-10e11,2)), ui=seq(1:527), ub=rep(10e11, 527))   # ATTENTION:: THE upper bounday might be to low for total energy production!!!
    		  bounds <- V_bound(li=seq(1:527), lb=c(rep(0, 525), rep(-10e13,2)), ui=seq(1:527), ub=c(rep(10e13, 498), c(rep(50000, 2), rep(400000, 4), 400000, rep(6000000, 2), rep(10e13, 20))))   # ATTENTION: THE upper bounday might be to low for total energy production!!!

			  ## Print vector position (optimised gridcell vector number)
			  print(j)

			  ## Built and solve optimisation problem using ROI package
			  prob <- OP(obj, contst1, types = types, maximum= TRUE, bounds=bounds)
			  solution <- ROI_solve(prob, solver = "clp", control=list(amount=1, iterations=5000, seconds=10))


				#  ----------- ALTERNATIVE SOLVING METHOD USING clpAPI package directly, taken from ROI.plugin.clp CODE on gitHub. The input data as to be tranformed in a suitable forl for clpAPI package
					# main_args <- list( obj = stats::terms(objective(prob))[["L"]],
										# mat = constraints(prob)$L,
										# dir = constraints(prob)$dir,
										# rhs = constraints(prob)$rhs,
										# bounds = bounds(prob),
										# max = prob$maximum )
					# obj_coef <- as.matrix( main_args$obj )[1, ]
			        # s_mat <- Matrix::sparseMatrix(i = main_args$mat$i, j = main_args$mat$j, x = main_args$mat$v)
					# ia <- s_mat@i
					# ja <-  s_mat@p
					# ar <- s_mat@x
					# ncols <- ncol(s_mat)
					# nrows <- nrow(s_mat)
					# rlb <- ifelse(main_args$dir %in% c("<="), -Inf, main_args$rhs)      # range of rows
					# rub <- ifelse(main_args$dir %in% c(">="), Inf, main_args$rhs)
					# lb <- NULL                                                          # range of coloumns
					# ub <- NULL
					# if(!is.null(main_args$bounds)){
					  # if(length(main_args$bounds$lower$ind) > 0){
						# lb <- rep(0, ncols)
						# lb[main_args$bounds$lower$ind] <- main_args$bounds$lower$val
					  # }
					  # if(length(main_args$bounds$upper$ind) > 0){
						# ub <- rep(Inf, ncols)
						# ub[main_args$bounds$upper$ind] <- main_args$bounds$upper$val
					  # }
					# }
					# lp <- clpAPI::initProbCLP()    # preparing the model

					# lpdir <- ifelse(main_args$max, -1, 1)   # minimize
					# clpAPI::setObjDirCLP(lp, lpdir)

					# clpAPI::loadProblemCLP(lp, ncols = ncols, nrows = nrows, ia, ja, ar,
								   # lb = lb, ub = ub, obj_coef = obj_coef,
								   # rlb = rlb, rub = rub)

					# clpAPI::setMaximumIterationsCLP(lp, 5000)  # set maximum number of solver iterations to 5000

					# clpAPI::solveInitialCLP(lp)  # solve lp problem

					# solution <- ROI_plugin_canonicalize_solution(solution = clpAPI::getColPrimCLP(lp),    ## ROI format
												 # optimum = clpAPI::getObjValCLP(lp),
												 # status = clpAPI::getSolStatusCLP(lp),
												 # solver = "clp")

					# names(solution$solution) <- c(paste0("Y_", crop_info$CropName[1:61]),
												# paste0("harv_N_", crop_info$CropName[1:61]),
												# paste0("stov_N_", crop_info$CropName[1:61]),
												# paste0("pool_N_", crop_info$CropName[1:61]),
												# paste0("fix_N_", crop_info$CropName[1:61]),
												# paste0("fert_N", crop_info$CropName[1:61]),
												# paste0("Sub_food_Y", crop_info$CropName[1:61]),
												# paste0("Sub_feed_Y", crop_info$CropName[1:61]),
												# "No.dairy", "No.beef", "No.dairy_goats","No.meat_goats","No.dairy_sheep", "No.meat_sheep", "No.pigs", "No.broilers", "No.hens", "Ntotavailable",
												# "No.milk", "No.meat", "No.milk_goats","No.meat_goats","No.milk_sheep", "No.meat_sheep", "No.meat_pigs", "No.meat_broilers", "No.eggs_hens",
												# "TLU.dairy", "TLU.beef", "TLU.dairy_goats","TLU.meat_goats","TLU.dairy_sheep", "TLU.meat_sheep", "TLU.pigs", "TLU.broilers", "TLU.hens",
												# "total_food/feed_crops_energy_production", "total_food/feed_crops_protein_production",
												# "total_grain_energy_used_livestock", "total_grain_protein_used_livestock",
												# "total_temporary_fodder_[tDM]", "total_energy_temporary_fodder_[MJ]", "total_protein_temporary_fodder_[tons]",
												# "total_fodder_energy_used_livestock", "total_fodder_protein_used_livestock",
												# "difference_produced_consumed_temp_fodder_MJ", "ratio_%_loss_manure_pasture_when_negative")



				counter[i,j] <- solution$status$code
				matrix_final_yield[i,j,] <- solution$solution[1:61]
                matrix_final_N_assigned_to_crop[i,j,] <- solution$solution[306:366]
				matrix_sub_food_yield[i,j,] <- solution$solution[367:427]
				matrix_sub_feed_yield[i,j,] <- solution$solution[428:488]
				number_of_livestock_heads[i,j,] <- solution$solution[499:507]
                number_of_livestock_standing_animals[i,j,] <- solution$solution[c(492, 494, 495, 496)]
				N_total_available[i,j,] <- solution$solution[498]
				statistics[i,j,] <- solution$solution[517:527]
                BNF[i,j,] <- solution$solution[(244+which(Fixed_N!=0))]
			}

else {			matrix_final_yield[i,j,] <- rep(NA, 61)
				matrix_final_N_assigned_to_crop[i,j,] <- rep(NA, 61)
				matrix_sub_food_yield[i,j,] <- rep(NA, 61)
				matrix_sub_feed_yield[i,j,] <- rep(NA, 61)
				number_of_livestock_heads[i,j,] <- rep(NA, 9)
                number_of_livestock_standing_animals[i,j,] <- rep(NA,4)
				N_total_available[i,j,] <- NA
				counter[i,j] <- NA
				statistics[i,j,] <- rep(NA,11)
                BNF[i,j,] <- rep(NA,length(which(Fixed_N!=0)))
				}
}



# -------------------------------------------------  FREE UP MEMORY DELETING VARIABLES   ------------------------------------------
library(pryr)
print(mem_used())
print(mem_change(rm(matrix_area_vec, matrix_yield_max_vec, Nyear_livestock_production_available, E_livestock_products, matrix_livestock_production, E_grain_dietary_req, E_grass_dietary_req, E_stovers_dietary_req, P_grain_dietary_req, P_grass_dietary_req, P_stovers_dietary_req)))
gc()

# -------------------------------------------------  4 DATA SAVING   ------------------------------------------



crs <- c("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

# Total crops yield
for (i in 1:61) {
	raster_1 <- raster(matrix(matrix_final_yield[,,i], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
	writeRaster(raster_1, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_yield_1_", crop_info$CropName[i], ".tif"), format="GTiff", overwrite=T)
}

rm(raster_1, matrix_final_yield)
gc()


# Total N from organic manure allocate to each crop
for (i in 1:61) {
    raster_2 <- raster(matrix(matrix_final_N_assigned_to_crop[,,i], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
    writeRaster(raster_2, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_MANURE_assigned_to_crop_1_", crop_info$CropName[i], ".tif"), format="GTiff", overwrite=T)
}

rm(raster_2, matrix_final_N_assigned_to_crop)
gc()

# Animal number of heads
raster_3 <- raster(matrix(number_of_livestock_heads[,,1], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_3, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_cows"), format="GTiff", overwrite=T)
raster_4 <- raster(matrix(number_of_livestock_heads[,,2], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_4, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_beef"), format="GTiff", overwrite=T)
raster_5 <- raster(matrix(number_of_livestock_heads[,,3], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_5, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_dairy_goats"), format="GTiff", overwrite=T)
raster_6 <- raster(matrix(number_of_livestock_heads[,,4], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_6, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_meat_goats"), format="GTiff", overwrite=T)
raster_7 <- raster(matrix(number_of_livestock_heads[,,5], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_7, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_dairy_sheep"), format="GTiff", overwrite=T)
raster_8 <- raster(matrix(number_of_livestock_heads[,,6], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_8, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_meat_sheep"), format="GTiff", overwrite=T)
raster_9 <- raster(matrix(number_of_livestock_heads[,,7], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_9, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_pigs"), format="GTiff", overwrite=T)
raster_10 <- raster(matrix(number_of_livestock_heads[,,8], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_10, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_broilers"), format="GTiff", overwrite=T)
raster_11 <- raster(matrix(number_of_livestock_heads[,,9], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_11, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_hens"), format="GTiff", overwrite=T)


rm(raster_3, raster_4, raster_5, raster_6, raster_7, raster_8, raster_9, raster_10, raster_11, number_of_livestock_heads)
gc()

# Total available Nitrogen in organic manure
raster_12 <- raster(matrix(N_total_available[,,1], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_12, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_tot_available"), format="GTiff", overwrite=T)

rm(raster_12, N_total_available)

# Error counter
raster_13 <- raster(matrix(counter[,], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_13, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_optimisation_counter"), format="GTiff", overwrite=T)

rm(raster_13, counter)
gc()

# Model statistics
raster_14 <- raster(matrix(statistics[,,1], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_14, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_total_food_feed_crops_energy_production"), format="GTiff", overwrite=T)
raster_15 <- raster(matrix(statistics[,,2], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_15, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_total_food_feed_crops_protein_production"), format="GTiff", overwrite=T)
raster_16 <- raster(matrix(statistics[,,3], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_16, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_total_grain_energy_used_livestock"), format="GTiff", overwrite=T)
raster_17 <- raster(matrix(statistics[,,4], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_17, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_total_grain_protein_used_livestock"), format="GTiff", overwrite=T)
raster_18 <- raster(matrix(statistics[,,5], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_18, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_total_temporary_fodder_[tDM]"), format="GTiff", overwrite=T)
raster_19 <- raster(matrix(statistics[,,6], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_19, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_total_energy_temporary_fodder_[MJ]"), format="GTiff", overwrite=T)
raster_20 <- raster(matrix(statistics[,,7], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_20, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_total_protein_temporary_fodder_[tons]"), format="GTiff", overwrite=T)
raster_21 <- raster(matrix(statistics[,,8], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_21, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_total_fodder_energy_used_livestock"), format="GTiff", overwrite=T)
raster_22 <- raster(matrix(statistics[,,9], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_22, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_total_fodder_protein_used_livestock"), format="GTiff", overwrite=T)
raster_23 <- raster(matrix(statistics[,,10], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_23, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_difference_produced_consumed_temp_fodder_MJ"), format="GTiff", overwrite=T)
raster_24 <- raster(matrix(statistics[,,11], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_24, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_ratio_%_loss_manure_pasture_when_negative"), format="GTiff", overwrite=T)

rm(raster_14, raster_15, raster_16, raster_17, raster_18, raster_19, raster_20, raster_21, raster_22, raster_23, raster_24, statistics)
gc()

# Crops SUB_Food yield
for (i in 1:61) {
	raster_25 <- raster(matrix(matrix_sub_food_yield[,,i], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
	writeRaster(raster_25, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_SUByield_food", crop_info$CropName[i], ".tif"), format="GTiff", overwrite=T)
}

rm(raster_25, matrix_sub_food_yield)
gc()

# Crops SUB_Feed yield
for (i in 1:61) {
	raster_26 <- raster(matrix(matrix_sub_feed_yield[,,i], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
	writeRaster(raster_26, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_SUByield_feed", crop_info$CropName[i], ".tif"), format="GTiff", overwrite=T)
}

rm(raster_26, matrix_sub_feed_yield)
gc()

# Standing animals
raster_27 <- raster(matrix(number_of_livestock_standing_animals[,,1], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_27, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_standing_meat_goats"), format="GTiff", overwrite=T)
raster_28 <- raster(matrix(number_of_livestock_standing_animals[,,2], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_28, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_standing_meat_sheep"), format="GTiff", overwrite=T)
raster_29 <- raster(matrix(number_of_livestock_standing_animals[,,3], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_29, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_standing_pigs"), format="GTiff", overwrite=T)
raster_30 <- raster(matrix(number_of_livestock_standing_animals[,,4], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
writeRaster(raster_30, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_N_standing_broilers"), format="GTiff", overwrite=T)

rm(raster_27, raster_28, raster_29, raster_30, number_of_livestock_standing_animals)
gc()


# BNF
for (i in 1:length(which(Fixed_N!=0))) {
    raster_28 <- raster(matrix(BNF[,,i], nrow=2160, byrow=TRUE), xmn=-180, xmx=180, ymn=-90, ymx=90, crs = crs)
    writeRaster(raster_28, filename = paste0("Output_data/Scenario_S",scenario_S_no,"/Script_", script_no, "/script_", script_no, "_BNF_", crop_info$CropName[which(crop_info$CropClass=="N-fixing")[i]], ".tif"), format="GTiff", overwrite=T)
}

rm(raster_28, BNF)
gc()
