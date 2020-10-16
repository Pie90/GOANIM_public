# ----------- Load R Packages ----------------------------------
library(tools)
library(raster)
library(rgdal)
library(ncdf4)
library(maptools)
## Parallel computing core packages
library(doParallel)
library(foreach)

# ----------- Set Working Directory and Temporary files directory in scratch--------------------

setwd("/home/pbarbieri/global/Version_I_first_inclusion_of_land_use_change_rules/")
getwd()
library(unixtools)
set.tempdir("/scratch/pbarbieri/R_temporary_files")
tempdir()

# *********************************   ATT! CORE CONTROLS, activate parrallelisation   **************************************
detectCores()   		        # Detect number of cores
cluster<-makeCluster(23)   		# Create cores cluster
registerDoParallel(cluster) 	# start cluster
getDoParWorkers()               # check number of cores used
#registerDoSEQ()                # Stop cluster

# ------------- Load manual functions ------------------------------------
	sum.na <- function(x,...){
	  if (sum(is.na(x))==length(x)) {    # given N cells to be sum, if they are all NA, then the result cell is NA
		NA
		} else {
		  sum(x, na.rm=TRUE)             # ... otherwise NA values are simply skipped (set to zero)
	  }
	}

	multiply_function <- function(var, multiplier) {
		res <- var * multiplier
		return(res)
	}



# -----Load global(program) coefficients, info files --------------------------------
		##     1. Coefficients
		global_production_share_coefficient_conv <- 1
		global_production_share_coefficient_org <- 1 - global_production_share_coefficient_conv
		conv_overfertil_allowed <- 1.05
		crop_residues_recycling_factor_conventional <- raster("Coefficients/map_coeff_residues_recycle_conventional.tif")
		crop_residues_recycling_factor_organic <- raster("Coefficients/map_coeff_residues_recycle_organic.tif")  # ORG_recycling = 1.428571 CONV_recycling. CONV recycling form SMIL et al. 1999

			### 1.1 BInary coefficients to activate/disactivate variables
			 K_conv_manure_surplus <- 1
			 K_waste_water <- 1

		##     2. Datasets
		crop_info <- read.csv("crop_list_60.csv", header=T)
		map_bruno <- raster("/home/pbarbieri/global/World shapefiles/Country codes/country_UNI_spatial_corrected_Pietro.nc")
		mask_continents <- map_bruno!=0  # create mask to add continentis shapes
		mask_continents <- reclassify(reclassify(mask_continents, cbind(0,NA)), cbind(1, 0))


		##     3. Initialise statistics/output

		Output_statistics_dataframe <- data.frame("variable_Num"=c(1:7))  # according to number of row needed, leaving colomn created as row number index
			row.names(Output_statistics_dataframe) <- c("total_N_fertiliser", "fertiliser_corrected_leaching", "fertilisers_correted all","%_N_leaching","%_N_losses_total",
														"N_fix_conv", "N_fix_conv_losses")
		Output_statistics_dataframe <- data.frame("1" = numeric())



# ******************************************************************************************************************
# *********************************  PART I: CONVENTIONAL FARMING SUB-MODEL ****************************************
# ******************************************************************************************************************


# ___________________________________________________________________________________
# ______________________  PART A: CROP AND RESIDUES DEMAND __________________________
# ___________________________________________________________________________________


	## -----------  Load conventional areas in ha i.e. Monfreda areas maps  --------------------------------

	dir <- c("EarthStat data/Conventional_crop_production/60 crops/Crop_areas/")
	files <- sort(list.files(dir, pattern = ".tif"))
	conventional_rasters_area <- parLapply(cluster, paste0(dir, files), raster)
	  foreach (i = 1:length(crop_info$CropName)) %do% {  # rename layers
		names(conventional_rasters_area)[i] <- paste(crop_info$CropName[i], "_conventional_area", sep = "")
	  }
	conventional_rasters_area <- stack(conventional_rasters_area)                          # Create raster-stack




	## -----------  Load conventional production (in tons per gridcell) maps i.e. Monfreda maps  --------------------------------

	dir <- c("EarthStat data/Conventional_crop_production/61 crops/")                      # ATTENTIONE ! QUI CARICO la PRODUZIONE, non il land use SHARE *************
	files <- sort(list.files(dir, pattern = ".tif"))
	conventional_rasters_production <- parLapply(cluster, paste0(dir, files), raster)
	  foreach (i = 1:length(crop_info$CropName)) %do% {  # rename layers
		names(conventional_rasters_production)[i] <- paste(crop_info$CropName[i], "_conventional_production", sep = "")
	  }
	conventional_rasters_production <- stack(conventional_rasters_production)              # Creaset raster-stack


	## ------------ Calculate production in DRY MATTER ------------------------------------
	crop_DM <- crop_info$dry_matter                                                 # Create a vector containing the DM coefficinets


	foreach (i = 1:61, .packages='raster') %dopar% {
		multipl <- conventional_rasters_production[[i]]*crop_DM[i]                  # Calculate production in DM
		writeRaster(multipl, filename = paste0("Script_generated_files/rasters_conventional_production_DM/", crop_info$CropName[i], "_conventional_DM"), format="GTiff", overwrite=T )
		}
	dir_conv_DM <- c("Script_generated_files/rasters_conventional_production_DM/")
	files_conv_DM <- sort(list.files(path = dir_conv_DM, pattern = ".tif"))
	conventional_rasters_production_DM <- parLapply(cluster, paste0(dir_conv_DM, files_conv_DM), raster)
		  foreach (i = 1:length(crop_info$CropName)) %do% {                         # rename layers
			names(conventional_rasters_production_DM)[i] <- paste(crop_info$CropName[i], "_conventional_production_DM", sep = "")
		  }
	conventional_rasters_production_DM <- stack(conventional_rasters_production_DM) # Creaset raster-stack


	##  ---------------------- Calculate total Crop Residues Production in DM ------------------------------------
	foreach (i = 1:61, .packages='raster') %dopar% {
		multipl <- conventional_rasters_production_DM[[i]] * crop_info$calculated_RPR[i]
		writeRaster(multipl, filename = paste0("Script_generated_files/rasters_conventional_production_DM_crop_residues/", crop_info$CropName[i], "_conventional_DM_crop_residues"), format="GTiff", overwrite=T )
		}

	dir_conv_residues <- c("Script_generated_files/rasters_conventional_production_DM_crop_residues/")
	files_conv_residues <- sort(list.files(path = dir_conv_residues, pattern = ".tif"))
	crop_residues_conventional <- parLapply(cluster, paste0(dir_conv_residues, files_conv_residues), raster)
			  foreach (i = 1:length(crop_info$CropName)) %do% {                     # rename layers
				names(crop_residues_conventional)[i] <- paste(crop_info$CropName[i], "_conventional_crop_residues_DM", sep = "")
			  }
	crop_residues_conventional <- stack(crop_residues_conventional)                 # Creaset raster-stack


	### --------------- N demand ----------------------------------------------------------
		foreach (i = 1:61, .packages='raster') %dopar% {                             # multipling crop production in DM with crop products N content to estimate harvested biomass N
			multipl <- conventional_rasters_production_DM[[i]]*crop_info$N[i]
			writeRaster(multipl, filename = paste0("Script_generated_files/conventional_crop_N/", crop_info$CropName[i], "_conventional_crop_N"), format="GTiff", overwrite=T )
			}
		dir_conv_N <- c("Script_generated_files/conventional_crop_N/")
		files_conv_N <- sort(list.files(path = dir_conv_N, pattern = ".tif"))
		conventional_rasters_N_demand <- parLapply(cluster, paste0(dir_conv_N, files_conv_N), raster)
			  foreach (i = 1:length(crop_info$CropName)) %do% {                      # rename layers
				names(conventional_rasters_N_demand)[i] <- paste(crop_info$CropName[i], "_conventional_N_demand_harvest", sep = "")
			  }
		conventional_rasters_N_demand <- stack(conventional_rasters_N_demand)        # Creaset raster-stack

		total_conventional_crop_N_demand <- clusterR(conventional_rasters_N_demand, calc, args=list(fun=sum.na), filename=paste0("Script_generated_files/","total_conventional_crop_N_demand.tif"), format="GTiff", overwrite=T, cl=cluster)
			total_conventional_crop_N_demand <- raster("Script_generated_files/total_conventional_crop_N_demand.tif") # calculate and load Total N demand harvested biomass of all 61 crops


			foreach (i = 1:61, .packages='raster') %dopar% {                         # multipling crop residues production in DM with crop residues N content to estimate crop residues N
				multipl <- crop_residues_conventional[[i]]*crop_info$N_crop_residues[i]
				writeRaster(multipl, filename = paste0("Script_generated_files/conventional_N_crop_residues/", crop_info$CropName[i], "_conventional_N_crop_residues"), format="GTiff", overwrite=T )
				}
			dir_conv_N_residues <- c("Script_generated_files/conventional_N_crop_residues/")
			files_conv_N_residues <- sort(list.files(path = dir_conv_N_residues, pattern = ".tif"))
			conventional_rasters_N_demand_residues <- parLapply(cluster, paste0(dir_conv_N_residues, files_conv_N_residues), raster)
				  foreach (i = 1:length(crop_info$CropName)) %do% {                 # rename layers
						names(conventional_rasters_N_demand_residues)[i] <- paste(crop_info$CropName[i], "_conventional_crop_residues_N_demand", sep = "")
					}
			conventional_rasters_N_demand_residues <- stack(conventional_rasters_N_demand_residues) # Creaset raster-stack

			total_conventional_crop_N_demand_crop_residues <- clusterR(conventional_rasters_N_demand_residues, calc, args=list(fun=sum.na), filename=paste0("Script_generated_files/","total_conventional_N_demand_crop_residues.tif"), format="GTiff", overwrite=T, cl=cluster)
				total_conventional_crop_N_demand_crop_residues <- raster("Script_generated_files/total_conventional_N_demand_crop_residues.tif") # calculate and load Total N crop residues demand of all 61 crops

	### P demand ----------------------------------------------------------
#		foreach (i = 1:61, .packages='raster') %dopar% {   # multipling crop production in DM with crop products P content to estimate main product P
#			multipl <- conventional_rasters_production_DM[[i]]*crop_info$P[i]
#			writeRaster(multipl, filename = paste0("Script_generated_files/conventional_crop_P/", crop_info$CropName[i], "_conventional_crop_P"), format="GTiff", overwrite=T )
#			}
#		dir_conv_P <- c("Script_generated_files/conventional_crop_P/")
#		files_conv_P <- sort(list.files(path = dir_conv_P, pattern = ".tif"))
#		conventional_rasters_P_demand <- parLapply(cluster, paste0(dir_conv_P, files_conv_P), raster)
#			  foreach (i = 1:length(crop_info$CropName)) %do% {  # rename layers
#				names(conventional_rasters_P_demand)[i] <- paste(crop_info$CropName[i], "_conventional_P_demand_harvest", sep = "")
#			  }
#		conventional_rasters_P_demand <- stack(conventional_rasters_P_demand) # Creaset raster-stack

#		total_conventional_crop_P_demand <- clusterR(conventional_rasters_P_demand, calc, args=list(fun=sum.na), filename=paste0("Script_generated_files/","total_conventional_crop_P_demand.tif"), format="GTiff", overwrite=T, cl=cluster)
#			total_conventional_crop_P_demand <- raster("Script_generated_files/total_conventional_crop_P_demand.tif") # calculate and load Total P demand harvested biomass of all 61 crops


#			foreach (i = 1:61, .packages='raster') %dopar% {      # multipling crop residues production in DM with crop residues P content to estimate crop residues P
#				multipl <- crop_residues_conventional[[i]]*crop_info$P_crop_residues[i]
#				writeRaster(multipl, filename = paste0("Script_generated_files/conventional_P_crop_residues/", crop_info$CropName[i], "_conventional_P_crop_residues"), format="GTiff", overwrite=T )
#				}
#			dir_conv_P_residues <- c("Script_generated_files/conventional_P_crop_residues/")
#			files_conv_P_residues <- sort(list.files(path = dir_conv_P_residues, pattern = ".tif"))
#			conventional_rasters_P_demand_residues <- stack(parLapply(cluster, paste0(dir_conv_P_residues, files_conv_P_residues), raster))
#				  foreach (i = 1:length(crop_info$CropName)) %do% {  # rename layers
#						names(conventional_rasters_P_demand_residues)[i] <- paste(crop_info$CropName[i], "_conventional_crop_residues_P_demand", sep = "")
#					}
#			conventional_rasters_P_demand_residues <- stack(conventional_rasters_P_demand_residues) # Creaset raster-stack

#				total_conventional_crop_P_demand_crop_residues <- clusterR(conventional_rasters_P_demand_residues, calc, args=list(fun=sum.na), filename=paste0("Script_generated_files/","total_conventional_P_demand_crop_residues.tif"), format="GTiff", overwrite=T, cl=cluster)
#					total_conventional_crop_P_demand_crop_residues <- raster("Script_generated_files/total_conventional_P_demand_crop_residues.tif") # calculate and load Total P crop residues demand of all 61 crops


					N_harvested_crop <- (data.frame(cellStats(total_conventional_crop_N_demand, sum)))                  #calculate statistics, total crops harvested demand
					N_crop_residues <- (cellStats(total_conventional_crop_N_demand_crop_residues, sum))                 #total crops residues demand
					N_harvested_crop <- rbind(N_harvested_crop, N_crop_residues)
					row.names(N_harvested_crop) <- c("total_N_deman_crop_harvested", "total_N_deman_crop_residues")     #set rows names
					names(N_harvested_crop) <- c(paste0("simulation.","1"))                                             # for SENSITIVITY ANALYSIS change "1" with sumulation number (loop)
				Output_statistics_dataframe <- rbind(Output_statistics_dataframe, N_harvested_crop)                     #add rows main database




# _________________________________________________________________________________________________
# ______________________  PART B: N and P SUPPLY BY CONVENTIONAL SOURCES __________________________
# _________________________________________________________________________________________________

	## --------- Syntetic Fertilizers ----------------------
	dir_N_fertilizers <- c("Inorganic_fertilisers/Inorganic N tons per gridcell/")
	   files_N_fertilizers <- sort(list.files(path = dir_N_fertilizers, pattern = ".tif"))
	   rasters_N_fertilizers <- stack(parLapply(cluster, paste0(dir_N_fertilizers, files_N_fertilizers), raster))

#	dir_P_fertilizers <- c("Inorganic_fertilisers/Inorganic P tons per gridcell/")
#	   files_P_fertilizers <- sort(list.files(path = dir_P_fertilizers, pattern = ".tif"))
#	   rasters_P_fertilizers <- stack(parLapply(cluster, paste0(dir_P_fertilizers, files_P_fertilizers), raster)


	total_N_fert <- clusterR(rasters_N_fertilizers, calc, args=list(fun=sum.na),                            # Total Tons N/gridcell for the 61 crop species
								filename=paste0("Script_generated_files/", "total_N_fert.tif"), format="GTiff", overwrite=T, cl=cluster)
				total_N_fert <- raster("Script_generated_files/total_N_fert.tif")

#	total_P_fert <- clusterR(rasters_P_fertilizers, calc, args=list(fun=sum.na),                            # Total Tons P/gridcell for the 61 crop species
#								filename=paste0("Script_generated_files/", "total_P_fert.tif"), format="GTiff", overwrite=T, cl=cluster)
#				total_P_fert <- raster("Script_generated_files/total_P_fert.tif")

		### ---- Estimating Nitrogen fertilisers available after losses NH3 and NOX  ------------------------------------

		total_N_fert_corrected <- total_N_fert * (1- (0.01 + 0.1))                                          # coefficient losses direc: 0.01 --> coefficient range: 0.003-0.03
                                                                                                            # According to IPCC, total losses coefficient for both NH3 and NO (indirect emissions): 0.10 --> coefficient range 0.03 - 0.3

		leaching_areas_mask <- raster("Climatic and irrigation data/mask_area_leaching_final_irrigation_above_005.tif") # Load leaching areas. The areas where leaching occurs were estimated according to the IPCC procedure. See "Estimation_areas_of_N_leaching.R" in /Pietro/R Analysis/ Global convetion.../ Estimation areas of.../R_code
		mask_correction_leaching <- leaching_areas_mask * 0.7                                                           # 0.3: correction to estimate the 30 % losses due to leaching
		mask_correction_leaching <- reclassify(mask_correction_leaching, cbind(0,1))

        total_N_fert_corrected <- total_N_fert_corrected - (total_N_fert * (1-mask_correction_leaching))                # correct for nitrogen leaching according to mask
        total_N_fert_corrected <- merge(total_N_fert_corrected, mask_continents)                                        # add continents shape

		writeRaster(total_N_fert_corrected, filename = paste0("Script_generated_files/", "total_N_fert_corrected"), format="GTiff", overwrite=T )
			total_N_fert_corrected <- raster("Script_generated_files/total_N_fert_corrected.tif")

					N_lost_leaching <- 1-((cellStats((total_N_fert * mask_correction_leaching), sum)) / (cellStats(total_N_fert, sum)))                               #calculate statistics
					N_lost_total <- 1-((cellStats(total_N_fert_corrected, sum)) / (cellStats(total_N_fert, sum)))
					N_fert_stats <- data.frame(cellStats(stack(total_N_fert, (total_N_fert*mask_correction_leaching), total_N_fert_corrected), sum))                  #build temporary data.frame and add rows
					N_fert_stats <- rbind(N_fert_stats, N_lost_leaching)
					N_fert_stats <- rbind(N_fert_stats, N_lost_total)
					row.names(N_fert_stats) <- c("total_N_fertiliser", "fertiliser_corrected_leaching", "fertilisers_correted all","%_N_leaching","%_N_losses_total") #set rows names
					names(N_fert_stats) <- c(paste0("simulation.","1"))                                                                                               # for SENSITIVITY ANALYSIS change "1" with sumulation number (loop)
				Output_statistics_dataframe <- rbind(Output_statistics_dataframe, N_fert_stats)                                                                       #add rows main database

	## --- Nitrogen fixation Conventional -----------------------


	fixation_coefficients <- crop_info$NiC*crop_info$X1.NHI*crop_info$Ndfa*crop_info$Proot                              # Coefficient for fixing model applied, Proot: not to be used, since roots are automatically staying in the soil at steady-state
                                                                                                                        # The previous statement is wrong. PRoot has to be included as it is N coming frol the atmosphere anyway.

	foreach (i = 1:nlayers(conventional_rasters_production_DM), .packages='raster') %dopar% {
		multipl <- conventional_rasters_production_DM[[i]]*fixation_coefficients[i]
		writeRaster(multipl, filename = paste0("Script_generated_files/N_fixation/conventional/", crop_info$CropName[i]), format="GTiff", overwrite=T )
		}
				dir_fix_conv <- c("Script_generated_files/N_fixation/conventional/")
				files_fix_conv <- sort(list.files(path = dir_fix_conv, pattern = ".tif"))
				rasters_fixation_conventional <- stack(parLapply(cluster, paste0(dir_fix_conv, files_fix_conv), raster))

	total_fixation_conventional <- clusterR(rasters_fixation_conventional, calc, args=list(fun=sum.na),                 # Tons N/gridcell
								 filename=paste0("Script_generated_files/", "total_rasters_fixation_conventional.tif"), format="GTiff", overwrite=T, cl=cluster)
				total_fixation_conventional <- raster("Script_generated_files/total_rasters_fixation_conventional.tif")

	   # coefficient losses: 0.01 --> coefficient range: 0.003-0.03 --> BNF DIRECT NO2 Emissions have been deleted from IPCC because no evidence of emission was found
	   #total_fixation_conventional_corrected_losses <- total_fixation_conventional_corrected_losses * mask_correction_leaching                             # 0.3: correction to estimate the 30% losses due to leaching     # Losses due to leaching from NBF are already estimated when accounteng losses in crop residues!!

				N_fix_conv <- data.frame(cellStats(total_fixation_conventional, sum))  																	    #build temporary data.frame and add rows
				#N_fix_conv_losses <- 1-(cellStats(total_fixation_conventional_corrected_losses, sum)/(cellStats(total_fixation_conventional, sum)))
				#N_fix_conv <- rbind(N_fix_conv, N_fix_conv_losses)
				row.names(N_fix_conv) <- c("N_fix_conv") 																									# set rows names
				names(N_fix_conv) <- c(paste0("simulation.","1"))																							# for SENSITIVITY ANALYSIS change "1" with sumulation number (loop)

					Output_statistics_dataframe <- rbind(Output_statistics_dataframe, N_fix_conv)														    # add rows main database



	### Free living bacterias -------

			cereals_tuber_oils <- 0.012 #                                                   # in tons/ha, Liu et al. 2010
			roots <- which(crop_info$CropGroup=="roots")
			cereal <- which(crop_info$CropGroup=="cereal")
			sec.cereal <- which(crop_info$CropGroup=="sec.cereal")
			oilcrops <- which(crop_info$CropGroup=="oilcrops")
                N_cyanobacteria <- array(dim=c(1,61))
				N_cyanobacteria[roots] <- N_cyanobacteria[cereal] <- N_cyanobacteria[sec.cereal] <- N_cyanobacteria[oilcrops]  <- cereals_tuber_oils
				N_cyanobacteria[50] <-  0.1                                                 #  sugarcane in tons/ha, Liu et al. 2010
				N_cyanobacteria[43] <-  0.02                                                #  rice in tons/ha, Liu et al. 2010
				N_cyanobacteria <- ifelse(!is.na(N_cyanobacteria), N_cyanobacteria, 0)



			foreach (i = 1:nlayers(conventional_rasters_area), .packages='raster') %dopar% {
		multipl <- conventional_rasters_area[[i]] * N_cyanobacteria[i]
		writeRaster(multipl, filename = paste0("Script_generated_files/N_fixation/conventional/Cyanobacteria/", crop_info$CropName[i], "_Cyanobacteria"), format="GTiff", overwrite=T )
		}

				dir_fix_conv_cyano <- c("Script_generated_files/N_fixation/conventional/Cyanobacteria/")
				files_fix_conv_cyano <- sort(list.files(path = dir_fix_conv_cyano, pattern = ".tif"))
				rasters_fixation_conventional_cyano <- stack(parLapply(cluster, paste0(dir_fix_conv_cyano, files_fix_conv_cyano), raster))

		total_fixation_conventional_cyano <- clusterR(rasters_fixation_conventional_cyano, calc, args=list(fun=sum.na), filename=paste0("Script_generated_files/", "total_fixation_conventional_cyano.tif"), format="GTiff", overwrite=T, cl=cluster) # Tons N/gridcell
		total_fixation_conventional_cyano <- raster("Script_generated_files/total_fixation_conventional_cyano.tif")


         N_fix_conv_cyano <- data.frame(cellStats(total_fixation_conventional_cyano, sum))  															# build temporary data.frame and add rows
         row.names(N_fix_conv_cyano) <- c("N_fix_conv_cyano") 																							# set rows names
         names(N_fix_conv_cyano) <- c(paste0("simulation.","1"))                                                                                        # for SENSITIVITY ANALYSIS change "1" with sumulation number (loop)

          Output_statistics_dataframe <- rbind(Output_statistics_dataframe, N_fix_conv_cyano)														    # add rows main database



	## --- Crop Residues return to soil  -----------------------


	crop_residues_N_input <- total_conventional_crop_N_demand_crop_residues * crop_residues_recycling_factor_conventional

	total_crop_residues_N_input_corrected_losses <- crop_residues_N_input * 0.99                                                                                # coefficient losses: 0.01 --> coefficient range: 0.003-0.03    # According to IPCC crop residues losses are only direct N2O emissions and leaching but NOT indirect NH3, NO emissions
    #total_crop_residues_N_input_corrected_losses <- total_crop_residues_N_input_corrected_losses * mask_correction_leaching                                    # 0.3: correction to estimate the 30% losses due to leaching
    total_crop_residues_N_input_corrected_losses <- total_crop_residues_N_input_corrected_losses - (crop_residues_N_input * (1-mask_correction_leaching))       # 0.3: correction to estimate the 30% losses due to leaching

				N_crop_res_to_soil <- data.frame(cellStats(total_crop_residues_N_input_corrected_losses, sum))                                # build temporary data.frame and add rows
				row.names(N_crop_res_to_soil) <- c("N_crop_res_to_soil")                                                                      # set rows names
				names(N_crop_res_to_soil) <- c(paste0("simulation.","1"))                                                                     # for SENSITIVITY ANALYSIS change "1" with sumulation number (loop)
					Output_statistics_dataframe <- rbind(Output_statistics_dataframe, N_crop_res_to_soil)  #add rows main database


	## --- Atmospheric depostions -------------------------------
	N_deposition <- raster("N atmospheric deposition/N_deposition_grid_cell.tif")
	cropland_share <- raster("/home/pbarbieri/global/cropland2000_area.tif")
	N_deposition <- N_deposition * cropland_share * mask_correction_leaching

				N_dep <- data.frame(cellStats(N_deposition, sum))                                                                            # build temporary data.frame and add rows
				row.names(N_dep) <- c("N_deposition")         																			     # set rows names
				names(N_dep) <- c(paste0("simulation.","1")) # for SENSITIVITY ANALYSIS change "1" with sumulation number (loop)
					Output_statistics_dataframe <- rbind(Output_statistics_dataframe, N_dep) 												 # add rows main database



	## --- Waste water production -------------------------------

	waste_water_nitrogen <- raster("/home/pbarbieri/global/waste water/Global_map_waste_water_available_Nitrogen_2015_estimate_redistributed_harvested_area.tif")
				rclmat <- matrix(c(2500, Inf, 2500), ncol=3, byrow=T)                                           # correction of errors in Waste water maps --> whaterver more than 2500 t gridcell give 2500
				waste_water_nitrogen <- reclassify(waste_water_nitrogen, rclmat)                                # correct for values above 1 --> if x>1, give 1

	total_N_waste_water_corrected_95crops <- waste_water_nitrogen * 0.96 * (1 - (0.0166 + 0.2))
                                                                                                    # ********* above coefficients description: 1) correction to account for the 5% of crops (in terms of cropland share) that we did not include in the 61 crops
                                                                                                    #                                           2) correction to estimate 1% losses due to direct N2O emissions and N2 (0.66 %)
                                                                                                    #											3) correction to estimate the 20% losses due to NH3 and NO volatilisation --> range coefficient: 0.05-0.5

			leaching_areas_mask <- raster("Climatic and irrigation data/mask_area_leaching_final_irrigation_above_005.tif")
			mask_correction_leaching <- leaching_areas_mask * 0.7                                   # 0.3: correction to estimate the 30% losses due to leaching
			mask_correction_leaching <- reclassify(mask_correction_leaching, cbind(0,1))

	total_N_waste_water_corrected_95crops <- total_N_waste_water_corrected_95crops - (waste_water_nitrogen * 0.96 * (1 - mask_correction_leaching))

				N_waste_water <- data.frame(cellStats(total_N_waste_water_corrected_95crops, sum))    # build temporary data.frame and add rows
				row.names(N_waste_water) <- c("N_waste_water")                                        # set rows names
				names(N_waste_water) <- c(paste0("simulation.","1"))                                  # for SENSITIVITY ANALYSIS change "1" with sumulation number (loop)
					Output_statistics_dataframe <- rbind(Output_statistics_dataframe, N_waste_water)  #add rows main database

		writeRaster(total_N_waste_water_corrected_95crops, filename="Script_generated_files/final_wastewater_corrected_losses.tif", format="GTiff", overwrite=T)
		final_waste_water_N <- total_N_waste_water_corrected_95crops



	##  ---- MANURE PRODUCTION ------------------------------------
		### Nitrogen -------------------------------
		dir_livestock_N <- c("/home/pbarbieri/global/N manure avail ipcc global medium boundary/")
		   files_livestock_N <- sort(list.files(path = dir_livestock_N, pattern = "correct_list_countries.tif"))
		   rasters_livestock_N <- stack(parLapply(cluster, paste0(dir_livestock_N, files_livestock_N), raster))

		total_N_manure <- clusterR(rasters_livestock_N, calc, args=list(fun=sum.na), filename=paste0("Script_generated_files/", "total_N_manure_correct_list_countries.tif"), format="GTiff", overwrite=T, cl=cluster) # Tons N/gridcell
		total_N_manure <- raster("Script_generated_files/total_N_manure_correct_list_countries.tif")
		rclmat <- matrix(c(5000, Inf, 5000), ncol=3, byrow=T)                           # correction of errors in Manure maps --> whaterver more than 5000 t gridcell give 5000
		total_N_manure <- reclassify(total_N_manure, rclmat)

		total_N_manure_corrected_95crops <- total_N_manure * 0.96 * (1 -  (0.0166 + 0.2))
                                                                                        # ********* above coefficients description: 1) correction to account for the 5% of crops (in terms of cropland share) that we did not include in the 61 crops
                                                                                        #                                           2) correction to estimate 1% losses due to direct N2O emissions and N2 (0.66%)
                                                                                        #											3) correction to estimate the 20% losses due to NH3 and NO volatilisation --> range coefficient: 0.05-0.5

		total_N_manure_corrected_95crops <- total_N_manure_corrected_95crops - (total_N_manure * 0.96 * (1 - mask_correction_leaching))



                        ### Compute manure statistics, update data.frame
						N_manure_lost_leaching <- 1-((cellStats((total_N_manure * 0.96 * mask_correction_leaching), sum)) / (cellStats((total_N_manure*0.96), sum)))                                    # calculate statistics
						N_manure_lost_total <- 1-((cellStats(total_N_manure_corrected_95crops, sum)) / (cellStats((total_N_manure*0.96), sum)))
						N_manure_stats <- data.frame(cellStats(stack((total_N_manure*0.96), (total_N_manure * 0.96 * mask_correction_leaching), total_N_manure_corrected_95crops), sum))                # build temporary data.frame and add rows
						N_manure_stats <- rbind(N_manure_stats, N_manure_lost_leaching)
						N_manure_stats <- rbind(N_manure_stats, N_manure_lost_total)
						row.names(N_manure_stats) <- c("total_N_common_manure", "common_N_manure_corrected_leaching", "common_N_manure_correted all","%_N_manure_leaching","%_N_manure_losses_total")   # set rows names
						names(N_manure_stats) <- c(paste0("simulation.","1"))                                                                                                                           # for SENSITIVITY ANALYSIS change "1" with sumulation number (loop)
				Output_statistics_dataframe <- rbind(Output_statistics_dataframe, N_manure_stats)                                                                                                       #add rows main database



# _______________________________________________________________________________________________________________
# ______________________  PART C: CONVENTIONAL BUDGETING, MANURE SURPLUS COMPUTATION ____________________________
# _______________________________________________________________________________________________________________


	## ----NITROGEN BUDGET ------------------------------------------------

	### computing first step conventional budget convetional considereing only chemical fertilisers, fixation and deposition as inputs
	budget_N <- ((mosaic((global_production_share_coefficient_conv * total_fixation_conventional),                                              #total_fixation_conventional_corrected_losses
	            (global_production_share_coefficient_conv * total_fixation_conventional_cyano),
				(global_production_share_coefficient_conv * N_deposition),
				(global_production_share_coefficient_conv * total_N_fert_corrected),
				(global_production_share_coefficient_conv * total_crop_residues_N_input_corrected_losses),  fun=sum.na))-(mosaic((global_production_share_coefficient_conv * conv_overfertil_allowed * total_conventional_crop_N_demand),
				(global_production_share_coefficient_conv * total_conventional_crop_N_demand_crop_residues), fun=sum.na)))                       # Also the total fertilization  has to be multiplied by "global_production_share_coefficient_conv", since here the value is total_P_fert=Area*Fertilizatrion_ratio --> The area is only the "global_production_share_coefficient_conv"% !

        manure_N_conventional <- global_production_share_coefficient_conv * total_N_manure_corrected_95crops   # load manure conventional



	### Run code to use as much manure to fill conventional budgets and calculate the surplus
	mask_1_N<-(budget_N<0)                                                      # This create a mask map containing 1 where the budget is negative
	manure_N_masked <- manure_N_conventional*mask_1_N                           # creates a manure map containing values only where the budget was negative
	budget_N_2 <- budget_N*mask_1_N                                             # creates a map containing budgets values only where budget is negative
	budget_N_3 <- budget_N_2 + manure_N_masked                                  # create new budgets by adding X% of manure production to cells where budget was < 0. This is not yet the final conventional budget since it contains also the surplus of manure and it does not contain the cells that were already positive in budget_N
	mask_2_N <- budget_N_3 > 0                                                  # select cells with positive values after manure add. The values represent the surplus of manure.
	surplus_N <- budget_N_3*mask_2_N                                            # maps the surplus of manure (positive cells)
	mask_3_N <- budget_N >= 0                                                   # This create a mask map containing 1 where the budget is positive or zero
	manure_N_masked_2 <- manure_N_conventional*mask_3_N                         # creates a map containing budgets values only where budget is positive or zero
	manure_N_surplus_total <- mosaic(manure_N_masked_2, surplus_N, fun=sum.na)  # creates a map containing the total surplus of manure

	manure_N_surplus_total <- extend(manure_N_surplus_total, extent(-180,180,-90,90))

    writeRaster(manure_N_surplus_total, filename = "Script_generated_files/manure_N_surplus_total_100_manure_cropland_correct_list_countries.tif",  format="GTiff", overwrite=T) #_80_conventional.tif

	budget_N_positive <- budget_N * (budget_N >= 0)      # create a map of the budget cells that were originally > 0
	budget_N_without_surplus <- budget_N_3 - surplus_N   # create a map of the budget cells that were originally < 0 by budgeting them to 0 i.e. eliminating the manure surplus
	budget_N_conventional <- mosaic(budget_N_positive, budget_N_without_surplus, fun=sum.na)  # merge the two complementary maps

	### Computing second step conventional budget (intriduction of Waste water in those cells that are still negative, compute waste water surplus) --------------------------------------
	mask_1_N<-(budget_N_conventional<0)                                          # This create a mask map containing 1 where the budget is negative
	wastewater_N_masked <- final_waste_water_N * mask_1_N                        # creates a wastewater_N map containing values only where the budget was negative
	budget_N_2 <- budget_N_conventional * mask_1_N                               # creates a map containing budgets values only where budget is negative
	budget_N_3 <- budget_N_2 + wastewater_N_masked                               # create new budgets by adding 70% of manure production to cells where byfget was < 0. This is not yet the final conventional budget since it contains also the surplus of manure and it does not contain the cells that were already positive in bundget_N
	mask_2_N <- budget_N_3 > 0                                                   # select cells with positive values after manure add. The values represent the surplus of manure.
	surplus_N_wastewater <- budget_N_3 * mask_2_N                                # maps the surplus of manure (positive cells)
	mask_3_N <- budget_N_conventional >= 0                                       # This create a mask map containing 1 where the budget is positive or zero
	wastewater_N_masked_2 <- final_waste_water_N * mask_3_N                      # creates a map containing budgets values only where budget is positive or zero
         origin(wastewater_N_masked_2) <- c(0,0)
         origin(surplus_N_wastewater) <- c(0,0)
   wastewater_N_surplus_total <- mosaic(wastewater_N_masked_2, surplus_N_wastewater, fun=sum.na)   # creates a map containing the total surplus of manure

	mask_N_positive <- budget_N_conventional >= 0
	budget_N_positive <- budget_N_conventional * mask_N_positive
	budget_N_without_surplus <- budget_N_3 - surplus_N_wastewater
	budget_N_conventional_final <- mosaic(budget_N_positive, budget_N_without_surplus, fun=sum.na)

		writeRaster(budget_N_conventional_final, filename = "Script_generated_files/budget_N_conventional_100_manure_cropland_correct_list_countries.tif",  format="GTiff", overwrite=T)       ### ATTENTION!! CHECK FILE NAME for SAVING!!!
		writeRaster(wastewater_N_surplus_total, filename = "Script_generated_files/wastewater_N_surplus_total_100_manure_cropland_correct_list_countries.tif",  format="GTiff", overwrite=T)   ### ATTENTION!! CHECK FILE NAME for SAVING!!!
			#budget_N_conventional_final <- raster("Script_generated_files/budget_N_conventional.tif")

	budget_N_conventional_corrected_overfertilisation <- budget_N_conventional + (global_production_share_coefficient_conv * (conv_overfertil_allowed-1) * total_conventional_crop_N_demand) # computes budgets at ther net of the allowed overfertilisation.
										# ATT! All in all, the overfertilisation serves to account the OVERUSE of N applied in comparison to crop demand. Therefore, to know if the REAL crop needs are satisfied, I need to eliminate from the budgets the
										# "artifical" part of the crop demand that accounts for this overfertilisation. Otherwide my budgets might be negative as referred to the augmented artificail demand of crops and not to their real demand.
		writeRaster(budget_N_conventional_corrected_overfertilisation, filename = "Script_generated_files/budget_N_conventional_corrected_overfertilisation_100_manure_correct_list_countries.tif",  format="GTiff", overwrite=T)
			#budget_N_conventional_corrected_overfertilisation <- raster("Script_generated_files/budget_N_conventional_corrected_overfertilisation_50_manure_cropland.tif")


				### Compute and update statistical dataframe ---------------------
					conv_manure_percent_surplus <- ((cellStats(manure_N_surplus_total, sum)) / (cellStats(manure_N_conventional, sum)))  #calculate statistics
                    wastewater_percent_surplus <- ((cellStats(wastewater_N_surplus_total, sum)) / (cellStats(final_waste_water_N, sum)))
					N_conv_budget_stats <- data.frame(cellStats(stack(budget_N, manure_N_surplus_total, budget_N_conventional, budget_N_conventional_final, budget_N_conventional_corrected_overfertilisation), sum)) #build temporary data.frame and add rows
					N_conv_budget_stats <- rbind(N_conv_budget_stats, conv_manure_percent_surplus)
					names(N_conv_budget_stats) <- c(paste0("simulation.","1"))                              # for SENSITIVITY ANALYSIS change "1" with sumulation number (loop)
                    N_conv_budget_stats <- rbind(N_conv_budget_stats,wastewater_percent_surplus)
                    row.names(N_conv_budget_stats) <- c("conv_N_budget_no_manure", "conv_N_manure_surplus", "N_budget_conventional_with_manure", "N_budget_conventional_with_wastewater", "budget_N_conventional_corrected_overfertilisation","conv_manure_percent_surplus","wastewater_percent_surplus") #set rows names

                    Output_statistics_dataframe <- rbind(Output_statistics_dataframe, N_conv_budget_stats)  #add rows main database


write.table(Output_statistics_dataframe, "Script_generated_files/Conventional_budgeting_statistics_100_manure_to_cropland_correct_list_countries.txt", sep=",")




				### Redistribute surplus maps at the national scale according to the total harvested area

                agriculture_area <- raster('/home/pbarbieri/global/total_harvested_area_organic_and_conventional.tif')
				manure_surplus_50 <-  raster("/home/pbarbieri/global/Version_I_first_inclusion_of_land_use_change_rules/Script_generated_files/manure_N_surplus_total_100_manure_cropland_correct_list_countries.tif")
				country_list <- read.csv("List_of_countres.csv", header=T)

				 dir <- c("Script_generated_files/rasters_N_manure_surplus_redistributed_agriculture_area/")
					  foreach (k = 1:length(country_list$country), .packages="raster") %dopar% {
						mask_country <- map_bruno==country_list$CNTRY_CODE[[k]]
						mask_country <- reclassify(mask_country, cbind(0, NA))
						cropped_area <- agriculture_area*mask_country
						total_area <- cellStats(cropped_area, sum)
						cropped_manure_surplus <- manure_surplus_50*mask_country
						total_surplus <- cellStats(cropped_manure_surplus, sum)
						N_per_ha <- total_surplus/total_area
					    N_per_ha <- ifelse(N_per_ha!=Inf, N_per_ha, 0)
						surplus_N_redistributed <- N_per_ha * cropped_area
						writeRaster(surplus_N_redistributed, filename=paste("Script_generated_files/rasters_N_manure_surplus_redistributed_agriculture_area/",countries_list$country[[k]],"_N_redistributed_manure_surplus",sep = ""), format="GTiff", overwrite=T)
				}
				  countries <- parLapply(cluster, paste0(dir, sort(list.files(path = dir, pattern = ".tif"))), raster)
				  global <- do.call(merge, countries)
  writeRaster(global, filename=paste0("Script_generated_files/N_redistributed_manure_surplus_100_correct_list_countries"), format="GTiff" , overwrite=T) ### ATTENTION!! CHECK FILE NAME for SAVING!!!

				#manure_surplus_50 <-  raster("/home/pbarbieri/global/Version_I_first_inclusion_of_land_use_change_rules/Script_generated_files/manure_N_surplus_total_100_manure_cropland.tif")


# **************************************************************************************************************
# *********************************  PART II: ORGANIC FARMING SUB-MODEL ****************************************
# **************************************************************************************************************



# _______________________________________________________________________________________________________________
# ______________________  PART D: LOAD/COMPUTE TOTAL AVAILABLE ORGANIC N AND P __________________________________
# _______________________________________________________________________________________________________________


	## --- Load Manure Organic and conventional surplus N and P -------------------------------------------
	manure_N_organic <- global_production_share_coefficient_org * total_N_manure_corrected_95crops   # Load manure organic, already corrected for losses
		manure_N_surplus_total <- raster("Script_generated_files/manure_N_surplus_total.tif")
	total_N_manure_organic <- manure_N_organic + K_conv_manure_surplus*manure_N_surplus_total   # ******* ATT! FOR THE MOMENT BASED ON CONVENTIONAL LIVESTOCK NUMBERS *******

	## --- Load Additional Fertilisers Surplus/Organic N and P -------------------------------------------
	wastewater_N_surplus_total <- raster("Script_generated_files/wastewater_N_surplus_total.tif")  # Already corrected for losses


	## --- Atmospheric depostions -------------------------------
	N_deposition <- raster("N atmospheric deposition/N_deposition_grid_cell.tif")  # TO BE CORRECTED FOR LOSSES TOO?
	N_deposition <- (total_conventional_crop_N_demand>0)*N_deposition
	N_deposition_organic <- global_production_share_coefficient_org * N_deposition   * mask_correction_leaching

	## --- Calculate total available N and P -------------------------------------------
	Total_organic_available_N <- total_N_manure_organic + K_waste_water * wastewater_N_surplus_total + N_deposition_organic
	Total_organic_available_N <- extend(Total_organic_available_N, e)

# ________________________________________________________________________________________________________________________
# ______________________  PART E: CALCULATE ORGANIC YIELD MAX (POTENTIAL) N and P DEMAND__________________________________
# ________________________________________________________________________________________________________________________


	## --- load CROP LAND USE SHARE in HECTARES ---------------------------------
	dir <- c("/home/pbarbieri/global/Land Use Change/100% conversion/Area_hectares/")
	files <- sort(list.files(path = dir, pattern = ".tif"))
	organic_rasters_land_use_hectars <- stack(lapply( paste0(dir, files), raster, xmn=-180, xmx=180,ymn=-56, ymx=90))
		writeRaster(organic_rasters_land_use_hectars, filename = "Script_generated_files/organic_rasters_land_use_hectars.tif",  format="GTiff", overwrite=T )
		organic_rasters_land_use_hectars <- stack("Script_generated_files/organic_rasters_land_use_hectars.tif")


	## --- load organic maximum yield, calculate DM and PRODUCTION  ------------------------ here: max yield = conventional yield * yield gap
	dir_yield <- c("Organic yield files/New/")
	files_yield <- sort(list.files(path = dir_yield, pattern = ".tif"))
	organic_rasters_yield <- stack(parLapply(cluster, paste0(dir_yield, files_yield), raster))

		foreach (i = 1:61, .packages='raster') %dopar% {   # Calculate yield in DM
			multipl <- organic_rasters_yield[[i]]*crop_DM[i]
			writeRaster(multipl, filename = paste0("Script_generated_files/rasters_organic_yield_DM/", crop_info$CropName[i]), format="GTiff", overwrite=T )
			}
	dir_yield_DM <- c("Script_generated_files/rasters_organic_yield_DM/")
	files_yield_DM <- sort(list.files(path = dir_yield_DM, pattern = ".tif"))
	organic_rasters_yield_DM <- stack(parLapply(cluster, paste0(dir_yield_DM, files_yield_DM), raster))


		foreach (i = 1:61, .packages='raster') %dopar% {    # Calculate production in DM
			multipl <- organic_rasters_land_use_hectars[[i]]*organic_rasters_yield_DM[[i]]
			writeRaster(multipl, filename = paste0("Script_generated_files/rasters_organic_production/", crop_info$CropName[i]), format="GTiff", overwrite=T )
			}
	dir_prod <- c("Script_generated_files/rasters_organic_production/")
	files_prod <- sort(list.files(path = dir_prod, pattern = ".tif"))
	organic_rasters_production <- stack(parLapply(cluster, paste0(dir_prod, files_prod), raster))

	###  --- Total Crop Residues production in DRY MATTER ------------------------------------
		foreach (i = 1:61, .packages='raster') %dopar% {
			multipl <- organic_rasters_production[[i]] * crop_info$calculated_RPR[[i]]
			writeRaster(multipl, filename = paste0("Script_generated_files/rasters_organic_production_crop_residues/", crop_info$CropName[i], "_crop_residues"), format="GTiff", overwrite=T )
			}
	dir_prod_residues <- c("Script_generated_files/rasters_organic_production_crop_residues/")
	files_prod_residues <- sort(list.files(path = dir_prod_residues, pattern = ".tif"))
	crop_residues_organic <- stack(parLapply(cluster, paste0(dir_prod_residues, files_prod_residues), raster))

	## --- N MAX POTENTIAL demand  ORGANIC----------------------------------------------------
	foreach (i = 1:nlayers(organic_rasters_production), .packages='raster') %dopar% {   # calculate main crop N content organic farming
		multipl <- organic_rasters_production[[i]]*crop_info$Final_N_harvested_part_demand_without_Ndfa[i]  # This N content coefficient automaticcaly accounts for N fixation when the crop is leguminous!!
		writeRaster(multipl, filename = paste0("Script_generated_files/organic_crop_N/organic_crop_N_including_fixation_no_PRoot/", crop_info$CropName[i], "_potential_demand_incl_fix"), format="GTiff", overwrite=T )
		}
			dir_org_N <- c("Script_generated_files/organic_crop_N/organic_crop_N_including_fixation_no_PRoot/")
			files_org_N <- sort(list.files(path = dir_org_N, pattern = ".tif"))
			organic_rasters_potential_N_demand <- stack(parLapply(cluster, paste0(dir_org_N, files_org_N), raster))

	total_organic_potential_crop_N_demand <- clusterR(organic_rasters_potential_N_demand, calc, args=list(fun=sum.na), filename=paste0("Script_generated_files/", "organic_rasters_potential_N_demand_incl_fixation.tif"), format="GTiff", overwrite=T, cl=cluster)
		total_organic_potential_crop_N_demand <- raster("Script_generated_files/organic_rasters_potential_N_demand_incl_fixation.tif") # calculate total main crop POTENTIAL N demand


	foreach (i = 1:nlayers(crop_residues_organic), .packages='raster') %dopar% {  # calculate crop residues N content organic farming
		multipl <- crop_residues_organic[[i]]*crop_info$N_crop_residues[i]
		writeRaster(multipl, filename = paste0("Script_generated_files/organic_N_crop_residues/", crop_info$CropName[i] , "_crop_residues_N"), format="GTiff", overwrite=T )
		}
			dir_org_N_residues <- c("Script_generated_files/organic_N_crop_residues/")
			files_org_N_residues <- sort(list.files(path = dir_org_N_residues, pattern = ".tif"))
			organic_rasters_N_demand_crop_residues <- stack(parLapply(cluster, paste0(dir_org_N_residues, files_org_N_residues), raster))
				writeRaster(organic_rasters_N_demand_crop_residues, filename = paste0("Script_generated_files/", "organic_rasters_N_demand_crop_residues.tif"), format="GTiff", overwrite=T )

	total_organic_crop_N_demand_crop_residues <- clusterR(organic_rasters_N_demand_crop_residues, calc, args=list(fun=sum.na), filename=paste0("Script_generated_files/
