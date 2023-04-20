########################################################################################

#	Predictor Variables

########################################################################################

#Create coordinates object to extract predictor data from netcdfs/geotiffs/etc from the same locations as sampled reponse data.
setwd("~/UCD/model_training") # Directory I created where objects are saved to on disk. CHANGE THIS FILEPATH TO MATCH WHERE YOU WANT TO SAVE OBJECTSON YOUR COMPUTER
train_df <- readRDS("response.RData")
pts <- train_df[, 1:2]


#Distance to land
dist_test <- ggOceanMaps::dist2land(pts) %>% mutate(where = ifelse(ldist == 0, "land", ifelse(ldist < 100*1.852, "coast", "sea")))
coast_dist <- dist_test$ldist

#EMODnet Bathymetry
raster_file <- "~/UCD/datasets/bathymetry/EMODbathymetry.tif"
geo_pts <- pts
coordinates(geo_pts) <- ~ lon+ lat
ras <- raster(raster_file)
bath_EMOD <- extract(ras, geo_pts)


#Bottom water temperature, Bottom water salinity, Surface primary productivity,
#mean current velocity, max current velocity from bio-oracle
layer_names <- c("BO22_salinitymean_bdmean", "BO22_tempmean_bdmean", "BO22_curvelmax_bdmean", "BO22_curvelmean_bdmean", "BO22_ppmean_bdmean")
data_extract <- list()
for(i in 1:length(layer_names)){
	data_layer <- load_layers(layer_names[[i]])
	data_extract[[i]] <- extract(data_layer, pts)
	print(i)
}
names(data_extract) <- c("salinity", "temperature", "max_current", "mean_current", "productivity")
str(data_extract)


#Total suspended matter
nc_files <- list.files("~/UCD/datasets/SPM", full.names=T)
out_mat <- matrix(0, nrow=nrow(pts), ncol=length(nc_files))
for(i in 1:length(nc_files)){
	nc_file_open <- nc_open(nc_files[i])
	nc_lat <- ncvar_get(nc_file_open, "lat")
	nc_lon <- ncvar_get(nc_file_open, "lon")
	nc_var <- ncvar_get(nc_file_open, "spm")
	nc_close(nc_file_open)
	nc_var <- na.approx(nc_var[, ])
	spm_mat <- list(x=nc_lon, y=nc_lat, z=nc_var)
	spm <- interp.surface(spm_mat, pts)
	out_mat[, i] <- spm
}
spm_mean <- rowMeans(out_mat, na.rm = T)

#Orbital wave velocity at the seabed
nc_file <- "~/UCD/datasets/Wilson_2018/sediment_properties.nc"
nc_file_open <- nc_open(nc_file)
nc_lat <- ncvar_get(nc_file_open, "Latitude")
nc_lon <- ncvar_get(nc_file_open, "Longitude")
vars <- c("OrbitalVelMean", "OrbitalVelMax", "MudPercent", "GravelPercent", "SandPercent")
out_vars <- list()
for (i in 1:length(vars)){
	nc_var <- ncvar_get(nc_file_open, vars[i])
	nc_var<-na.approx(nc_var)
	out_mat <- list(x=nc_lon, y=nc_lat, z=nc_var)
	out_vars[[i]] <- interp.surface(out_mat, pts)
}
nc_close(nc_file_open)
names(out_vars) <- vars
str(out_vars)


#Combine all predictor variables into one dataframe
pred_vars <- data.frame(train_df$lon, train_df$lat, train_df$carbon, coast_dist, dist_test$where, bath, data_extract[[1]], data_extract[[2]], data_extract[[3]], data_extract[[4]], data_extract[[5]], spm_mean, out_vars[[1]], out_vars[[2]], out_vars[[3]], out_vars[[4]],	
					out_vars[[5]])
pred_vars <- pred_vars[pred_vars$dist_test.where != "land", ]
names(pred_vars) <- c("lon", "lat", "response", "coast_dist", "where", "bathymetry", "salinity", "temperature", "max_current", "mean_current","productivity", "spm", "wave_mean", "wave_max", "mud_percent", "gravel_percent", "sand_percent")
pred_vars <- pred_vars[, c(1:4, 6:17)]
str(pred_vars)

#Save the resulting dataframe to disk
setwd("~/UCD/model_training")# Directory I created where objects are saved to on disk. CHANGE THIS FILEPATH TO MATCH WHERE YOU WANT TO SAVE OBJECTSON YOUR COMPUTER
saveRDS(pred_vars, "pred_vars.RData")

########################################################################################

		#	END!!!		#

########################################################################################
