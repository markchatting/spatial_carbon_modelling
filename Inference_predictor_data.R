########################################################################################

		#		EXTRACT PREDICTOR DATA FOR INFERENCE AREA		#
		
########################################################################################

#Create x and y spatial grid from inference_shp shapefile. 
outer_area <- read_sf("~/UCD/model_training/inference_shp/inference_shp.shp")
inner_area <- read_sf("~/QU/GIS shapefiles/GB_I_Isles/GB_I_Isles.shp")
inference_area <- st_difference(outer_area, inner_area)

inference_grid <- inference_area %>% 
  st_make_grid(cellsize = 0.0125, what = "centers") %>% # grid of points
  st_intersection(inference_area)     

#Visualize inference point grid
ggplot() + geom_sf(data = inference_area) + geom_sf(data = inference_grid)

#Convert inference sf point grid to simple data frame (pts) to work with
pts <- (data.frame(st_coordinates(inference_grid)))
names(pts) <- c("lon", "lat")
str(pts)

#Distance to land
dist_test <- ggOceanMaps::dist2land(pts) %>% mutate(where = ifelse(ldist == 0, "land", ifelse(ldist < 100*1.852, "coast", "sea")))
coast_dist <- dist_test$ldist

#Bathymetry
raster_file <- "~/UCD/datasets/bathymetry/EMODbathymetry.tif"
geo_pts <- pts
coordinates(geo_pts) <- ~ lon+ lat
ras <- raster(raster_file)
bath <- extract(ras, geo_pts)


#Bottom water temperature, Bottom water salinity, Surface primary productivity,
#mean current velocity, max current velocity
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
	print(i)
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
	print(i)
}
nc_close(nc_file_open)
names(out_vars) <- vars
str(out_vars)


#Calculate Euclidean distances
output <- c(rep(0, times = nrow(infer_df)))
for(i in 1:nrow(infer_df)){
	dists <- distGeo(infer_df[i, 1:2], infer_df[, 1:2])
	output[i] <- sum(dists)/1000
}

setwd("~/UCD/model_training")
saveRDS(output, "EUCLIDEAN_DISTS_INFER.RData")
infer_df$euclidean_dist <- output

#Started 1st April 0812 hrs. Time to finish = 4.88 days.

#Calculate Eastings and Northings to potentially use as predictors in RF
UTMZone <- UTMZone_find(pts$lon)
output <- data.frame(1:nrow(pts), 1:nrow(pts),  1:nrow(pts))
for(i in 1:nrow(pts)){
	output[i, ] <- LongLatToUTM(pts$lon[i], pts$lat[i], UTMZone[i])	
}
names(output) <- c("ID", "eastings", "northings")
output <- output[, 2:3]
str(output)

#Combine all predictor variables
pred_vars <- data.frame(pts$lon, pts$lat, coast_dist, dist_test$where, bath, data_extract[[1]], data_extract[[2]], data_extract[[3]], data_extract[[4]], data_extract[[5]], spm_mean, out_vars[[1]], out_vars[[2]], out_vars[[3]], out_vars[[4]], out_vars[[5]], output$eastings, output$northings)					
pred_vars <- pred_vars[pred_vars$dist_test.where != "land", ]
names(pred_vars) <- c("lon", "lat", "coast_dist", "where", "bathymetry", "salinity", "temperature", "max_current", "mean_current","productivity", "spm", "wave_mean", "wave_max", "mud_percent", "gravel_percent", "sand_percent", "easting", "northing")
infer_vars <- na.omit(pred_vars)
infer_pts <- infer_vars[, 1:2]
infer_vars <- infer_vars[, c(3, 5:18)]
str(infer_vars)

setwd("~/UCD/model_training")
saveRDS(infer_vars, "inference_vars.RData")
saveRDS(infer_pts, "infer_pts.RData")
