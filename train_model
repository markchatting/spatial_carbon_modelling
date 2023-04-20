########################################################################################

	#	TRAINING DATA FRAME CREATION	#
	
########################################################################################

#Training parameters
trees <- 500
folds <- 10
n_preds <- 2

#Training DF creation
setwd("~/UCD/model_training")
pred_vars <- readRDS("pred_vars.RData")

df <- pred_vars
df <- na.omit(df)
#df <- df[df$coast_dist>5, ]
response <- df$response
train_df <- df


#Create Spatial Folds
#The spatial folds I created was spatially irregular since the points were distributed irregularly. I've seen techniques where they create regular blocks when the training data is regularly spaced (Valavi et al. 2018). But in the CAST package Meyer recomends when when polygon data are spaced irregularly to leave out whole polygons from training folds. First I rounded coordinates to the nearest whole numbers then pasted them together and split the resulting vector into 10 partitions. The 10 partitions were then used as training folds. I chose 10 partitions because I used 10 training folds. 10 or 20 training folds seems standard as well.
 
data.df <- train_df[, 1:2]
cell_size_x <- (max(data.df$lon) - min(data.df$lon))
cell_size_y <- (max(data.df$lat) - min(data.df$lat))
cell_width <- mean(cell_size_y, cell_size_y)/sqrt(folds)
spatial_group <- function(xy, origin=c(0,0), cellsize=c(cell_width, cell_width)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}
spat_group <- spatial_group(cbind(data.df$lon, data.df$lat), cell_width)
train_df$space <- as.factor(paste(spat_group[, 1], spat_group[, 2], sep=""))
indices <- CreateSpacetimeFolds(train_df,spacevar = "space", k=folds)
#train_df <- train_df[, c(1:16, 19)]
str(train_df)

#Vizualise spatial folds
C <- train_df
coordinates(C) <- c("lon", "lat")
B <- SpatialPoints(spat_group)
D <- SpatialPoints(C)

setwd("~/UCD/model_training")
tiff("Spatial_folds.tiff", units="cm", width=35, height=20, res=150)
par(mfrow = c(2, 5))
par(mar = c(0, 1, 0, 1))
for(i in 1:length(indices$index)){
	#plot(PP)
	plot(B, cex = 15.5, pch = 0)
	plot(C, pch=20, col = "red", add = T)
	plot(C[indices$index[[i]][1:length(indices$index[[i]])], ], pch = 20, col = "black", add = T)
	plot(GB_and_I, add = T)
	print(i)
}
dev.off()


#Calculate Euclidean distances to possibly be included as a predictor in RF
b <- c(rep(0, times = nrow(train_df)))
for(i in 1:nrow(train_df)){
	dists <- distGeo(train_df[i, 1:2], train_df[, 1:2])
	b[i] <- sum(dists)/1000
}
train_df$euclidean_dist <- b
str(train_df)

#Calculate Eastings and Northings to potentially use as predictors in RF
#Creat function
LongLatToUTM<-function(x,y,zone){
 xy <- data.frame(ID = 1:length(x), X = x, Y = y)
 coordinates(xy) <- c("X", "Y")
 proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
 res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
 return(as.data.frame(res))
}

UTMZone_find <- function(x){
	a <- round(x, 0) + 180
	return(round(a/6, 0))
	}
UTMZone <- UTMZone_find(train_df$lon)

output <- data.frame(1:nrow(train_df), 1:nrow(train_df),  1:nrow(train_df))
for(i in 1:nrow(train_df)){
	output[i, ] <- LongLatToUTM(train_df$lon[i], train_df$lat[i], UTMZone[i])	
}
names(output) <- c("ID", "eastings", "northings")
output <- output[, 2:3]
str(output)

train_df$easting <- output$eastings
train_df$northing <- output$northings
train_pts <- train_df[, 1:2]
train_df <-train_df[, 3:20]
str(train_df)

########################################################################################

			#	CARBON DENSITY MODEL TRAINING	#

########################################################################################

#Forward feature selection and spatial cross validation without Euclidean Distances
train_df_i <- train_df[, c(2:14, 17:18)]
rf_mod_ffs <- ffs(train_df_i, response, metric="RMSE",
				method="rf", tuneGrid=data.frame("mtry"=2),
				importance=TRUE, ntree=trees,
				trControl=trainControl(method="cv", index=indices$index))

rf_mod_ffs
plot(varImp(rf_mod_ffs))
rf_mod_ffs$selectedvars
plot_ffs(rf_mod_ffs)


setwd("~/UCD/model_training")
saveRDS(rf_mod_ffs, "rf_mod_ffs.RData")


########################################################################################

		#		PARTIAL PLOTS		#
		
########################################################################################

library("gridExtra")

rf_mod_ffs <- readRDS("rf_mod_ffs.RData")
selected_model <- rf_mod_ffs
selected_model
predictors <- selected_model$finalModel$xNames
nRow <- floor(sqrt(length(predictors)))

pdplot <- list()
for(i in 1:length(predictors)){
	pd <- partial(selected_model, pred.var=predictors[i])
	pdplot[[i]] <- plotPartial(pd, ylim=c(0, mean(train_df$response)+(1.25*sd(train_df$response))),
	ylab = 'Carbon density %',
	xlab = predictors[i])
	print(i)
}

setwd("~/UCD/model_training")
jpeg("carbon_density_partial_plots.jpeg", units = "cm", width = 25, height = 16, res = 200)
do.call("grid.arrange", c(pdplot, nrow=nRow))
dev.off()
