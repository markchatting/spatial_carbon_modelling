########################################################################################

	#	TRAINING DATA FRAME CREATION	#
	
########################################################################################

#Define training parameters
trees <- 500
folds <- 10
n_preds <- 2 #'mtry' parameter. The number of predictors tried for each decision tree resample in the training process

#Training DF creation
setwd("~/UCD/model_training") # Directory I created where objects are saved to on disk. CHANGE THIS FILEPATH TO MATCH WHERE YOU WANT TO SAVE OBJECTSON YOUR COMPUTER
pred_vars <- readRDS("pred_vars.RData")

df <- pred_vars
df <- na.omit(df)
#df <- df[df$coast_dist>5, ]
response <- df$response
train_df <- df

#Create Spatial Folds
#The CAST package creates spatial folds ('spacevar = space') based on a factor that the user creates (the factor I created was 'train_df$space').
#To create the 'train_df$space' factor, I divided the training data spatial extent into a grid. The points in each grid square consisted one group.
#The groups were then randomly divided into 10 training folds.
 
#Create spatial grid
data.df <- train_df[, 1:2]
cell_size_x <- (max(data.df$lon) - min(data.df$lon))
cell_size_y <- (max(data.df$lat) - min(data.df$lat))
cell_width <- mean(cell_size_y, cell_size_y)/sqrt(folds)
spatial_group <- function(xy, origin=c(0,0), cellsize=c(cell_width, cell_width)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}
spat_group <- spatial_group(cbind(data.df$lon, data.df$lat), cell_width)

#Group points by spatial grid
train_df$space <- as.factor(paste(spat_group[, 1], spat_group[, 2], sep=""))
indices <- CreateSpacetimeFolds(train_df,spacevar = "space", k=folds)
#train_df <- train_df[, c(1:16, 19)]
str(train_df)

#Vizualise spatial folds
C <- train_df
coordinates(C) <- c("lon", "lat")
B <- SpatialPoints(spat_group)
D <- SpatialPoints(C)

setwd("~/UCD/model_training")# Directory I created where objects are saved to on disk. CHANGE THIS FILEPATH TO MATCH WHERE YOU WANT TO SAVE OBJECTSON YOUR COMPUTER
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

#Calculate Euclidean distances to possibly be included as a predictor in RF. IN THE END I HAVEN'T USED EUCLIDEAN DISTANCES AS A PREDICTOR
b <- c(rep(0, times = nrow(train_df)))
for(i in 1:nrow(train_df)){
	dists <- distGeo(train_df[i, 1:2], train_df[, 1:2])
	b[i] <- sum(dists)/1000
}
train_df$euclidean_dist <- b
str(train_df)

#Calculate Eastings and Northings from lon and lat to potentially use as predictors in RF
#Create function to convert lon and lat to UTM
LongLatToUTM<-function(x,y,zone){
 xy <- data.frame(ID = 1:length(x), X = x, Y = y)
 coordinates(xy) <- c("X", "Y")
 proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
 res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
 return(as.data.frame(res))
}

#Create function to find UTM zone from the longitude
UTMZone_find <- function(x){
	a <- round(x, 0) + 180
	return(round(a/6, 0))
	}
UTMZone <- UTMZone_find(train_df$lon)

#Loop through all train_df lon and lat's and convert each to UTM using previoulst create LongLatToUTM() function.
output <- data.frame(1:nrow(train_df), 1:nrow(train_df),  1:nrow(train_df))
for(i in 1:nrow(train_df)){
	output[i, ] <- LongLatToUTM(train_df$lon[i], train_df$lat[i], UTMZone[i])	
}
names(output) <- c("ID", "eastings", "northings")
output <- output[, 2:3]
str(output)

#Append newly created eastings and northings objects to train_df
train_df$easting <- output$eastings
train_df$northing <- output$northings
train_pts <- train_df[, 1:2] # create separate training coordinates dataframe to be saved.
train_df <-train_df[, 3:20] # save the rest of train_df as the dataframe to train the RF model.
str(train_df)
	  
########################################################################################

			#	CARBON DENSITY MODEL TRAINING	#

########################################################################################

#Forward feature selection model and use spatial cross validation to asses model performance
train_df_i <- train_df[, c(2:14, 17:18)]
rf_mod_ffs <- ffs(train_df_i, response, metric="RMSE", # Meyer's papers use RMSE instead of RSquared as the performance metric of choice
				method="rf", tuneGrid=data.frame("mtry"=2),
				importance=TRUE, ntree=trees,
				trControl=trainControl(method="cv", index=indices$index))

rf_mod_ffs # Model summary
plot(varImp(rf_mod_ffs)) #Plot model predictor importance.
rf_mod_ffs$selectedvars #View final selected predictors based on Forward Feature Selection (FFS).
plot_ffs(rf_mod_ffs) #View model performance with various numbers of predictors used.

setwd("~/UCD/model_training")# Directory I created where objects are saved to on disk. CHANGE THIS FILEPATH TO MATCH WHERE YOU WANT TO SAVE OBJECTSON YOUR COMPUTER
saveRDS(rf_mod_ffs, "rf_mod_ffs.RData")

########################################################################################

		#		PARTIAL PLOTS		#
		
########################################################################################

library("gridExtra")

setwd("~/UCD/model_training")# Directory I created where objects are saved to on disk. CHANGE THIS FILEPATH TO MATCH WHERE YOU WANT TO SAVE OBJECTSON YOUR COMPUTER
rf_mod_ffs <- readRDS("rf_mod_ffs.RData")
selected_model <- rf_mod_ffs

#Dynamically create partial plots based on whatever predictors FFS chose. This will automatically update with new predictors everytime the model is re-trained.
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

########################################################################################

		#		END!!!		#
		
########################################################################################
